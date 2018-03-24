## ---- 3.configuration

m <- 9            # number of RA retries
Ps <- 0.03        # sleep power consumption [mW]
Pi <- 10          # idle power consumption [mW]
Prx <- 100        # rx power consumption [mW]
Prbp <- 32.18     # tx power consumption per RB pair [mW]
Ppre <- 32.18     # preamble tx power consumption [mW]

Tbpre <- 0.0025   # time before preamble transmission [s]
Tpre <- 0.0014    # time for preamble transmission [s]
Trarx <- 0.01     # rx time to perform RA [s]
Tcrcp <- 0.016    # sum of processing delays to establish a connection [s]
Twait <- 0.054    # S1 processing and transfer delay [s]

Brbp <- 36        # bytes per RB pair (QPSK modulation)
Breq <- 7         # RRC request message size (bytes)
Bcompcp <- 129    # RRC setup complete + NAS control plane SR + data for CP (bytes)

Tra <- Tbpre + Trarx + Tpre
Pra <- (Tbpre*Pi + Trarx*Prx + Tpre*Ppre) / Tra
rbs <- (ceiling(Breq/Brbp) + ceiling(Bcompcp/Brbp)) * 1e-3
Ttx <- Tcrcp + rbs + Twait
Ptx <- (Tcrcp*Prx + rbs*Prbp + Twait*Prx) / Ttx

tx_period <- 3600 # time between transmissions (seconds)
Tsim <- 24*3600   # simulation time (seconds)

## ---- 3.simulation

library(simmer)

set.seed(1234)

# parametrized simulation function
# - param[["meter_n"]] is the number of IoT devices in the cell
# - param[["backoff"]] is the backoff time
simulate <- function(param) {

  # identifiers for RA preambles
  preambles <- paste0("preamble_", 1:54)

  # IoT device logic
  meter <- trajectory() %>%
    trap("reading") %>%
    # sleep
    set_attribute("P", 0) %>%
    wait() %>%
    timeout(function() round(runif(1, 0, param[["backoff"]]), 3)) %>%
    # ra start
    simmer::select(preambles, policy="random") %>%
    seize_selected(
      continue=c(TRUE, TRUE),
      # ra & tx
      post.seize=trajectory() %>%
        set_attribute("P", Pra) %>%
        timeout(Tra) %>%
        release_selected() %>%
        set_attribute("P", Ptx) %>%
        timeout(Ttx),
      # ra & backoff & retry
      reject=trajectory() %>%
        set_attribute("P", Pra) %>%
        timeout(Tra) %>%
        set_attribute("P", Pi) %>%
        timeout(function() sample(1:20, 1) * 1e-3) %>%
        rollback(6, times=m)
    ) %>%
    rollback(5, times=Inf)

  # trigger a reading for all the meters every tx_period
  trigger <- trajectory() %>%
    timeout(tx_period) %>%
    send("reading") %>%
    rollback(2, times=Inf)

  # simulation environment
  env <- simmer() %>%
    # IoT device workers
    add_generator("meter_", meter, at(rep(0, param[["meter_n"]])), mon=2) %>%
    # trigger worker
    add_generator("trigger_", trigger, at(0), mon=0)

  for (i in preambles)
    # one resource per preamble
    env %>% add_resource(i, 1, 0, mon=FALSE)

  env %>%
    run(until=Tsim) %>%
    wrap()
}

# grid of scenarios
cases <- expand.grid(meter_n = c(5e3, 1e4, 3e4), backoff = c(5, 10, 30, 60))

# parallel simulation
system.time({
  envs <- parallel::mclapply(split(cases, 1:nrow(cases)), simulate,
                             mc.cores=nrow(cases), mc.set.seed=FALSE)
})

## ---- 3.analysis

library(tidyverse)

bp.vals <- function(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  r <- quantile(x, probs=probs, na.rm=TRUE)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

envs %>%
  get_mon_attributes() %>%
  group_by(replication, name) %>%
  summarise(dE = sum(c(0, head(value, -1) * diff(time)))) %>%
  left_join(rowid_to_column(cases, "replication")) %>%
  # plot
  ggplot(aes(factor(meter_n), dE*tx_period/Tsim, color=factor(backoff))) +
  stat_summary(fun.data=bp.vals, geom="boxplot", position="dodge") +
  theme_bw() + theme(legend.justification=c(0, 1), legend.position=c(.02, .98)) +
  labs(y="Energy per Tx [mJ]", x="No. of devices", color="Backoff window [s]")
