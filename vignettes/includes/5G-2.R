## ---- 2.configuration

C <- 1.25e9                           # link capacity [bps]
Tg <- 1e-6                            # guard time [s]

bytes <- c(40, 576, 1500)             # traffic distribution
Weights <- c(7, 4, 1) / 12            #
EX <- sum(Weights * (8 * bytes / C))  # avg. interarrival time [s]
burst <- 20                           # burst length [pkts]

ONU_lambda <- 20e6 / C / EX           # avg. rate per ONU [pkts/s]
SC_lambda <- 150e6 / C / EX           # avg. rate for the SC [pkts/s]
RRH_on <- 8 * 6000 / C                # RRH on period [s]
RRH_period <- RRH_on * C / 720e6      # RRH total period [s]
RRH_off <- RRH_period - RRH_on        # RRH off period [s]

## ---- 2.helpers

# helper function: round-robin-based selection of ONUs
cyclic_counter <- function(n) {
  n <- as.numeric(n)
  i <- 1
  function(op) {
    if (!missing(op)) {
      i <<- i + op
      if (i > n) i <<- 1
      else if (i < 1) i <<- n
    }
    i
  }
}

# helper function: generator of packet sizes of n packets
gen_pkts <- function(n) sample(bytes*8/C, size=n, prob=Weights, replace=TRUE)

# helper function: double-exponential generator of interarrival times
gen_exp <- function(lambda, burst) function()
  unlist(lapply(rexp(100/burst, lambda/burst), function(i) c(i, rep(0, rpois(1, burst)))))

## ---- 2.simulation

library(simmer)

set.seed(1234)

# parametrized simulation function
# - param[["scenario"]] is the scenario identifier (A=small cell, B=RRH)
# - param[["ONU_n"]] is the number of ONUs
# - param[["limit"]] is the max. transmission window
simulate <- function(param) {

  # global variables
  lambda <- rep(ONU_lambda, param[["ONU_n"]])
  limit <- rep(param[["limit"]], param[["ONU_n"]]) * 8 / C
  if (param[["scenario"]] == "A") {
    param[["ONU_n"]] <- param[["ONU_n"]] + 1
    lambda <- c(lambda, SC_lambda)
    limit <- c(limit, Inf)
    tx_t <- RRH_on + Tg + Tg * 0:param[["ONU_n"]]
  } else {
    Tg_n <- RRH_off %/% Tg - 1
    Tg_i <- rep(1:Tg_n, length.out=param[["ONU_n"]]+1)
    period_i <- rep(0:param[["ONU_n"]], each=Tg_n, length.out=param[["ONU_n"]]+1)
    tx_t <- RRH_on + RRH_period * period_i + Tg * Tg_i
  }
  eq_pkts <- rep(list(NULL), param[["ONU_n"]])
  tx_pkts <- rep(list(NULL), param[["ONU_n"]])
  t_head <- tail(tx_t, 1)
  onu_i <- cyclic_counter(param[["ONU_n"]])
  remaining <- Inf

  # DBA logic
  set_next_window <- function() {
    # time until the next RRH_on
    if (param[["scenario"]] == "B")
      remaining <- (t_head %/% RRH_period + 1) * RRH_period - t_head
    # generate new pkt lengths
    pkts <- get_queue_count(env, paste0("ONU", onu_i()))
    eq_pkts[[onu_i()]] <<- c(eq_pkts[[onu_i()]], gen_pkts(pkts - length(eq_pkts[[onu_i()]])))
    # reserve the next transmission window
    eq_cumsum <- cumsum(eq_pkts[[onu_i()]])
    n <- sum(eq_cumsum <= min(limit[[onu_i()]], remaining - Tg))
    if ((pkts && !n) || (remaining <= Tg)) {
      # go past the next RRH_on
      t_head <<- t_head + remaining + RRH_on + Tg
      n <- sum(eq_cumsum <= min(limit[[onu_i()]], RRH_off - 2*Tg))
    }
    tx_pkts[[onu_i()]] <<- head(eq_pkts[[onu_i()]], n)
    eq_pkts[[onu_i()]] <<- tail(eq_pkts[[onu_i()]], -n)
    tx_t[[onu_i()]] <<- t_head
    t_head <<- t_head + sum(tx_pkts[[onu_i()]]) + Tg
    index <<- 0
    tx_t[[onu_i(1)]] - now(env)
  }

  # list of ONU_n trajectories, one per ONU
  ONUs <- lapply(1:param[["ONU_n"]], function(i) {
    trajectory() %>%
      seize(paste0("ONU", i)) %>%
      set_capacity(paste0("ONU", i), 0) %>%
      seize("link") %>%
      timeout(function() {
        index <<- index + 1
        tx_pkts[[i]][index]
      }) %>%
      release("link") %>%
      release(paste0("ONU", i))
  })

  # OLT logic
  OLT <- trajectory() %>%
    simmer::select(function() paste0("ONU", onu_i())) %>%
    set_attribute("ONU", function() onu_i()) %>%
    set_capacity_selected(function() length(tx_pkts[[onu_i()]])) %>%
    timeout(function() sum(tx_pkts[[onu_i()]])) %>%
    timeout(set_next_window) %>%
    rollback(amount=6, times=Inf)

  # RRH logic
  RRH <- trajectory() %>%
    seize("link") %>%
    timeout(RRH_on) %>%
    release("link") %>%
    timeout(RRH_off) %>%
    rollback(amount=4, times=Inf)

  # simulation environment
  env <- simmer() %>%
    # the fiber link as a resource
    add_resource("link", 1, Inf)

  if (param[["scenario"]] == "B")
    # RRH worker
    env %>% add_generator("RRH_", RRH, at(0))

  for (i in 1:param[["ONU_n"]])
    env %>%
      # ONU_n resources, one per ONU
      add_resource(paste0("ONU", i), 0, Inf) %>%
      # ONU_n traffic generators, one per ONU
      add_generator(paste0("ONU_", i, "_"), ONUs[[i]], gen_exp(lambda[i], burst))

  env %>%
    # OLT worker
    add_generator("token_", OLT, at(RRH_on + Tg), mon=2) %>%
    run(until=1e5/sum(lambda)) %>%
    wrap()
}

# grid of scenarios
cases <- data.frame(scenario = c(rep("A", 4), rep("B", 1)),
                    ONU_n = c(rep(31, 4), rep(7, 1)),
                    limit = c(Inf, 1500, 3000, 6000, Inf))

# parallel simulation
system.time({
  envs <- parallel::mclapply(split(cases, 1:nrow(cases)), simulate,
                             mc.cores=nrow(cases), mc.set.seed=FALSE)
})

## ---- 2.analysis

library(tidyverse)

bp.vals <- function(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  r <- quantile(x, probs=probs, na.rm=TRUE)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

envs %>%
  get_mon_arrivals() %>%
  left_join(rowid_to_column(cases, "replication")) %>%
  mutate(scenario = forcats::fct_recode(
    scenario, `SmallCell + 31 ONUs`="A", `RRH + 7 ONUs`="B")) %>%
  separate(name, c("flow", "index", "n")) %>%
  mutate(flow = if_else(index == ONU_n+1, "SmallCell", flow)) %>%
  mutate(total_time = end_time - start_time,
         waiting_time = total_time - activity_time) %>%
  # plot
  ggplot(aes(forcats::fct_rev(flow), waiting_time*1e6, color=factor(limit))) + theme_bw() +
  facet_grid(~scenario, scales="free", space="free") +
  stat_summary(fun.data=bp.vals, geom="boxplot", position="dodge") +
  theme(legend.justification=c(0, 1), legend.position=c(.02, .98)) +
  labs(y=expression(paste("Queueing Delay [", mu, "s]")), x=element_blank(), color="Limit [bytes]")
