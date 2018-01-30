## ---- 1.configuration

C <- 40e9                                   # link capacity [bps]
rho <- 0.75                                 # utilization

FH_EX <- (80 + 46) * 8 / C                  # avg. FH interarrival time [s]
BH_bytes <- c(40, 576, 1500)                # BH traffic distribution
Weights <- c(7, 4, 1) / 12                  #
BH_EX <- sum(Weights * (8 * BH_bytes / C))  # avg. BH interarrival time [s]

FH_w <- 0.5                                 # FH traffic ratio
BH_w <- 1 - FH_w                            # BH traffic ratio
FH_lambda <- FH_w * rho / FH_EX             # FH avg. rate [pkts/s]
BH_lambda <- BH_w * rho / BH_EX             # BH avg. rate [pkts/s]

Tsim <- 1e5 / (FH_lambda + BH_lambda)       # simulation time

## ---- 1.simulation

library(simmer)

set.seed(1234)

# parametrized simulation function
# - param[["n_switch"]] is the number of switches
# - param[["fh_prio"]] is the priority level for FH traffic
# - param[["preemptive"]] is the preemptiveness flag for switches
simulate <- function(param) {

  # single FH traffic trajectory traversing n_switch elements sequentially
  fh_traffic <- lapply(1:param[["n_switch"]], function(i) {
    trajectory() %>%
      seize(paste0("switch", i)) %>%
      timeout(FH_EX) %>%
      release(paste0("switch", i))
  }) %>% join()

  # list of n_switch trajectories, one per element, modeling the interfering BH traffic
  bh_traffic <- lapply(1:param[["n_switch"]], function(i) {
    trajectory() %>%
      seize(paste0("switch", i)) %>%
      timeout(function() sample(BH_bytes*8/C, size=1, prob=Weights)) %>%
      release(paste0("switch", i))
  })

  # simulation environment
  env <- simmer() %>%
    # generator of FH traffic
    add_generator("FH_0_", fh_traffic, function() rexp(100, FH_lambda), priority=param[["fh_prio"]])

  for (i in 1:param[["n_switch"]])
    env %>%
      # n_switch resources, one per switch
      add_resource(paste0("switch", i), 1, Inf, mon=FALSE, preemptive=param[["preemptive"]]) %>%
      # n_switch generators of BH traffic
      add_generator(paste0("BH_", i, "_"), bh_traffic[[i]], function() rexp(100, BH_lambda))

  env %>%
    run(until=Tsim) %>%
    wrap()
}

# grid of scenarios
cases <- expand.grid(n_switch = c(1, 2, 5),
                     fh_prio = c(0, 1),
                     preemptive = c(TRUE, FALSE))

# parallel simulation
system.time({
  envs <- parallel::mclapply(split(cases, 1:nrow(cases)), simulate,
                             mc.cores=nrow(cases), mc.set.seed=FALSE)
})

## ---- 1.analysis

library(tidyverse)

bp.vals <- function(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95)) {
  r <- quantile(x, probs=probs, na.rm=TRUE)
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

arrivals <- envs %>%
  get_mon_arrivals() %>%
  left_join(rowid_to_column(cases, "replication")) %>%
  filter(!(fh_prio==0 & preemptive==TRUE)) %>%
  separate(name, c("Flow", "index", "n")) %>%
  mutate(total_time = end_time - start_time,
         waiting_time = total_time - activity_time,
         Queue = forcats::fct_recode(interaction(fh_prio, preemptive),
           "without SP" = "0.FALSE",
           "with SP" = "1.FALSE",
           "with SP & preemption" = "1.TRUE"))

arrivals %>%
  filter(n_switch == 1) %>%
  # plot
  ggplot(aes(Queue, waiting_time*1e9, color=Flow)) + theme_bw() +
  stat_summary(fun.data=bp.vals, geom="boxplot", position="dodge") +
  theme(legend.justification=c(0, 1), legend.position=c(.02, .98)) +
  labs(y = "Queueing Delay [ns]", x = element_blank())

arrivals %>%
  filter(Flow == "FH") %>%
  # plot
  ggplot(aes(factor(n_switch), waiting_time*1e9, color=Queue)) + theme_bw() +
  stat_summary(fun.data=bp.vals, geom="boxplot", position="dodge") +
  theme(legend.justification=c(0, 1), legend.position=c(.02, .98)) +
  labs(y = "Queueing Delay [ns]", x = "No. of switches")
