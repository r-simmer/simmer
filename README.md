simmer: Discrete Event Simulation done R(ight)
==============================================

Example trajectory:
```
traj1<-
  read.table(header=T, text=
               "event_id  description resource        amount  duration      successor
                1         intake      vpk/logistieke  1/1     rnorm(1,10)   sample(c(2,3),1)
                2         consult     arts            1       rnorm(1,10)   NA
                3         logistieke  logistieke      1       rnorm(1,10)   NA"
  )
```


Example simulation:
```
library(magrittr)
sim<-
  create_simulator(name = "SuperDuperSim") %>%
  add_resource("vpk", 1) %>%
  add_resource("logistieke", 2) %>%
  add_resource("arts", 2) %>%
  add_trajectory("t1",t2) %>%
  add_entities_with_interval(10, "test", "t1", 5) 


simmer(sim, until = 240, verbose = TRUE)
```