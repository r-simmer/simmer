## TODOs 
## resource seizing blokkerend maken - DONE
## multiple resource seizing mogelijk maken - DONE
## simmer functie --> moet method van Simulator worden
## next_step beter/goed bepalen
## plot functies toevoegen
## simcontainer toevoegen : replications mogelijk maken + variations









# sim2<-

# order_objects_by_slot_value(sim2@events, "early_start")


traj1<-
  read.table(header=T, text=
               "event_id description resource amount duration successor
                1 vpk vpk 1 rnorm(1,10) sample(c(2,3),1)
                2 arts arts 1 rnorm(1,10) NA
                3 logistieke logistieke 1 rnorm(1,10) NA"
  )

t2<-
  read.table(header=T, text=
               "event_id description resource amount duration successor
             1 vpk vpk/logistieke 1/1 rnorm(1,10) 2
             2 arts arts 1 rnorm(1,10) NA
             3 logistieke logistieke 1 rnorm(1,10) NA"
  )

library(magrittr)
sim<-
  create_simulator(name = "SuperDuperSim") %>%
  #   add_entity("test","r4e5rea4") 
  add_resource("vpk", 1) %>%
  add_resource("logistieke", 2) %>%
  add_resource("arts", 2) %>%
  add_trajectory("t1",t2) %>%
  add_entities_with_interval(10, "test", "t1", 5) 

# %>%
#   simmer()

simmer(sim, until = 240, verbose = TRUE)

