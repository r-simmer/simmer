## TODOs 
## resource seizing blokkerend maken - DONE
## multiple resource seizing mogelijk maken - DONE
## simmer functie --> moet method van Simulator worden
## next_step beter/goed bepalen
## plot functies toevoegen
## simcontainer toevoegen : replications mogelijk maken + variations
## verbose: global options gebruiken




library(ggthemr)
ggthemr('fresh', layout="scientific")



# sim2<-

# order_objects_by_slot_value(sim2@events, "early_start")


t1<-
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
  add_trajectory("t1",t1) %>%
  add_entities_with_interval(10, "test", "t1", 5) %>%
  replicator(10)

# %>%
#   simmer()

simmer(sim, until = 240, verbose = TRUE)



sim$plot_resource_usage("arts")

# 
# 
# bar<-setRefClass("bar", fields = list(name = "character"))
# foo<-setRefClass("foo", fields = list(tcp_vector = "list"))
# 
# instal
# x1<-foo()
# x1$tcp_vector <- list(bar(name = "test1"))
# 
# x1$tcp_vector[[1]]$name # equals "test1"

# x2 <- x1$copy()
# 
# x2$tcp_vector[[1]]$name # equals "test1"
# 
# x2$tcp_vector[[1]]$name <- "test2"  # set to "test2"
# 
# x2$tcp_vector[[1]]$name # equals "test2"
# 
# x1$tcp_vector[[1]]$name # also equals "test2"??


