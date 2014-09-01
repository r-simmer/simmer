## TODOs 
## plot functions to different file --> decouple from refclasses --> to regular functions
## convenience function to different file
## BUG: readme example crashed with until of 120


# options(verbose = T)

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
create_simulator() %>%
  add_trajectory(name = "t2", trajectory_df = t2) %>%
  add_resource(name = "vpk", 3) %>%
  add_resource(name = "logistieke", 3) %>%
  add_resource(name = "arts", 3) %>%
  add_entities_with_interval(10, "test", "t1", 5) 

sim$init_events()

library(magrittr)
library(simmer)
sim<-
  create_simulator(name = "SuperDuperSim") %>%
  #   add_entity("test","r4e5rea4") 
  add_resource("vpk", 4) %>%
  add_resource("logistieke", 4) %>%
  add_resource("arts", 4) %>%
  add_trajectory("t1",t1) %>%
  add_entities_with_interval(10, "test", "t1", 5) %>%
  replicator(40)

# %>%
#   simmer()

simmer(sim, until = 120)

# Rprof("Rprof.out")
# simmer(sim, until = 120)
# Rprof(NULL)
# summaryRprof("Rprof.out")


res<-Resource()

# 
# plot_resource_utilization(sim)
plot_evolution_entity_times(sim, type="flow_time")









# 
# plot_resource_usage(sim, "vpk", 2)
# plot_resource_usage(sim, "vpk")
# sim$simulators[[2]]$get_resource("arts")
# 
# 
# library(dplyr)
# 
# entity_data<-sim$entities[[3]]$time_value_monitor$data
# 
# activity_data<-
#   entity_data %>%
#   group_by(t) %>%
#   summarise(v=max(v)) %>%
#   mutate(activity_time = (t-lag(t)) * lag(v)) %>%
#   ungroup() %>%
#   summarise(activity_time = sum(activity_time, na.rm=T)) %>%
#   data.frame(activity_time = ., 
#              start_time = min(subset(entity_data, v>0, select="t")),
#              end_time = entity_data[nrow(entity_data),"t"]) %>%
#   mutate(flow_time = end_time - start_time,
#          waiting_time = flow_time - activity_time)
# 
# 
# # 
# # 
# # bar<-setRefClass("bar", fields = list(name = "character"))
# # foo<-setRefClass("foo", fields = list(tcp_vector = "list"))
# # 
# # instal
# # x1<-foo()
# # x1$tcp_vector <- list(bar(name = "test1"))
# # 
# # x1$tcp_vector[[1]]$name # equals "test1"
# 
# # x2 <- x1$copy()
# # 
# # x2$tcp_vector[[1]]$name # equals "test1"
# # 
# # x2$tcp_vector[[1]]$name <- "test2"  # set to "test2"
# # 
# # x2$tcp_vector[[1]]$name # equals "test2"
# # 
# # x1$tcp_vector[[1]]$name # also equals "test2"??
# 
# 
# run_times<-sapply(sim$simulators, function(sim_obj) sim_obj$now())
# resources_names<-sapply(sim$simulators[[1]]$resources, function(obj) obj$name)
# resources_capacity<-sapply(sim$simulators[[1]]$resources, function(obj) obj$capacity)
# 
# 
# dataset<-
#   do.call(rbind,
#           mapply(function(sim_obj, rep, runtime) { 
#             dataset<-
#               do.call(rbind,
#                       mapply(function(resource_name, resource_capacity){
#                         dataset<- sim_obj$get_resource(resource_name)$monitor$data
#                         dataset$resource<-resource_name
#                         dataset$capacity<-resource_capacity
#                         dataset
#                       }, resources_names, resources_capacity, SIMPLIFY=F))  
#             
#             dataset$rep<-rep
#             dataset$runtime<-runtime
#             dataset
#           }, sim$simulators, 1:length(sim$simulators),run_times, SIMPLIFY=F)
#   )
# 
# 
# dataset %>%
#   group_by(resource, rep, capacity, runtime, t) %>%
#   summarise(v=max(v)) %>%
#   ungroup() %>%
#   group_by(resource, rep, capacity, runtime) %>%
#   mutate(in_use = (t-lag(t)) * lag(v)) %>%
#   group_by(resource, rep, capacity, runtime) %>%
#   summarise(in_use = sum(in_use, na.rm=T)) %>%
#   ungroup() %>%
#   mutate(utilization = in_use / runtime) %>%
#   group_by(resource, capacity) %>%
#   summarise(Q25 = quantile(utilization, .25),
#             Q50 = quantile(utilization, .5),
#             Q75 = quantile(utilization, .75))
# 
# 
# test<-function(...){
#   print(list(...))
# }
# 
# test(3,4,8, 9)

testClass<-setRefClass("testClass", fields = list(value = "numeric", text = "character"))

ptm <- Sys.time()
holdList<-list()
for(x in 1:1000){
  testClass(value = x, text=as.character(x))
  holdList<-c(holdList, testClass)
}
Sys.time() - ptm

ptm <- Sys.time()
holdList<-list(values= c(), texts = c())
for(x in 1:1000){
 holdList$values<-c(holdList$values, x)
 holdList$texts<-c(holdList$texts, as.character(x))
}
Sys.time() - ptm

