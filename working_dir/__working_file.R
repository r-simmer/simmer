# Sys.setenv("PKG_CXXFLAGS"="-std=c++0x")
# TODO: create SkipEventsEvent
# TODO: create resource overview
library(simmer)
library(magrittr)

library(ggthemr)
ggthemr("fresh", layout="scientific")


t1<-
create_trajectory("test") %>%
  add_seize_event("vpk",1.0) %>%
  add_skip_event("sample(c(1,0),1)") %>%
  add_timeout_event("rnorm(1,10,1)") %>%
  add_timeout_event("rnorm(1,10,1)") %>%
  add_release_event("vpk",1.0) 
# 
# 
# t2<-
#   create_trajectory("test") %>%
#   add_seize_event("dr",1.0) %>%
#   add_timeout_event(50) %>%
#   add_release_event("dr",1.0) 


sim<-create_simulator("test", n = 50,verbose = T, until = 120) %>%
  add_resource("vpk",1) %>%
#   add_resource("dr",1) %>%
  add_entity(t1, name = "test", activation_time = 0) %>%
  add_entity(t1, name = "test", activation_time = 0) %>%
  add_entity(t1, name = "test", activation_time = 10) %>%
#   add_entity(t1, name = "test", activation_time = 0) %>%
  add_entities_with_interval(t1, n = 15, interval = 15) %>%
  simmer()


get_entity_monitor_values(sim)
get_entity_monitor_values(sim, aggregated =T)
get_resource_monitor_values(sim, "vpk") 

now_(sim@simulators[[1]])

plot_resource_utilization(sim, c("vpk"))
# plot_resource_usage(sim, "vpk", smooth_line = F)
plot_evolution_entity_times(sim, "activity_time")
# get_resource_monitor_values(sim, "dr") 
