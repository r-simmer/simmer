# Sys.setenv("PKG_CXXFLAGS"="-std=c++0x")

library(simmer)
library(magrittr)

library(ggthemr)
ggthemr("fresh", layout="scientific")


t1<-
create_trajectory("test") %>%
  add_seize_event("vpk",1.0) %>%
  add_timeout_event("rnorm(1,10,1)") %>%
  add_release_event("vpk",1.0) 
# 
# 
# t2<-
#   create_trajectory("test") %>%
#   add_seize_event("dr",1.0) %>%
#   add_timeout_event(50) %>%
#   add_release_event("dr",1.0) 


sim<-create_simulator("test", n = 50,verbose = F) %>%
  add_resource("vpk",1) %>%
#   add_resource("dr",1) %>%
  add_entity(t1, name = "test", activation_time = 0) %>%
  add_entity(t1, name = "test", activation_time = 0) %>%
  add_entity(t1, name = "test", activation_time = 10) %>%
#   add_entity(t1, name = "test", activation_time = 0) %>%
  add_entities_with_interval(t1, n = 15, interval = 15) %>%
  run()


# get_entity_monitor_values(sim)
# get_resource_monitor_values(sim, "vpk") 

# plot_resource_usage(sim, "vpk", smooth_line = F)
plot_evolution_entity_times(sim, "activity_time")
# get_resource_monitor_values(sim, "dr") 

# add_entity("fdssq", t1, activation_time = 0)
# 
# for(i in 1:10){}
#   print(i)
#   e1<-
#     create_entity(activation_time = 50) %>%
#     add_seize_event("vpk",1.0) %>%
#     add_timeout_event(10) %>%
#     add_release_event("vpk",1.0) 
#   
#   e2<-
#     create_entity() %>%
#     add_seize_event("vpk",1) %>%
#     add_timeout_event(10) %>%
#     add_release_event("vpk",1)
#   
#   e3 <- copy_entity(e1)
#   
#   sim<-
#     create_simulator(verbose=F) %>%
#     add_resource("vpk", 1) %>%
#     add_entity(e1) %>%
#     add_entity(e2) %>%
#     add_entity(e3) %>%
#     run()
#   
# #   gc()
# }
# 
# get_entity_monitor_values(sim)
# get_resource_monitor_values(sim, "vpk")
# 
# # 
# # sim<-
# #   create_simulator(verbose=T) %>%
# #   add_resource("vpk",1) %>%
# #   add_entity(e1) %>%
# #   add_entity(
# #     create_entity(activation_time = 50) %>%
# #       add_seize_event("vpk",1.0) %>%
# #       add_timeout_event(10) %>%
# #       add_release_event("vpk",1.0) 
# #   ) %>%
# #   add_entity(
# #     create_entity(activation_time = 0) %>%
# #       add_seize_event("vpk",1.0) %>%
# #       add_timeout_event(10) %>%
# #       add_release_event("vpk",1.0) 
# #   ) %>%
# #   add_entity(
# #     create_entity() %>%
# #       add_seize_event("vpk",1.0) %>%
# #       add_timeout_event(10) %>%
# #       add_release_event("v5pk",1.0) 
# #   ) %>%
# #   add_entity(
# #     create_entity() %>%
# #       add_seize_event("vpk",1.0) %>%
# #       add_timeout_event(10) %>%
# #       add_release_event("vpk",1.0) 
# #   ) %>%
# #   #   add_entity(e2) %>%
# #   run()
# 
# # # rm(list=ls())
# # # gc()
