Entity <- setRefClass("Entity", 
                      fields = list(
                        name = "character",
                        entity_index = "numeric",
                        trajectory = "character",
                        trajectory_index = "numeric",
                        early_start_time="numeric",
                        current_event = "Event",
                        time_value_monitor = "TimeValueMonitor",
                        key_value_monitor = "KeyValueMonitor")
)



