sd_section("Build trajectories",
  "Methods for building trajectory objects.",
  c(
    "create_trajectory",
    "branch",
    "release",
    "rollback",
    "seize",
    "set_attribute",
    "timeout"
  )
)

sd_section("Manage trajectories",
  "Methods for getting information about trajectory objects.",
  c(
    "get_head",
    "get_tail",
    "get_n_activities",
    "get_next_activity",
    "get_prev_activity",
    "print_activity"
  )
)

sd_section("Build and run simulation environments",
  "Methods for building and running simulation objects.",
  c(
    "simmer",
    "add_generator",
    "add_resource",
    "onestep",
    "reset",
    "run",
    "wrap"
  )
)

sd_section("Manage simulation environments",
  "Methods for getting information about simulation components.",
  c(
    "get_capacity",
    "get_n_generated",
    "get_queue_size",
    "get_queue_count",
    "get_server_count",
    "now",
    "peek"
  )
)

sd_section("Statistics",
  "Methods for getting monitored statistics.",
  c(
    "get_mon_arrivals",
    "get_mon_attributes",
    "get_mon_resources"
  )
)

sd_section("Convenience functions",
  "Helper methods for generators and resources.",
  c(
    "at",
    "every",
    "from",
    "from_to",
    "schedule",
    "to"
  )
)

sd_section("Plotting",
  "Basic plotting functions.",
  c(
    "plot_attributes",
    "plot_evolution_arrival_times",
    "plot_resource_usage",
    "plot_resource_utilization"
  )
)
