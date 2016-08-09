sd_section("Build trajectories",
  "Methods for building trajectory objects.",
  c(
    "create_trajectory",
    "batch",
    "branch",
    "clone",
    "join",
    "leave",
    "renege_in",
    "rollback",
    "seize",
    "select",
    "set_attribute",
    "set_prioritization",
    "timeout"
  )
)

sd_section("Manage trajectories",
  "Methods for getting information about trajectory objects.",
  c(
    "get_head",
    "get_n_activities",
    "get_next_activity",
    "print_activity"
  )
)

sd_section("Build and run simulation environments",
  "Methods for building and running simulation objects.",
  c(
    "simmer",
    "add_generator",
    "add_resource",
    "reset",
    "run",
    "wrap"
  )
)

sd_section("Manage simulation environments",
  "Methods for getting information about simulation components.",
  c(
    "get_n_generated",
    "resource",
    "now",
    "peek"
  )
)

sd_section("Statistics",
  "Methods for getting monitored statistics.",
  c(
    "get_mon"
  )
)

sd_section("Convenience functions",
  "Helper methods for generators and resources.",
  c(
    "at",
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
