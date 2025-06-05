## Strategy 2 set up

### Manually set IBM parameters
# # Number of quarters to generate - 10 years
# erad_quarter_time_step <- 4

# Methods for starting condition and threshold 1 
method_option_names <- c("initial", "threshold_1")
method_options <- list()
for(option in 1:length(method_option_names)) {
  method_options[[option]] <- list()
}
names(method_options) <- method_option_names

# Identifying the methods that will be used under each of these conditions
method_options$initial$methods <- erad_methods
method_options$threshold_1$methods <- erad_methods[2]

## Create list to hold quarters where eradication methods are used for each condition
for(option in 1:2) {
  method_options[[option]]$erad_quarters <- list()
  for(method in method_options[[option]]$methods) {
    method_options[[option]]$erad_quarters[[method]] <- c(1:erad_quarter_time_step)
  }
}

## Days where eradication methods are used for each condition
# Initial condition days
method_options$initial$erad_days <- list()
for(quarter in 1:erad_quarter_time_step) {
  method_options$initial$erad_days[[quarter]] <- list()
  # ADS: low effort (1 treatment) per quarter
  method_options$initial$erad_days[[quarter]]$ADS <- c(45, 48)
  # Visual survey: medium effort (6 weeks, 2 teams) per quarter - surveying every other day
  method_options$initial$erad_days[[quarter]]$visual <- seq(2, (7*6 - 1), 2)
  # Trap: medium effort (6 weeks) per quarter - checking traps every 3 days
  method_options$initial$erad_days[[quarter]]$trap <- seq(2, (7*6 - 1), 3)
  # Bait tubes: medium effort (6 weeks) per quarter
  method_options$initial$erad_days[[quarter]]$bait_tube <- seq(7*6, (7*12 - 1), 3)
}
names(method_options$initial$erad_days) <- paste0("quarter_", c(1:erad_quarter_time_step))

# Threshold 1 days (visual surveys only)
method_options$threshold_1$erad_days <- list()
for(quarter in 1:erad_quarter_time_step) {
  method_options$threshold_1$erad_days[[quarter]] <- list()
  # Visual survey: medium effort (6 weeks, 2 teams) per quarter - surveying every other day
  method_options$threshold_1$erad_days[[quarter]]$visual <- seq(2, (7*6 - 1), 2)
}
names(method_options$threshold_1$erad_days) <- paste0("quarter_", c(1:erad_quarter_time_step))


# Bounds of primary sampling periods for each condition (the same for the first 3)
for(option in 1:length(method_options)) {
  method_options[[option]]$primary_sampling_period <- c(2,(7*6 - 1))
}


## Coverage for each method in a quarter for each condition
for(option in 1:length(method_options)) {
  method_options[[option]]$erad_coverage <- list()
  # overlap of ADS over transects (100%) is the same for all conditions
  method_options[[option]]$ADS_overlap_on_transect <- 1 
}
# Initial condition coverage
method_options$initial$erad_coverage$ADS <- 1
# Transect coverage (~50%)
method_options$initial$erad_coverage$transects_per_quarter <- 0.5

# Threshold 1 condition coverage
method_options$threshold_1$erad_coverage$ADS <- 0 # necessary for a function, can fix this later
# Transect coverage (100% because effort is doubled) 
method_options$threshold_1$erad_coverage$transects_per_quarter <- 1


# Number of visual survey teams for each method
for(option in 1:length(method_options)) {
  method_options[[option]]$num_teams <- list()
  method_options[[option]]$cost_num_teams <- list()
}
# This one is used to calculate the spatial coverage 
# (there might be more than one team, but if they don't overlap spatially, then the encounter
# probability will be the same as if its one team)
method_options$initial$num_teams$visual <- 1
method_options$threshold_1$num_teams$visual <- 1

# This one is used to calculate the cost, so its the actual number of teams per quarter
# Initial 
method_options$initial$cost_num_teams$visual <- 1
method_options$initial$cost_num_teams$trap <- 1
method_options$initial$cost_num_teams$bait_tube <- 1
# Threshold 1
method_options$threshold_1$cost_num_teams$visual <- 2



