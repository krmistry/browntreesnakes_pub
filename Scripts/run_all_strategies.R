#### Run all strategies ####
library(here)

source(here("Scripts/static_strategy_function.R"))
source(here("Scripts/dynamic_strategy_functions.R"))
source(here("Scripts/dynamic_strategy_parallel_function.R"))


# # Temporarily changing number of variants for troubleshooting purposes
# num_variants <- 1

# Run static strategies
for(strat in 1:length(static_strategies)) {
  # Source strategy set up file
  source(paste0(here("Scripts/"),setup_file_names[static_strategies[strat]]))
  for(p in 1:length(P_list)) {
    for(d in 1:length(D_list)) {
      # Run IBM model for all variants, saves all outputs into appropriate results folder
      static_fun(P = p,
                 D = d,
                 strategy_name = static_strategies[strat])
    }
  }
}


# Run dynamic strategies using just for loops 
# (will take a very long time to run, but won't overload memory)
for(strat in 1:length(dynamic_strategies)) {
  # Source strategy set up file
  source(paste0(here("Scripts"),"/", setup_file_names[dynamic_strategies[strat]]))
  strat_num <- strat + 1
  # Set the maximum number of dynamic sets based on strategy (10 years)
  final_time_step <- erad_quarter_time_step*10
  #threshold_fun_name <- paste0("strat_", strat_num, "_threshold_fun")
  # Run each permutation
  for(p in 1:length(P_list)) {
    for(d in 1:length(D_list)) {
      for(variant in 1:num_variants) {
        parallel_fun(P = p, 
                     D = d, 
                     final_time_step = final_time_step,
                     variant = variant,
                     threshold_fun = strat_threshold_fun,
                     strategy_name = dynamic_strategies[strat],
                     quarter_time_step = erad_quarter_time_step) 
      }
    }
  }
}

# Starting with strategy 3, low, more small on loon


# Run dynamic strategies using parallel for loops 
# (will still take a long time to run but not as long, and there is some risk of memory overlaoding while saving 
# jags files, which would stop the run - have to keep an eye on it)
# Setting up parallel clusters
library(doParallel)
# Detect the number of clusters available
n_cores <- detectCores()
# Select half of them - broke, so trying fewer cores
cl <- makeCluster(n_cores/4, outfile = "")
registerDoParallel(cl)

for(strat in 1:length(dynamic_strategies)) {
  strat_num <- strat + 1
  
  # Run each permutation
  for(p in 1:length(P_list)) {
    for(d in 1:length(D_list)) {
      results <- foreach(variant = 1:num_variants)  %dopar% {
        library(here)
        source(here("Scripts/dynamic_strategy_parallel_function.R"))
        # Source strategy set up file
        source(paste0(here("Scripts"), "/", setup_file_names[dynamic_strategies[strat]]))
        # Set the maximum number of dynamic sets based on strategy (10 years)
        final_time_step <- erad_quarter_time_step*10
        parallel_fun(P = p, 
                     D = d, 
                     final_time_step = final_time_step,
                     variant = variant,
                     threshold_fun = strat_threshold_fun,
                     strategy_name = dynamic_strategies[strat],
                     quarter_time_step = erad_quarter_time_step)
      }
    }
  }
}
# Stop the cluster
stopCluster(cl = cl)

