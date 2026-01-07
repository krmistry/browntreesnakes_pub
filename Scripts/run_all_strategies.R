#### Run all strategies ####
library(here)

source(here("Scripts/static_strategy_function.R"))
source(here("Scripts/dynamic_strategy_functions.R"))
source(here("Scripts/dynamic_strategy_parallel_function.R"))
source(here("Scripts/static_strategy_function.R"))

# Set the folder to save results to (external harddive)
#save_folder <- "D:/BTS_pub"
save_folder <- "E:/BTS_pub"
#save_folder <- here("Results")

# Create results folder list (also creates the actual folders if they don't exist yet)
results_folders <- results_folder_fun(save_folder = save_folder)

# # # Temporarily changing number of variants for troubleshooting purposes
# # num_variants <- 1
# 
# Run static strategies
for(strat in 1:length(static_strategies)) {
  # Source strategy set up file
  source(paste0(here("Scripts"),"/",setup_file_names[static_strategies[strat]]))
  for(p in 1:length(P_list)) {
    for(d in 1:length(D_list)) {
      # Run IBM model for all variants, saves all outputs into appropriate results folder
      static_fun(P = p,
                 D = d,
                 strategy_name = static_strategies[strat])
    }
  }
}


# # Run dynamic strategies using just for loops
# # (will take a very long time to run, but won't overload memory)
# for(strat in 1:length(dynamic_strategies)) {
#   # Source strategy set up file
#   source(paste0(here("Scripts"),"/", setup_file_names[dynamic_strategies[strat]]))
#   # Run each permutation
#   for(p in 1:length(P_list)) {
#     for(d in 1:length(D_list)) {
#       for(variant in 1:num_variants) {
#         parallel_fun(P = p,
#                      D = d,
#                      final_time_step = final_time_step,
#                      variant = variant,
#                      threshold_fun = strat_threshold_fun,
#                      strategy_name = dynamic_strategies[strat],
#                      quarter_time_step = erad_quarter_time_step)
#       }
#     }
#   }
# }

# Starting with strategy 3, low, more small on loon


# Run dynamic strategies using parallel for loops 
# (will still take a long time to run but not as long, and there is some risk of memory overlaoding while saving 
# jags files, which would stop the run - have to keep an eye on it)
# Setting up parallel clusters
library(doParallel)
# Detect the number of clusters available
n_cores <- detectCores()
# Select half of them - broke, so trying fewer cores
cl <- makeCluster(2, outfile = "")
registerDoParallel(cl)



# On Loon, model runs:
# - Strategy 3
#   - (P = 1, D = 1) - check for incomplete runs
#   - (P = 1, D = 2)
#   - (P = 3, D = 1) 
# - Strategy 4
#   - (P = 1, D = 2) 
#   - (P = 3, D = 1)

# On Ursus:
# - Strategy 4
#   - (P = 2, D = 1) 
# - Strategy 3
#   - (P = 2, D = 1)
#   - (P = 2, D = 2)

# On Delphine:
# - Strategy 2
#   - (P = 1, D = 1) 
#   - (P = 1, D = 2)
#   - (P = 3, D = 1)
# - Strategy 4
#   - (P = 1, D = 2)
#   - (P = 3, D = 1)

# On Ursus:
# - Strategy 4
#   - (P = 2, D = 1) 
#   - (P = 2, D = 2) 
#   - (P = 3, D = 2)
# - Strategy 3
#   - (P = 2, D = 1)
#   - (P = 2, D = 2)
#   - (P = 3, D = 2)

# On Delphine:
# - Strategy 2
#   - (P = 1, D = 1) 
#   - (P = 1, D = 2)
#   - (P = 2, D = 1)
#   - (P = 2, D = 2)
#   - (P = 3, D = 1)
# - Strategy 4
#   - (P = 1, D = 1) 

# Run for each strategy (actually only running 1 at a time, because of long run times and large output files)
# for(strat in 1:length(dynamic_strategies)) {
#   # Run each permutation
#   for(p in 2:length(P_list)) {
#     for(d in 1:length(D_list)) {
      results <- foreach(variant = 1:num_variants)  %dopar% {
        library(here)
        source(here("Scripts/dynamic_strategy_parallel_function.R"))
        strat <- 3
        p <- 3
        d <- 2
        # Source strategy set up file
        source(paste0(here("Scripts"), "/", setup_file_names[dynamic_strategies[strat]]))
        # Create object with results folder names, based on save_folder (and create those folders if they don't exist)
        results_folders <- results_folder_fun(save_folder = save_folder)
        parallel_fun(P = p, 
                     D = d, 
                     final_time_step = final_time_step,
                     variant = variant,
                     threshold_fun = strat_threshold_fun,
                     strategy_name = dynamic_strategies[strat],
                     quarter_time_step = erad_quarter_time_step)
      }
#     }
#   }
# }

# Stop the cluster
stopCluster(cl = cl)



# Check for and compile incomplete variant runs
run_status <- vector()
for(variant in 1:num_variants) {
  run_status[variant] <- check_run_completion_fun(P = 1,
                                         D = 1,
                                         strategy_name = dynamic_strategies[1],
                                         variant = variant)
}
incomplete_runs <- which(run_status == "incomplete")

# Re-run incomplete variant runs (from beginning)
#results <- foreach(variant = incomplete_runs)  %dopar% {
  library(here)
  source(here("Scripts/dynamic_strategy_parallel_function.R"))
  strat <- 2
  p <- 1
  d <- 1
  # Source strategy set up file
  source(paste0(here("Scripts"), "/", setup_file_names[dynamic_strategies[strat]]))
  # Create object with results folder names, based on save_folder (and create those folders if they don't exist)
  results_folders <- results_folder_fun(save_folder = save_folder)
  parallel_fun(P = p, 
               D = d, 
               final_time_step = final_time_step,
               variant = variant,
               threshold_fun = strat_threshold_fun,
               strategy_name = dynamic_strategies[strat],
               quarter_time_step = erad_quarter_time_step)
#}


# Stop the cluster
stopCluster(cl = cl)

# Re-run any strategy, any permutation from beginning
results <- foreach(rerun = 2:nrow(variants_w_issues))  %dopar% {
  run <- variants_w_issues[rerun,]
  library(here)
  source(here("Scripts/dynamic_strategy_parallel_function.R"))
  strat <- which(strategies == run$strategy) - 1
  p <- which(names(starting_density) == run$density)
  d <- which(names(starting_size_dist) == run$size_dist)
  variant <- as.numeric(run$variant)
  # Source strategy set up file
  source(paste0(here("Scripts"), "/", setup_file_names[dynamic_strategies[strat]]))
  # Create object with results folder names, based on save_folder (and create those folders if they don't exist)
  results_folders <- results_folder_fun(save_folder = save_folder)
  parallel_fun(P = p, 
               D = d, 
               final_time_step = final_time_step,
               variant = variant,
               threshold_fun = strat_threshold_fun,
               strategy_name = dynamic_strategies[strat],
               quarter_time_step = erad_quarter_time_step)
}
