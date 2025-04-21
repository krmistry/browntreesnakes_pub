## Function to run static strategy and save IBM results


static_fun <- function(P,
                       D,
                       strategy_name) {
  # Set up initial population parameters
  N_reps <- starting_pop[[P]]
  size_dist <- starting_size_dist[[D]]
  for(variant in 1:num_variants) {
    # Setting starting N for this variant
    N <- N_reps[variant]
    replicates <- list()
    # Separating out parameters based on condition (only 1 condition for static)
    coverage <- method_options$initial$erad_coverage
    primary_sampling_period <- method_options$initial$primary_sampling_period
    erad_quarters <- method_options$initial$erad_quarters
    erad_days <- method_options$initial$erad_days
    ADS_overlap <- method_options$initial$ADS_overlap_on_transect
    num_teams <- method_options$initial$num_teams
    # Running model 
    replicates <- quarter_operations(initial_N = N, 
                                     initial_size_dist = size_dist, 
                                     p_g = g_density_prob,
                                     lambda = lambda,
                                     total_quarters = erad_quarter_time_step,
                                     total_days = day_time_step,
                                     erad = "on",
                                     erad_method = names(erad_quarters),
                                     erad_coverage = coverage,
                                     primary_sampling_period = primary_sampling_period,
                                     erad_quarters = erad_quarters,
                                     erad_days = erad_days,
                                     ADS_overlap_on_transect = ADS_overlap,
                                     type_of_run = "initial",
                                     num_teams = num_teams)
    # Save each IBM results for each variant
    saveRDS(replicates, file = paste0(results_folders[[strategy_name]][[P]][[D]]["IBM"], "/IBM_", 
                                      names(starting_pop)[P], "_", names(starting_size_dist)[D], 
                                      "-var_", variant, ".rds"))
    print(paste0("variant ", variant, " completed"))
  }
}
