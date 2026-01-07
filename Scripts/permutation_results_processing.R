### Processing & plotting permutation results for each strategy

library(reshape2)
library(here)
library(tidyr)

source(here("Scripts/all_strategy_set_up.R"))
source(here("Scripts/model_evaluation.R"))
source(here("Scripts/results_processing_functions.R"))
source(here("Scripts/alt_strategies_cost_calc.R"))
source(here("Scripts/strategy_2_set_up.R"))
source(here("Scripts/strategy_3_set_up.R"))
source(here("Scripts/strategy_4_set_up.R"))
source(here("Scripts/IBM_threshold_funs.R"))

# Strategy  & permutation names
strategies <- paste0("Strategy_", c("one", "two", "three", "four"))
permutations <- vector()
counter <- 1
for(p in 1:length(starting_pop)) {
  for(d in 1:length(starting_size_dist)) {
    permutations[counter] <- paste0(names(starting_pop)[p], "_", names(starting_size_dist)[d])
    counter <- counter + 1
  }
}

# Plot labels & colors
plot_labels <- list()
plot_labels$size_class <- c("small" = "Small",
                            "medium" = "Medium",
                            "large" = "Large",
                            "xlarge" = "X-large")
plot_labels$method <- c("ADS" = "ADS",
                        "visual" = "Visual survey",
                        "trap" = "Live traps",
                        "bait_tube" = "Bait tubes")
plot_labels$type_of_N <- c("small" = "Small", 
                           "medium" = "Medium", 
                           "large" = "Large", 
                           "xlarge" = "X-large", 
                           "total" = "Total")
plot_labels$strategy <- c("ADS only",
                          "all methods",
                          "ADS plus monitoring",
                          "ground-based methods")
names(plot_labels$strategy) <- strategies
plot_labels$permutation <- c("low density,\n more small snakes",
                            "low density,\n more x-large snakes",
                            "medium density,\n more small snakes",
                            "medium density,\n more x-large snakes",
                            "high density,\n more small snakes",
                            "high density,\n more x-large snakes")
names(plot_labels$permutation) <- permutations
plot_labels$init_pop <- c("low" = "Low", "medium" = "Medium", "high" = "High")
plot_labels$init_size_dist <- c("more_small" = "More small", "more_xlarge" = "More extra-large")

plot_colors <- list()
plot_colors$strategy <- hue_pal()(length(strategies))
names(plot_colors$strategy) <- strategies
plot_colors$permutation <- hue_pal()(length(permutations))
names(plot_colors$permutation) <- permutations

# Checking for and creating if necessary all results folders
save_folder <- paste0(here("Results", "alt_strategies", "Manuscript_plots"), "/")

#### Analyzing results by permutation & strategy ####

N_data_list <- list()
estimates_list <- list()
conditions_list <- list()
objectives_probs <- list()
permutation_costs <- list()
for(permutation_name in permutations) {
  N_data_list[[permutation_name]] <- list()
  estimates_list[[permutation_name]] <- list()
  conditions_list[[permutation_name]] <- list()
  permutation_costs[[permutation_name]] <- list()
  objectives_probs[[permutation_name]] <- as.data.frame(matrix(NA,
                                                          nrow = length(strategies),
                                                          ncol = 7))
  colnames(objectives_probs[[permutation_name]]) <- c("strategy", "erad_prob", "mean_erad_quarter",
                                                 "total_suppress_prob", "mean_total_quarter",  "upper_3_suppress_prob",
                                                 "mean_upper_3_quarter")
  row_counter <- 1
  for(strategy in strategies) {
    N_data_list[[permutation_name]][[strategy]] <- list()
    estimates_list[[permutation_name]][[strategy]] <- list()
    # Read in results for this permutation and strategy
    permutation_results <- readRDS(paste0(here("Results", "alt_strategies", strategy, "permutation_results"),
                                            "/permutation-", permutation_name, "_results.RDS"))
    # Assign N data 
    N_data_list[[permutation_name]][[strategy]] <- permutation_results$N_data_plot$data_all_variants
    # Assign estimated N data
    estimates_list[[permutation_name]][[strategy]] <- permutation_results$N_data_plot$data_all_variants
    # Assign conditions data (except for strategy 1)
    if(strategy != strategies[1]) {
      conditions_list[[permutation_name]][[strategy]] <- list()
      conditions_list[[permutation_name]][[strategy]] <- permutation_results$condition_plot$condition_record
    }
    # Assign eradication & threshold probabilities
    objectives_probs[[permutation_name]]$strategy[row_counter] <- strategy
    objectives_probs[[permutation_name]]$erad_prob[row_counter] <- permutation_results$erad_prob$erad_prob
    objectives_probs[[permutation_name]]$mean_erad_quarter[row_counter] <- mean(permutation_results$erad_prob$erad_quarter)
    objectives_probs[[permutation_name]]$total_suppress_prob[row_counter] <- permutation_results$total_suppress_prob$suppession_prob
    objectives_probs[[permutation_name]]$mean_total_quarter[row_counter] <- mean(permutation_results$total_suppress_prob$suppression_reached[(!is.na(permutation_results$total_suppress_prob$suppression_reached))])
    objectives_probs[[permutation_name]]$upper_3_suppress_prob[row_counter] <- permutation_results$upper_3_suppress_prob$suppession_prob
    objectives_probs[[permutation_name]]$mean_upper_3_quarter[row_counter] <- mean(permutation_results$upper_3_suppress_prob$suppression_reached[(!is.na(permutation_results$upper_3_suppress_prob$suppression_reached))])


    # Processing and saving permutation costs (for dynamic strategies, so all but strategy one)
    if(strategy %in% strategies[c(3:4)]) {
      permutation_costs[[permutation_name]][[strategy]] <- dynamic_strategy_cost_calc(permutation_results,
                                                    strategy_condition_costs,
                                                    strategy = strategy)
    } else if (strategy == strategies[1]) {
      permutation_costs[[permutation_name]][[strategy]] <- strategy_condition_costs[[strategy]]$initial
    } else {
      permutation_costs[[permutation_name]][[strategy]] <- strat_2_dynamic_strategy_cost_calc(permutation_results,
                                                                                              strategy_condition_costs)
    }
      
    row_counter <- row_counter + 1
  }
}

 

# Combining all N data with appropriate labeling
all_data <- melt(N_data_list, id.vars = colnames(N_data_list[[1]][[1]]))
colnames(all_data)[c(5:6)] <- c("Strategy", "permutation")
all_data <- separate(all_data, permutation, into = c("init_pop", "init_size_dist"), 
                     sep = "_", extra = "merge")

# Creating another column so geom_path will isolate each variant in each permutation
all_data <- mutate(all_data, variant_strategy = paste0(Strategy, "_",
                                                            variant))
# Manually set levels for initial pop & strategies for legend order
all_data$init_pop <- factor(all_data$init_pop, levels = c("low", "medium", "high"))
all_data$Strategy <- factor(all_data$Strategy, levels = strategies)
all_data$permutation <- paste0(all_data$init_pop, "_", all_data$init_size_dist)
all_data$permutation <- factor(all_data$permutation, levels = c("low_more_small",
                                                                "medium_more_small",
                                                                "high_more_small",
                                                                "low_more_xlarge",
                                                                "medium_more_xlarge",
                                                                "high_more_xlarge"))
# Creating another column so geom_path will isolate each variant in each strategy
all_data <- mutate(all_data, variant_permutation = paste0(permutation, "_",
                                                          variant))

# Saving plots comparing abundance through time for each size class and then total population, by strategy and by permutation
for(pop_type in unique(all_data$size_class)) {
  permutations_plot <- ggplot(all_data[all_data$size_class == pop_type,], aes(x = Quarter/4, y = N, color = Strategy)) +
    geom_path(aes(group = variant_strategy), alpha = 0.5) +
    facet_grid(rows = vars(init_size_dist), cols = vars(init_pop),
               labeller = labeller(init_size_dist = plot_labels$init_size_dist,
                                   init_pop = plot_labels$init_pop)) +
    theme_bw() +
    scale_color_manual(values = plot_colors$strategy,
                       labels = plot_labels$strategy) +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.position="top") +
    #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.75)) +
    labs(x = "Year", color = "",
         y = paste0(str_to_title(pop_type), " population"))
  # 
  ggsave(filename = paste0(save_folder, "all_strategies_by_permutation-", pop_type,"_pop.png"),
         permutations_plot, device = 'png', width = 6.2, height = 4)
  
  # Refactor so plot labeling makes more sense in the strategies plot
  all_data$permutation <- factor(all_data$permutation, levels = c("low_more_small",
                                                                  "low_more_xlarge",
                                                                  "medium_more_small",
                                                                  "medium_more_xlarge",
                                                                  "high_more_small",
                                                                  "high_more_xlarge"))
  
  strategies_plot <- ggplot(all_data[all_data$size_class == pop_type,], 
                            aes(x = Quarter/4, y = N, color = permutation)) +
    geom_path(aes(group = variant_permutation), alpha = 0.4) +
    facet_wrap(vars(Strategy), 
               labeller = labeller(Strategy = plot_labels$strategy)) +
    theme_bw() +
    scale_color_manual(values = plot_colors$permutation,
                       labels = plot_labels$permutation) +
    guides(color = guide_legend(override.aes = list(alpha = 1))) +
    theme(legend.position="bottom") +
    #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.75)) +
    labs(x = "Year", 
         y = paste0(str_to_title(pop_type), " population"),
         color = "")
  
  ggsave(filename = paste0(save_folder, "all_permutations_by_strategy-", pop_type, "_pop.png"),
         strategies_plot, device = 'png', width = 7, height = 4)
  
}



##### Objectives performance #####
all_objs_probs <- melt(objectives_probs, id.vars = colnames(objectives_probs[[1]]))
colnames(all_objs_probs)[8] <- "permutation"

all_objs_probs$permutation <- factor(all_objs_probs$permutation, levels = c("low_more_small",
                                                                            "medium_more_small",
                                                                            "high_more_small",
                                                                            "low_more_xlarge",
                                                                            "medium_more_xlarge",
                                                                            "high_more_xlarge"))
all_objs_probs$strategy <- factor(all_objs_probs$strategy, levels = strategies)

objectives_plot_by_permutation <- ggplot(all_objs_probs) +
  geom_point(aes(y = erad_prob, x = total_suppress_prob, color = strategy)) +
  facet_wrap(vars(permutation)) +
  theme_bw()+
  labs(x = "Probability of upper 3 size class suppression", y = "Probability of full eradication")

ggsave(filename = paste0(save_folder, "_objs_comparison_by_permutation_plot.png"),
       objectives_plot_by_permutation, device = 'png', width = 6, height = 4)

objectives_plot_by_strategy <- ggplot(all_objs_probs) +
  geom_point(aes(y = erad_prob, x = total_suppress_prob, color = permutation)) +
  facet_wrap(vars(strategy)) +
  theme_bw()+
  labs(x = "Probability of upper 3 size class suppression", y = "Probability of full eradication")

ggsave(filename = paste0(save_folder, "_objs_comparison_by_strategy_plot.png"),
       objectives_plot_by_strategy, device = 'png', width = 6, height = 4)

##### Cost calculations for strategies (dynamic strategies use means across variants) #####
indiv_cost_categories <- c("transects", erad_methods)

mean_permutation_costs <- list()
# For each permutation
for(permutation_name in permutations) {
  strategy_costs <- list()
  # For strategy one (static strategy)
  strategy_costs$Strategy_one <- as.data.frame(matrix(NA, nrow = length(indiv_cost_categories), 
                                                      ncol = 2))
  colnames(strategy_costs$Strategy_one) <- c("method", "mean_cost")
  strategy_costs$Strategy_one$method <- indiv_cost_categories
  strategy_costs$Strategy_one$mean_cost <- 0
  strategy_costs$Strategy_one$mean_cost
  strategy_costs$Strategy_one$mean_cost[strategy_costs$Strategy_one$method == "ADS"] <- permutation_costs[[permutation_name]]$Strategy_one$total_cost$ADS
  
  ### For strategy 2 - using max cost, because its really not a huge difference between min and max. 
  # If the totals are close enough to another strategy that it might affect the optimization, 
  # then I'll reconsider how to do this
  strategy <- strategies[2]
  summed_costs <- as.data.frame(matrix(NA, nrow = num_variants, ncol = 6))
  colnames(summed_costs) <- c("variant",indiv_cost_categories)
  summed_costs$variant <- c(1:num_variants)
  for(variant in 1:num_variants) {
    # Combine all costs for each variant
    variant_costs <- permutation_costs[[permutation_name]][[strategy]][permutation_costs[[permutation_name]][[strategy]]$variant == variant,]
    # Combine costs for each method for each variant
    for(method in indiv_cost_categories) {
      summed_costs[[method]][variant] <- sum(variant_costs$max_dollars[variant_costs$method == method])
    }
  }
  # Calculate mean costs for each strategy (across variants)
  strategy_costs[[strategy]] <- as.data.frame(matrix(NA, nrow = length(indiv_cost_categories), 
                                                                ncol = 2))
  colnames(strategy_costs[[strategy]]) <- c("method", "mean_cost")
  strategy_costs[[strategy]]$method <- indiv_cost_categories
  for(method in 1:length(indiv_cost_categories)) {
    strategy_costs[[strategy]]$mean_cost[method] <- mean(summed_costs[[indiv_cost_categories[method]]])
  }
  
  # For strategies 3 and 4
  for(strategy in strategies[c(3:4)]) {
    summed_costs <- as.data.frame(matrix(NA, nrow = num_variants, ncol = 6))
    colnames(summed_costs) <- c("variant", indiv_cost_categories)
    summed_costs$variant <- c(1:num_variants)
    for(variant in 1:num_variants) {
      # Combine all costs for each variant
      variant_costs <- permutation_costs[[permutation_name]][[strategy]][permutation_costs[[permutation_name]][[strategy]]$variant == variant,]
      # summed_costs$summed_cost[variant] <- sum(variant_costs$dollars)
      # Combine costs for each method for each variant
      for(method in indiv_cost_categories) {
        summed_costs[[method]][variant] <- sum(variant_costs$dollars[variant_costs$method == method])
      }
    }
    # Calculate mean costs for each strategy (across variants)
    strategy_costs[[strategy]] <- as.data.frame(matrix(NA, nrow = length(indiv_cost_categories), 
                                                                  ncol = 2))
    colnames(strategy_costs[[strategy]]) <- c("method", "mean_cost")
    strategy_costs[[strategy]]$method <- indiv_cost_categories
    for(method in 1:length(indiv_cost_categories)) {
      strategy_costs[[strategy]]$mean_cost[method] <- mean(summed_costs[[indiv_cost_categories[method]]])
    }
    #strategy_costs[[strategy]]$summed_cost <- mean(summed_costs$summed_cost)
  }
  
  mean_permutation_costs[[permutation_name]] <- strategy_costs
}
# Combining above into one dataframe with all mean costs
all_mean_costs <- melt(mean_permutation_costs, id.vars = colnames(mean_permutation_costs[[1]][[1]]))
colnames(all_mean_costs)[c(3:4)] <- c("strategy", "permutation")

# Combining summed costs and objectives for plotting
costs_vs_obj_probs <- all_objs_probs
for(permutation_name in permutations) {
  for(strategy in strategies) {
    perm_strat_costs <- all_mean_costs[all_mean_costs$strategy == strategy & 
                                         all_mean_costs$permutation == permutation_name,]
    costs_vs_obj_probs$mean_cost[costs_vs_obj_probs$strategy == strategy & costs_vs_obj_probs$permutation == permutation_name] <- sum(perm_strat_costs$mean_cost)
  }
}

# Summarize the average quarter when objectives were achieved, if they were
obj_probs_melted <- melt(objectives_probs, colnames(objectives_probs[[1]]))
colnames(obj_probs_melted)[8] <- "permutation"
obj_reached <- list()
for(strategy in strategies) {
  obj_reached[[strategy]]$erad_quarter <- summary(obj_probs_melted$mean_erad_quarter[which(obj_probs_melted$strategy == strategy)])
  obj_reached[[strategy]]$total_suppress_quarter <- summary(obj_probs_melted$mean_total_quarter[which(obj_probs_melted$strategy == strategy)])
  obj_reached[[strategy]]$upper_3_quarter <- summary(obj_probs_melted$mean_upper_3_quarter[which(obj_probs_melted$strategy == strategy)])
}



##### Calculate true threshold conditions #####
# Using the IBM data, calculate what threshold condition should have been triggered, then compare to what occurred 
# using estimation
true_conditions <- list()
# Strategy two:
true_conditions$Strategy_two <- list()
for(permutation in permutations) {
  true_conditions$Strategy_two[[permutation]] <- list()
  for(variant in 1:num_variants) {
    data <- all_data[which(all_data$variant == variant & 
                             all_data$permutation == permutation & 
                             all_data$Strategy == strategies[2]),]
    true_conditions$Strategy_two[[permutation]][[paste0("variant_", variant)]] <- strat_2_IBM_threshold_fun(data)
  }
}

# Strategy three:
true_conditions$Strategy_three <- list()
for(permutation in permutations) {
  true_conditions$Strategy_three[[permutation]] <- list()
  for(variant in 1:num_variants) {
    data <- all_data[which(all_data$variant == variant & 
                             all_data$permutation == permutation & 
                             all_data$Strategy == strategies[3]),]
    true_conditions$Strategy_three[[permutation]][[paste0("variant_", variant)]] <- strat_3_IBM_threshold_fun(data)
  }
}


# Strategy four:
true_conditions$Strategy_four <- list()
for(permutation in permutations) {
  true_conditions$Strategy_four[[permutation]] <- list()
  for(variant in 1:num_variants) {
    data <- all_data[which(all_data$variant == variant & 
                             all_data$permutation == permutation & 
                             all_data$Strategy == strategies[4]),]
    true_conditions$Strategy_four[[permutation]][[paste0("variant_", variant)]] <- strat_4_IBM_threshold_fun(data)
  }
}

# Melting list into a single dataframe
true_condition_df <- melt(true_conditions)
colnames(true_condition_df)[3:5] <- c("variant", "permutation", "strategy")

#### Removed snakes by method ####

observed_methods <- c("visual", "trap")

# Import removal data from visual and trap methods
observed_name_list <- c("boiga_observed.RDS",
                        "delphine_observed.RDS",
                        "loon_observed_1.RDS",
                        "loon_observed_2.RDS",
                        "ursus_observed_1.RDS",
                        "ursus_observed_2.RDS")
observed_list <- list()
for(file_name in observed_name_list) {
  observed_list[[file_name]] <- readRDS(here("Results/alt_strategies/observed", file_name))
}
observed_df <- melt(observed_list, id.vars = colnames(observed_list[[1]][[1]]))

# Calculate mean, max and min for each strategy with each permutation
observed_summary <- list()
for(strategy in strategies[-1]) {
  observed_summary[[strategy]] <- list()
  for(permutation in permutations) {
    observed_summary[[strategy]][[permutation]] <- list()
    for(variant in paste0("variant_", c(1:50))) {
      observed_summary[[strategy]][[permutation]][[variant]] <- list()
      for(quarter in 1:40) {
        df <- as.data.frame(matrix(NA, nrow = 5, ncol = 3))
        colnames(df) <- c("size_class", "visual", "trap")
        df$size_class <- c(size_class_names, "total")
        for(size in size_class_names) {
          for(method in c("visual", "trap")) {
            df[which(df$size_class == size), method] <-  nrow(observed_df[which(observed_df$Strategy == strategy & 
                                                                                  observed_df$permutation == permutation &
                                                                                  observed_df$Quarter == quarter &
                                                                                  observed_df$variant == variant &
                                                                                  observed_df$method == method &
                                                                                  observed_df$size_category == size),])
            df[which(df$size_class == "total"), method] <- sum(df[which(df$size_class != "total"), method])
          }
        }
        observed_summary[[strategy]][[permutation]][[variant]][[quarter]] <- df
      }
    }
  }
}

observed_summary_df <- melt(observed_summary, id.vars = colnames(observed_summary[[1]][[1]][[1]][[1]]))
colnames(observed_summary_df)[c(4:7)] <- c("Quarter", "variant", "permutation", "strategy")

# Separating out totals, and calculating mean, min and max 
total_observed <- observed_summary_df[which(observed_summary_df$size_class == "total"),]

total_observed_summary <- list()
for(strategy in strategies[-1]) {
  total_observed_summary[[strategy]] <- list()
  for(permutation in permutations) {
    total_observed_summary[[strategy]][[permutation]] <- list()
    variants_data <- total_observed[which(total_observed$strategy == strategy &
                                            total_observed$permutation == permutation), ]
    if(strategy == strategies[3]) {
      strategy_methods <- "visual"
    } else {
      strategy_methods <- observed_methods
    }
    
    for(quarter in 1:max(variants_data$Quarter)) {
      df <- as.data.frame(matrix(NA, nrow = length(strategy_methods), ncol = 5))
      colnames(df) <- c("method", "mean", "median", "min", "max")
      df$method <- strategy_methods
      for(method in 1:length(strategy_methods)) {
        method_data <- variants_data[which(variants_data$Quarter == quarter), observe_methods[method]]
        df$mean[method] <- summary(method_data)[c("Mean")]
        df$median[method] <- summary(method_data)[c("Median")]
        df$min[method] <- summary(method_data)[c("Min.")]
        df$max[method] <- summary(method_data)[c("Max.")]
      }
      total_observed_summary[[strategy]][[permutation]][[quarter]] <- df
    }
  }
}
total_observed_summary_df <- melt(total_observed_summary, id.vars = colnames(total_observed_summary[[1]][[1]][[1]]))
colnames(total_observed_summary_df)[c(6:8)] <- c("Quarter", "permutation", "Strategy")
# Fixing factoring in dataframe
total_observed_summary_df$Strategy <- factor(total_observed_summary_df$Strategy, levels = strategies[-1])
total_observed_summary_df$permutation <- factor(total_observed_summary_df$permutation, levels = c("low_more_small",
                                                                                                  "medium_more_small",
                                                                                                  "high_more_small",
                                                                                                  "low_more_xlarge",
                                                                                                  "medium_more_xlarge",
                                                                                                  "high_more_xlarge"))
# Creating another column so geom_path will isolate each variant in each strategy
total_observed_summary_df <- mutate(total_observed_summary_df, variant_permutation = paste0(permutation, "_",
                                                                                            variant))


# plot removal data
removed_snakes_plot <- ggplot(total_observed_summary_df,
                              aes(x = Quarter/4, y = mean, color = method)) +
  geom_point() + 
  geom_errorbar(aes(ymin = min, ymax = max)) +
  facet_grid(rows = vars(permutation), cols = vars(Strategy), scales = "free_y",
             labeller = labeller(permutation = plot_labels$permutation, Strategy = plot_labels$strategy[-1])) +
  theme_bw() +
  labs(y = "Removed snakes", color = "Removal method", x = "Year")

ggsave(filename = paste0(save_folder, "removed_snakes_plot.png"),
        removed_snakes_plot, device = 'png', width = 8, height = 8.2)

#### Figures & Tables ####

##### Objectives vs cost plots  #####

# Eradication probabilty vs cost plots
cost_vs_erad_objs_plot_by_permutation <- ggplot(costs_vs_obj_probs) +
  geom_point(aes(y = erad_prob, x = mean_cost/1000000, color = strategy)) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(vars(permutation), 
             labeller = labeller(permutation = plot_labels$permutation)) +
  theme_bw()+
  scale_color_manual(values = plot_colors$strategy,
                     labels = plot_labels$strategy) +
  theme(legend.position="bottom") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.75)) +
  labs(x = "Projected mean cost (dollars in millions)", 
       y = "Probability of full eradication",
       color = "Strategy")

ggsave(filename = paste0(save_folder, "costs_vs_erad_obj_comparison_by_permutation_plot.png"),
       cost_vs_erad_objs_plot_by_permutation, device = 'png', width = 7, height = 4)

cost_vs_erad_objs_plot_by_strategy <- ggplot(costs_vs_obj_probs) +
  geom_point(aes(y = erad_prob, x = mean_cost/1000000, color = permutation)) +
  scale_y_continuous(limits = c(-0.01,1)) +
  scale_color_manual(values = hue_pal()(6), labels = plot_labels$permutation) +
  facet_wrap(vars(strategy), labeller = labeller(strategy = plot_labels$strategy)) +
  theme_bw()+
  labs(x = "Projected mean cost (dollars in millions)", 
       y = "Probability of full eradication",
       color = "Starting condition")

ggsave(filename = paste0(save_folder, "costs_vs_erad_obj_comparison_by_strategy_plot.png"),
       cost_vs_erad_objs_plot_by_strategy, device = 'png', width = 6, height = 4)

# Upper 3 size class suppression probabilty vs cost plots
cost_vs_upper_3_objs_plot_by_permutation <- ggplot(costs_vs_obj_probs) +
  geom_point(aes(y = upper_3_suppress_prob, x = mean_cost/1000000, color = strategy)) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(vars(permutation), 
             labeller = labeller(permutation = plot_labels$permutation)) +
  scale_color_manual(values = plot_colors$strategy,
                     labels = plot_labels$strategy) +
  theme_bw()+
  theme(legend.position="bottom") +
  labs(x = "Projected mean cost (dollars in millions)", 
       y = "Probability of suppression upper 3 size classes",
       color = "Strategy")

ggsave(filename = paste0(save_folder, "costs_vs_upper_3_obj_comparison_by_permutation_plot.png"),
       cost_vs_upper_3_objs_plot_by_permutation, device = 'png', width = 7, height = 4)

cost_vs_upper_3_objs_plot_by_strategy <- ggplot(costs_vs_obj_probs) +
  geom_point(aes(y = upper_3_suppress_prob, x = mean_cost/1000000, color = permutation)) +
  scale_y_continuous(limits = c(-0.01,1)) +
  scale_color_manual(values = hue_pal()(6), labels = plot_labels$permutation) +
  facet_wrap(vars(strategy), labeller = labeller(strategy = plot_labels$strategy)) +
  theme_bw()+
  labs(x = "Projected mean cost (dollars in millions)", 
       y = "Probability of suppression upper 3 size classes",
       color = "Starting condition")

ggsave(filename = paste0(save_folder, "costs_vs_upper_3_obj_comparison_by_strategy_plot.png"),
       cost_vs_upper_3_objs_plot_by_strategy, device = 'png', width = 6, height = 4)

# Total suppression probabilty vs cost plots
cost_vs_total_supp_objs_plot_by_permutation <- ggplot(costs_vs_obj_probs) +
  geom_point(aes(y = total_suppress_prob, x = mean_cost/1000000, color = strategy)) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(vars(permutation), 
             labeller = labeller(permutation = plot_labels$permutation)) +
  scale_color_manual(values = plot_colors$strategy,
                     labels = plot_labels$strategy) +
  theme_bw()+
  theme(legend.position="bottom") +
  labs(x = "Projected mean cost (dollars in millions)", 
       y = "Probability of total population suppression",
       color = "Strategy")

ggsave(filename = paste0(save_folder, "costs_vs_total_supp_obj_comparison_by_permutation_plot.png"),
       cost_vs_total_supp_objs_plot_by_permutation, device = 'png', width = 7, height = 4)

cost_vs_total_supp_objs_plot_by_strategy <- ggplot(costs_vs_obj_probs) +
  geom_point(aes(y = total_suppress_prob, x = mean_cost/1000000, color = permutation)) +
  scale_y_continuous(limits = c(-0.01,1)) +
  facet_wrap(vars(strategy), labeller = labeller(strategy = plot_labels$strategy)) +
  scale_color_manual(values = hue_pal()(6), labels = plot_labels$permutation) +
  theme_bw()+
  labs(x = "Projected mean cost (dollars in millions)", 
       y = "Probability of total population suppression",
       color = "Starting condition")

ggsave(filename = paste0(save_folder, "costs_vs_total_supp_obj_comparison_by_strategy_plot.png"),
       cost_vs_total_supp_objs_plot_by_strategy, device = 'png', width = 6, height = 4)



#### Plots of methods & costs ####


##### Method combinations in each threshold condition for each strategy #####
# in alternative_strategies_graphics script

##### Actual methods used across replicates for each dynamic strategy #####
all_conditions <- melt(conditions_list, id.vars = colnames(conditions_list[[1]]$Strategy_two))
colnames(all_conditions)[c(4:5)] <- c("strategy", "permutation")
all_conditions$permutation <- factor(all_conditions$permutation, levels = c("low_more_small",
                                                                            "medium_more_small",
                                                                            "high_more_small",
                                                                            "low_more_xlarge",
                                                                            "medium_more_xlarge",
                                                                            "high_more_xlarge"))
all_conditions$strategy <- factor(all_conditions$strategy, levels = strategies)
# Removing the single threshold_3 value (it wasn't applied, the final snake died of natural mortality)
remove_ind <- which(all_conditions$condition == "threshold_3")
all_conditions <- all_conditions[-remove_ind,]
# Setting NA values to initial - there's a chance it coud have been threshold 2, but its unlikely
NA_inds <- which(is.na(all_conditions$condition))
all_conditions$condition[NA_inds] <- "initial"

condition_type <- c("initial" = "Initial", "threshold_1" = "Threshold 1", "threshold_2" = "Threshold 2")

condition_plot <- ggplot(all_conditions) +
  geom_bar(aes(y = set, fill = condition)) +
  facet_grid(rows = vars(strategy), cols = vars(permutation), scales = "free_y",
             labeller = labeller(permutation = plot_labels$permutation, 
                                 strategy = plot_labels$strategy)) +
  scale_fill_manual(values = hue_pal()(3), labels = condition_type) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Estimation interval", x = "Replicate", fill = "Methods condition")

ggsave(filename = paste0(save_folder, "all_conditions_plot.png"),
       condition_plot, device = 'png', width = 8.2, height = 5.5)

# Comparison of what the condition should be (based on IBM data) and what the estimation directed
# Combine all conditions
true_condition_df$source <- "IBM"
all_conditions$source <- "Estimation"
combined_conditions <- rbind(true_condition_df, all_conditions)

# Adding column to estimation conditions df to indicate if the estimation was correct
all_conditions$accurate <- NA
for(strategy in strategies[c(2:4)]) {
  for(permutation in permutations) {
    for(variant in paste0("variant_", c(1:50))) {
      data <- all_conditions[which(all_conditions$strategy == strategy &
                                     all_conditions$permutation == permutation &
                                     all_conditions$variant == variant),]
      sets <- data$set
      for(set in sets) {
        true_condition <- true_condition_df$condition[which(true_condition_df$strategy == strategy &
                                                              true_condition_df$permutation == permutation &
                                                              true_condition_df$variant == variant &
                                                              true_condition_df$set == set)]
        estimated_condition <- all_conditions$condition[which(all_conditions$strategy == strategy &
                                                               all_conditions$permutation == permutation &
                                                               all_conditions$variant == variant &
                                                               all_conditions$set == set)]
        if(is.na(estimated_condition) == TRUE) {
          estimated_condition <- "initial"
        }
        if(estimated_condition == "threshold_3") { # Only 1 variant has this, and it'll be removed later
          next()
        }
        
        if(estimated_condition == true_condition) {
          all_conditions$accurate[which(all_conditions$strategy == strategy &
                                          all_conditions$permutation == permutation &
                                          all_conditions$variant == variant &
                                          all_conditions$set == set)] <- "TRUE"
        } else {
          all_conditions$accurate[which(all_conditions$strategy == strategy &
                                          all_conditions$permutation == permutation &
                                          all_conditions$variant == variant &
                                          all_conditions$set == set)] <- "FALSE"
        }
        
      }
    }
  }
}

# Plotting how often the estimation was correct about the threshold condition
condition_accuracy_plot <- ggplot(all_conditions) +
  geom_bar(aes(y = set, fill = accurate)) +
  facet_grid(rows = vars(strategy), cols = vars(permutation), scales = "free_y",
             labeller = labeller(permutation = plot_labels$permutation, 
                                 strategy = plot_labels$strategy)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Estimation interval", x = "Replicate", fill = "Condition accuracy")

ggsave(filename = paste0(save_folder, "condition_accuracy_plot.png"),
       condition_accuracy_plot, device = 'png', width = 8.2, height = 5.5)

# Calculate accuracy rate
condition_accuracy <- list()
for(strategy in strategies[c(2:4)]) {
  condition_accuracy[[strategy]] <- list()
  for(permutation in permutations) {
    condition_accuracy[[strategy]][[permutation]] <- list()
    data <- all_conditions[which(all_conditions$strategy == strategy &
                                   all_conditions$permutation == permutation),]
    sets <- unique(data$set)
    for(set in sets) { 
      condition_accuracy[[strategy]][[permutation]][[set]] <- nrow(data[which(data$set == set & data$accurate == TRUE),])/50
    }
  }
}
condition_accuracy_df <- melt(condition_accuracy)
colnames(condition_accuracy_df) <- c("accuracy", "set", "permutation", "strategy")
# Removing set 1, which is always initial
condition_accuracy_df <- condition_accuracy_df[which(condition_accuracy_df$set != 1),]
# Re-factor permutations and strategies for plotting
condition_accuracy_df$permutation <- factor(condition_accuracy_df$permutation, levels = c("low_more_small",
                                                                                          "low_more_xlarge",
                                                                                          "medium_more_small",
                                                                                          "medium_more_xlarge",
                                                                                          "high_more_small",
                                                                                          "high_more_xlarge"))
condition_accuracy_df$strategy <- factor(condition_accuracy_df$strategy, levels = strategies[-1])

condition_accuracy_rate_plot <- ggplot(condition_accuracy_df, aes(y = accuracy, x = set, color = permutation)) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(strategy), scales = "free_x",
             labeller = labeller(strategy = plot_labels$strategy)) +
  scale_color_manual(values = hue_pal()(6), labels = plot_labels$permutation) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(y = "Condition accuracy rate", x = "Estimation interval", color = "Starting condition")

ggsave(filename = paste0(save_folder, "condition_accuracy_rate_plot.png"),
       condition_accuracy_rate_plot, device = 'png', width = 9, height = 5.5)


# Check how often methods targeting large snakes are discontinued when they shouldn't have been (estimated threshold 
# 1, when it should have been initial or threshold 2)
thres_1_est <- all_conditions[which(all_conditions$condition == "threshold_1"),]
strategy_insufficient_effort <- vector()
for(strategy in 2:length(strategies)) {
  strategy_insufficient_effort[strategy] <- nrow(thres_1_est[which(thres_1_est$accurate == "FALSE" &
                                                          thres_1_est$strategy == strategies[strategy]),])/nrow(thres_1_est[which(thres_1_est$strategy == strategies[strategy]),])
}

no_thres_1_est <- all_conditions[which(all_conditions$condition != "threshold_1"),]
strategy_unnecessary_cost <- vector()
for(strategy in 2:length(strategies)) {
  strategy_unnecessary_cost[strategy] <- nrow(no_thres_1_est[which(no_thres_1_est$accurate == "FALSE" &
                                                                     no_thres_1_est$strategy == strategies[strategy]),])/nrow(no_thres_1_est[which(no_thres_1_est$strategy == strategies[strategy]),])
}

thres_2_est <- all_conditions[which(all_conditions$condition == "threshold_2"),]
nrow(thres_2_est[which(thres_2_est$accurate == "FALSE"),])/nrow(thres_2_est)


### When did replicates meet the objectives



##### Format dataframe for a plot of total costs for each strategy #####

### Below melts all of the variants' costs into a single dataframe, but I probably only 
### care about the mean cost per strategy, so using that instead for plotting
# dynamic_costs <- list()
# for(permutation in permutations) {
#   # Exclude min_dollars from strategy 2 and rename the column to match the others
#   strategy_two_costs <- permutation_costs[[permutation]]$Strategy_two[,-1]
#   colnames(strategy_two_costs)[1] <- "dollars"
#   strategy_two_costs$strategy <- strategies[2]
#   # Separate out strategy one and melt the other strategy costs
#   strategy_costs <- melt(permutation_costs[[permutation]][c(3:4)], 
#                          id.vars = colnames(permutation_costs[[1]][[3]]))
#   colnames(strategy_costs)[5] <- "strategy"
#   strategy_costs <- rbind(strategy_costs, strategy_two_costs)
#   dynamic_costs[[permutation]] <- strategy_costs
# }
# # Melt all permutations together
# all_dynamic_costs <- melt(dynamic_costs, id.vars = colnames(dynamic_costs[[1]]))
# colnames(all_dynamic_costs)[6] <- "permutation"

# Fixing factor levels for plotting
all_mean_costs$permutation <- factor(all_mean_costs$permutation, levels = c("low_more_small",
                                                                            "medium_more_small",
                                                                            "high_more_small",
                                                                            "low_more_xlarge",
                                                                            "medium_more_xlarge",
                                                                            "high_more_xlarge"))
all_mean_costs$strategy <- factor(all_mean_costs$strategy, levels = strategies)

mean_cost_by_method_strat_plot <- ggplot(all_mean_costs) +
  geom_bar(aes(y = mean_cost, x = strategy, fill = strategy), position="dodge", stat="identity") +
  facet_grid(rows = vars(method), cols = vars(permutation),
             labeller = labeller(method = c(plot_labels$method, "transects" = "Transects"), 
                                 permutation = plot_labels$permutation)) +
  scale_fill_manual(values = plot_colors$strategy,
                     labels = plot_labels$strategy) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Mean cost (dollars in millions)", x = "Strategy", fill = "") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(filename = paste0(save_folder, "mean_cost_by_method-strat_plot.png"),
       mean_cost_by_method_strat_plot, device = 'png', width = 8.2, height = 5.5)


mean_cost_by_method_perm_plot <- ggplot(all_mean_costs) +
  geom_bar(aes(y = mean_cost, x = permutation, fill = permutation), position="dodge", stat="identity") +
  facet_grid(rows = vars(method), cols = vars(strategy),
             labeller = labeller(method = c(plot_labels$method, "transects" = "Transects"), 
                                 strategy = plot_labels$strategy)) +
  scale_fill_manual(values = plot_colors$permutation,
                    labels = plot_labels$permutation) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(y = "Mean cost (dollars in millions)", x = "Permutation", fill = "") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(filename = paste0(save_folder, "mean_cost_by_method-perm_plot.png"),
       mean_cost_by_method_perm_plot, device = 'png', width = 8, height = 5.5)

#### Plots of accuracy & precision metrics (RMSE, PRD and coverage) & dataframe for table ####



