## Threshold function to use with IBM data to determine what the true conditions are


## Function to evaluate IBM results given threshold of 1/10 snakes per ha over 1,150 mm SVL (x-large size )
IBM_threshold_fun <- function(data,
                              xlarge_threshold = 0.1,
                              area = area_size,
                              size_class = size_class_names) {
  conditions <- vector()
  # Condition (default is initial)
  conditions[c(1:4)] <- "initial"
  # Adding a column with density
  data$density <- data$N/area
  # Determine what the final quarter for this variant was:
  final_quarter <- max(data$Quarter)
  # Check if the final quarter is less than 40, and if so identify when the next estimation round would have occurred
  if(final_quarter < 40) {
    possible_sets <- seq(4, 40, 4)
    final_quarter <- possible_sets[which(possible_sets > final_quarter)][1]
  }
  # Determine which condition should have been triggered after each estimation step (every 2 quarters)
  for(quarter in seq(4, final_quarter-4, 4)) {
    # Separating final time step
    last_quarter_data <- data[which(data$Quarter == quarter & data$size_class != "total"), ]
    ## Calculating X-large size class density
    xlarge_density <- last_quarter_data$N[last_quarter_data$size_class == size_class[4]]/area
    
    ## Threshold 1: Estimated mean density is of x-large size class is <=0.01 snake/ha 
    if(xlarge_density <= xlarge_threshold) {
      conditions[c((quarter+1):(quarter+4))] <- "threshold_1"
    } 
    
    ## If threshold isn't triggered, go back to initial
    if (xlarge_density > xlarge_threshold) {
      conditions[c((quarter+1):(quarter+4))] <- "initial"
    }
  }
  conditions <- conditions[seq(1,final_quarter,4)]
  condition_by_set <- as.data.frame(cbind(conditions, c(1:(final_quarter/4))))
  colnames(condition_by_set) <- c("condition", "set")
  
  return(condition_by_set)
}


