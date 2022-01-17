# Function for calculating ATE by bootstrapping
# data: a dataframe. 
# outcome: a string of the outcome variable name.
# treatment: a string of the binary treatment variable name.
# control: string or numeric value of the control group in the treament variable.
# boot: an integer for the number of simulations
# conf.level: confidence level of the interval.

ATE.boot <- function(data, outcome, treatment, control, boot = 1000, conf.level = .95) {
  
  temp.df <- data[c(outcome, treatment)]
  names(temp.df) <- c("outcome", "treatment")
  
  # Generate the label of the treated group
  treated <- unique(temp.df$treatment[temp.df$treatment != control])
  
  bootResult <- data.frame(matrix(NA, nrow = boot, ncol = 4))
  names(bootResult) <- c("control", "n_control", "treated", "n_treated")
  
  for (i in 1:boot) {
    d <- temp.df[sample(nrow(temp.df), nrow(temp.df), replace = T), ]
    
    bootResult$control[i] <- mean(d$outcome[d$treatment == control], na.rm = T)
    bootResult$n_control[i] <- length(d$outcome[d$treatment == control & !is.na(d$outcome)])
    
    bootResult$treated[i] <- mean(d$outcome[d$treatment == treated], na.rm = T)
    bootResult$n_treated[i] <- length(d$outcome[d$treatment == treated & !is.na(d$outcome)])
  }
  
  bootResult$diff <- bootResult$treated - bootResult$control
  
  observedDiff <- mean(temp.df$outcome[temp.df$treatment == treated], na.rm = T) - 
    mean(temp.df$outcome[temp.df$treatment == control], na.rm = T)
  
  output <- cbind.data.frame(
    est_control = mean(bootResult$control, na.rm = T),
    est_treated = mean(bootResult$treated, na.rm = T),
    diff = mean(bootResult$diff, na.rm = T),
    conf.low = quantile(bootResult$diff, (1 - conf.level) / 2, na.rm = T),
    conf.high = quantile(bootResult$diff, 1 - (1 - conf.level) / 2, na.rm = T)
  )
  
  rownames(output) <- NULL
  
  return(list(bootResult = bootResult, 
              test = output))
  
}
