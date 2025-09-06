
rm(list=ls())
source("Parallel.R")
# Define the input parameters
noise_fwhm_values <- c(5, 25, 50)
# pulse
centered_r <- centered_ranges(c(5, 50, 100))

N_sim <- 2500
sample_size <- 10
noise_sd_values <- 1
methods <- c("Parametric_SPM", "Nonparametric_SPM")

for(p_range in 1:nrow(centered_r)){
  
  #generate pulse
  # Pulse <- square_pulse(start_end_pulse = c(centered_r$Start[p_range],
  #                                           centered_r$End[p_range]),
  #                       start_height = 0, pulse_height = 1)
  
  for (noise_fwhm in noise_fwhm_values) {
    
    file_name = paste0("Power_Results_","percentage",
                       centered_r$Percentage[p_range], "_NoiseFWHM",
                       noise_fwhm)
    library(readxl)
    assign(x = file_name,
           value = read_excel(paste0("Outputs/pvalues/", file_name, ".xlsx")))
    
  }
}



#####pvalue function######
set.seed(123)
smp <- sample(1:N_sim, 10)
par(mfrow = c(3, 3))

for(p_range in 1:nrow(centered_r)) {
  for (noise_fwhm in noise_fwhm_values) {
    file_name <- paste0("Power_Results_",
                        "percentage", centered_r$Percentage[p_range],
                        "_NoiseFWHM", noise_fwhm)
    
    # retrieve the data frame by name:
    this_df <- get(file_name)
    
    
    # In each simulation, how many points is rejected
    #apply(this_df, 2, function(x) sum(x < 0.05))
    
    # power
    power <- mean(apply(this_df, 2, function(x) sum(x < 0.05)) >= 1)
    
    # plot the selected columns (time 0:100 in rows; sims in columns)
    matplot(x    = 0:100,
            y    = this_df[, smp],      # transpose so each sim is a line
            type = "l",
            lty = 1,
            lwd = 2,
            xlab = "Domain",
            ylab = "p-value",
            ylim = c(0, 1),
            main = paste0("percentage", centered_r$Percentage[p_range]," ",
                                 "NoiseFWHM", noise_fwhm, ", power = ", round(power, 2))
    )
    abline(h = 0.05, col = "red", lty = 2, lwd = 2)
  }
}



####percentage of rejection at each point####

par(mfrow = c(3, 3))


for(p_range in 1:nrow(centered_r)) {
  for (noise_fwhm in noise_fwhm_values) {
    file_name <- paste0("Power_Results_",
                        "percentage", centered_r$Percentage[p_range],
                        "_NoiseFWHM", noise_fwhm)
    
    # retrieve the data frame by name:
    this_df <- get(file_name)
    
    # power
    power <- mean(apply(this_df, 2, function(x) sum(x < 0.05)) >= 1)
    
    # plot the selected columns (time 0:100 in rows; sims in columns)
    matplot(x    = 0:100,
            y    = apply(this_df, 1, function(x) mean(x < 0.05)),      # transpose so each sim is a line
            type = "l",
            lwd = 2,
            xlab = "Domain",
            ylab = "Percentage",
            main = paste0("percentage", centered_r$Percentage[p_range]," ",
                          "NoiseFWHM", noise_fwhm, ", power = ", round(power, 2) ),
            ylim = c(0, 1)
    )
  }
}
  


#### power and roi power ####


for(p_range in 1:nrow(centered_r)) {
  for (noise_fwhm in noise_fwhm_values) {
    file_name <- paste0("Power_Results_",
                        "percentage", centered_r$Percentage[p_range],
                        "_NoiseFWHM", noise_fwhm)
    
    # retrieve the data frame by name:
    this_df <- get(file_name)
    
    # power
    power <- mean(apply(this_df, 2, function(x) sum(x < 0.05)) >= 1)
    
    # power restricted to region of interest
    signal <- square_pulse(start_end_pulse = c(centered_r$Start[p_range],
                                              centered_r$End[p_range]),
                          start_height = 0, pulse_height = 1)
    roi <- which(signal!= 0)
    power_roi <- mean(apply(this_df[roi, ], 2, function(x) sum(x < 0.05)) >= 1)
    print(c(power,power_roi))
    

  }
}



#### histogram min p-value of each functions  ####

par(mfrow = c(3, 3))

for(p_range in 1:nrow(centered_r)) {
  for (noise_fwhm in noise_fwhm_values) {
    file_name <- paste0("Power_Results_",
                        "percentage", centered_r$Percentage[p_range],
                        "_NoiseFWHM", noise_fwhm)
    
    # retrieve the data frame by name:
    this_df <- get(file_name)
    
    # power
    power <- mean(apply(this_df, 2, function(x) sum(x < 0.05)) >= 1)
    
    min_pval <- as.numeric(apply(this_df, 2, min))
    
    hist(min_pval,
         breaks = seq(0, 1, by = 0.05),
         main = paste0("percentage", centered_r$Percentage[p_range]," ",
                       "NoiseFWHM", noise_fwhm, ", power = ", round(power, 2) ),
         xlab = "p-value",
         ylab = "Frequency",
         xlim = c(0, 1),
         col = "lightblue",
         ylim = c(0, 2500))
    
    
  }
}


####how many points is rejected for each curve####


for(p_range in 1:nrow(centered_r)) {
  for (noise_fwhm in noise_fwhm_values) {
    file_name <- paste0("Power_Results_",
                        "percentage", centered_r$Percentage[p_range],
                        "_NoiseFWHM", noise_fwhm)
    
    # retrieve the data frame by name:
    this_df <- get(file_name)
    
    
    rejected_howmany <- apply(this_df, 2, function(x) sum(x < 0.05))
    
    cat("percentage", centered_r$Percentage[p_range],
        "NoiseFWHM", noise_fwhm, "\n")
    print(table(rejected_howmany))
    
    
  }
}


  