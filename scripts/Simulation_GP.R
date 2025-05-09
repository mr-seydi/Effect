rm(list=ls())
source("Parallel.R")


# Define the input parameters
noise_fwhm_values <- seq(5,50,by=5)

# pulse fwhm
pfwhm <- seq(5,100,by=5)

N_sim <- 2500
sample_size <- 10
noise_sd_values <- 1
methods <- c("Parametric_SPM", "Nonparametric_SPM")




results_list <- list() # To store Input_Summary for each simulation

for(p_range in 1:length(pfwhm)){
  
  #generate pulse
  Gauss_P <- gaussian_pulse(center=50, fwhm=pfwhm[p_range], continuum_size=101)
  Pulse <- amplitude_pulse(Gauss_P$density_val, amp=1)
  
  for (noise_fwhm in noise_fwhm_values) {
    
    
    # Print current loop status
    cat("Pulse FWHM", pfwhm[p_range], "Noise FWHM:", noise_fwhm, "\n")
    
    
    # Initialize sample size and track methods that have met power threshold
    
    methods_to_run <- methods # Start with all methods
    
    
    # Run the Power_parallel function with the current sample size
    power_results <- Power_parallel(
      data = NULL, # Replace `your_data` with the actual data variable
      sample_size = sample_size,
      noise_mean = 0, # Assuming noise_mean is set to 0, modify as needed
      noise_sd = noise_sd_values,
      noise_fwhm = noise_fwhm,
      signal = Pulse, # Adjust signal as required
      method = methods_to_run,
      n_iterations = N_sim # Set appropriate iteration count
    )
    
    # Extract power results for each method
    power_result_summary <- power_results$Power_results
    
    # Add input summary to results list for tracking
    results_list[[paste("Pulse FWHM", pfwhm[p_range],
                        "Noise_FWHM", noise_fwhm, sep = "_")]] <- power_results$Input_Summary
    
    
  }
}


# Save or inspect results_list for tracking after simulation completes
# For example, you can write results to an Excel file or analyze them further
# Write to Excel if needed
copy_results<-results_list
# copy_results has different number of columns, so we need to make them equal and then rbind the results
#rbind the results
# Load the necessary package
library(plyr)
library(openxlsx)

# Ensure each data frame has the same columns and combine them
combined_results <- rbind.fill(copy_results)

#write.xlsx(combined_results, "/Users/more0056/Desktop/temp_file/Simulation_Input_Summary.xlsx")

#############################################

