
rm(list=ls())
source("Parallel.R")


# Define the input parameters
noise_fwhm_values <- seq(5,50,by=5)

# pulse
centered_ranges <- centered_ranges(seq(5, 100, by = 5))
five_percent <- centered_ranges[centered_ranges$Percentage == 5,]
fifty_percent <- centered_ranges[centered_ranges$Percentage == 50,]
hundred_percent <- centered_ranges[centered_ranges$Percentage == 100,]
percentage_ranges <- rbind(five_percent, fifty_percent, hundred_percent)

N_sim <- 2500
sample_size <- 10
noise_sd_values <- 1
methods <- c("Parametric_SPM", "Nonparametric_SPM")




results_list <- list() # To store Input_Summary for each simulation

for(p_range in 1:nrow(percentage_ranges)){
  
  
  for(height in seq(0.1,1,by=0.1)){
    
    #generate pulse
    Pulse <- square_pulse(start_end_pulse = c(percentage_ranges$Start[p_range],
                                              percentage_ranges$End[p_range]),
                          start_height = 0, pulse_height = height)
    
    for (noise_fwhm in noise_fwhm_values) {
      
      
      # Print current loop status
      cat("Pulse percentage", percentage_ranges$Percentage[p_range], "Pulse height", height,"Noise FWHM:", noise_fwhm,"\n")
      
      
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
      results_list[[paste("Pulse percentage", percentage_ranges$Percentage[p_range],
                          "Pulse height", height,
                          "Noise_FWHM", noise_fwhm, sep = "_")]] <- power_results$Input_Summary
      
      
    }
    
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

percentage_domain <- rep(percentage_ranges$Percentage, each=length(seq(0.1,1,by=0.1))*length(noise_fwhm_values))
height_domain <- rep(seq(0.1,1,by=0.1), each=length(noise_fwhm_values))
combined_results <- cbind(combined_results,percentage_domain,height_domain)
#write.xlsx(combined_results, "/Users/more0056/Desktop/temp_file/Simulation_Input_Summary.xlsx")

#############################################

