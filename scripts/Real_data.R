
rm(list=ls())
source("Parallel.R")
source("Data_functions.R")

# Define the input parameters
noise_fwhm_values <- seq(5,50,by=5)

vGRFPhan <- col_diff(vGRF_data_Phan("both"))
JCF <- col_diff(JCF_data("both"))
Angle <- col_diff(Angle_data("both"))
Moment <- -col_diff(Moment_data("both"))
MF <- -col_diff(MF_data("both"))
EMG <- -col_diff(EMG_data("both"))

# plot(vGRFPhan, main = "vGRFPhan",type = "l",ylim=c(-0.1,1.5))
# plot(JCF, main = "JCF",type = "l",ylim=c(0,1))
# plot(Angle, main = "Angle",type = "l",ylim=c(0,10))
# plot(-Moment, main = "Moment",type = "l",ylim=c(-1,1))
# plot(-MF, main = "MF",type = "l")
# plot(-EMG, main = "EMG", type = "l")


geom <- cbind(vGRFPhan, JCF, Angle, Moment, MF, EMG)
apply(geom, 2,max)
apply(geom, 2,min)

N_sim <- 2500
sample_size <- 10

methods <- c("Parametric_SPM", "Nonparametric_SPM")




results_list <- list() # To store Input_Summary for each simulation

for (g in 1:ncol(geom)){
  
  noise_sd_values <- max(geom[,g])
  
  for (noise_fwhm in noise_fwhm_values) {
    
    
    # Print current loop status
    # cat("Geom",colnames(geom)[g],
    #   "Noise FWHM:", noise_fwhm, "\n")
    
    
    # Initialize sample size and track methods that have met power threshold
    
    methods_to_run <- methods # Start with all methods
    
    
    # Run the Power_parallel function with the current sample size
    power_results <- Power_parallel(
      data = NULL, # Replace `your_data` with the actual data variable
      sample_size = sample_size,
      noise_mean = 0, # Assuming noise_mean is set to 0, modify as needed
      noise_sd = noise_sd_values,
      noise_fwhm = noise_fwhm,
      signal = geom[,g], # Adjust signal as required
      method = methods_to_run,
      n_iterations = N_sim # Set appropriate iteration count
    )
    
    # Extract power results for each method
    power_result_summary <- power_results$Power_results
    
    # Add input summary to results list for tracking
    results_list[[paste("Geom",colnames(geom)[g],"Noise_FWHM", noise_fwhm, sep = "_")]] <- power_results$Input_Summary
    
    
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
combined_results <- cbind(combined_results, geom = rep(colnames(geom),each=length(noise_fwhm_values)))

#write.xlsx(combined_results, "/Users/more0056/Desktop/temp_file/Simulation_Input_Summary.xlsx")

#############################################

