
rm(list=ls())
source("R/sources.R")

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
#apply(geom, 2,max)
#apply(geom, 2,min)

N_sim <- 10000
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
      n_iterations = N_sim, # Set appropriate iteration count
      Write_file = TRUE,
      file_name = paste("Outputs/RealData/data", colnames(geom)[g],
                        "NoiseFWHM", noise_fwhm, sep = "_")
    )
    # remove and use gc()
    rm(power_results)
    gc()
    
  }
}






