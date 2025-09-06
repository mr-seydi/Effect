rm(list=ls())
source("R/sources.R")


# Define the input parameters
noise_fwhm_values <- seq(5,50,by=5)

# pulse fwhm
pfwhm <- seq(5,100,by=5)

N_sim <- 10000
sample_size <- 10
noise_sd_values <- 1
methods <- c("Parametric_SPM", "Nonparametric_SPM")





for(p_range in 1:length(pfwhm)){
  
  #generate pulse
  Gauss_P <- gaussian_pulse(center=50, fwhm=pfwhm[p_range], continuum_size=101)
  Pulse <- amplitude_pulse(Gauss_P$density_val, amp=1)
  
  for (noise_fwhm in noise_fwhm_values) {
    
    
    # Print current loop status
    #cat("Pulse FWHM", pfwhm[p_range], "Noise FWHM:", noise_fwhm, "\n")
    
    
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
      n_iterations = N_sim, # Set appropriate iteration count
      domain_points = 1001, # number of evaluation points
      Write_file = TRUE,
      file_name = paste("Outputs/eval1001/G","GeometryFWHM", pfwhm[p_range],
                        "NoiseFWHM", noise_fwhm, sep = "_")
    )
    
    # remove and use gc()
    rm(power_results)
    gc()
    
  }
}


