
rm(list=ls())
source("R/sources.R")


# Define the input parameters
noise_fwhm_values <- seq(5,50,by=5)

# pulse
left<-centered_ranges(seq(5, 100, by = 5), domain=c(0,50))
right<-centered_ranges(seq(5, 100, by = 5), domain=c(50,100))
cent_ranges <- cbind(left,right)

N_sim <- 10000
sample_size <- 10
noise_sd_values <- 1
methods <- c("Parametric_SPM", "Nonparametric_SPM")



for(p_range in 1:nrow(cent_ranges)){
  
  #generate pulse
  one_side <- square_pulse(start_end_pulse=c(left$Start[p_range],left$End[p_range]), start_height=0,
                           pulse_height=1)
  other_side <- square_pulse(start_end_pulse=c(right$Start[p_range],right$End[p_range]), start_height=0,
                             pulse_height=-1)
  Pulse = one_side + other_side
  
  for (noise_fwhm in noise_fwhm_values) {
    
    
    # Print current loop status
    #cat("Pulse percentage", cent_ranges$Percentage[p_range], "Noise FWHM:", noise_fwhm, "\n")
    
    
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
      file_name = paste("Outputs/eval1001/dSquare","GeometryPercentage", cent_ranges$Percentage[p_range],
                        "NoiseFWHM", noise_fwhm, sep = "_")
    )
    
    # remove and use gc()
    rm(power_results)
    gc()
    
    
  }
}



