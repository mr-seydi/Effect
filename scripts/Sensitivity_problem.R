
source("R/sources.R")

Power_parallel <- function(data, sample_size,
                           noise_mean, noise_sd, noise_fwhm,
                           signal,
                           method, n_iterations,
                           Continuum_size = 101,
                           domain_points = NULL,
                           Write_file = FALSE,
                           file_name = "Power_Results.xlsx"){
  

  # 1) Create an initial “empty” result list with exactly the same names
  init_res <- list(
    method_list       = Initialize_method_list(Methods      = method,
                                               Conti_size   = ifelse(is.null(domain_points), Continuum_size, domain_points),
                                               Iter_number  = n_iterations)
  )
  
  # parallelize the power calculation
  number_cores=detectCores() #number of cores
  registerDoParallel(number_cores) #register the number of cores
  
  
  # The .combine function in foreach always takes two inputs at a time:
  # First: The cumulative result of previous iterations (starting with an initial value, if provided)
  # Second: The result of the current iteration of the loop
  loop <- foreach(k = seq_len(n_iterations),
                  .combine = parallel_combine_function, .packages      = c("fst"),
                  .init    = init_res,
                  .export      = c(
                    "Power_data_generator", "Pvalue_calculator",
                    "estimate_fwhm", "residuals_data"
                  )) %dopar% {
                    
                    # Generate the data
                    generated_data <- Power_data_generator(Sample_size = sample_size,
                                                           Data = data,
                                                           Signal = signal,
                                                           Conti_size = Continuum_size,
                                                           Noise_mu = noise_mean,
                                                           Noise_sig = noise_sd,
                                                           Noise_fwhm = noise_fwhm,
                                                           n_evaluation_points = domain_points)
                    
                    # … fill your method_list as before …
                    ml <- Pvalue_calculator(init_res$method_list,
                                            generated_data$data1,
                                            generated_data$data2)
                    

                    
                    # drop the heavy raw data
                    rm(generated_data)
                    gc()
                    
                    # return a named list matching init_res
                    list(
                      method_list    = ml
                    )
                    
                  }
  
  
  stopImplicitCluster()
  
  # # Calculate the power based on the result
  # power_results <- Power_calculator(loop$method_list , n_iterations, Alpha = 0.05)
  # 
  # # Add power results to input_info dataframe for each method
  # for (method_name in names(power_results)) {
  #   # Add a new column with the power result for each method
  #   input_info[[method_name]] <- power_results[[method_name]]
  # }
  
  if (Write_file == TRUE) {
    
    write_results_to_fst(loop$method_list, base_path = file_name)

    
  }
  
  
  # loop gc()
  return(loop)


  
}




# Define the input parameters
noise_fwhm_values <- seq(5,50,by=5)

# geometry
cent_ranges <- centered_ranges(seq(5, 100, by = 5))

N_sim <- 1000
sample_size <- 200
noise_sd_values <- 1
methods <- c("Parametric_SPM")

p_range <- 2
noise_fwhm <- 50

  
  #generate pulse
  Pulse <- square_pulse(start_end_pulse = c(cent_ranges$Start[p_range],
                                            cent_ranges$End[p_range]),
                        start_height = 0, pulse_height = 1)

    
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
      Write_file = FALSE,
      file_name = ""
    )

    
Power_calculator(power_results$method_list, 1000, 0.05)
D1 <- which(Pulse!=0)
Sensetivity_calculator(power_results$method_list, D1, 0.05)

plot1 = pulse_single_plot(square_pulse(start_end_pulse = c(cent_ranges$Start[p_range],
                                                   cent_ranges$End[p_range]),
                               start_height = 0, pulse_height = 1),"10", "Domain%")
plot2 = pulse_single_plot(Pulse, "100", "Domain%")

#plot1 and plot 2 in one plot
library(ggplot2)
library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)


