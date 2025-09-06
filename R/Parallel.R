Power_parallel <- function(data, sample_size,
                           noise_mean, noise_sd, noise_fwhm,
                           signal,
                           method, n_iterations,
                           Continuum_size = 101,
                           domain_points = NULL,
                           Write_file = FALSE,
                           file_name = "Power_Results.xlsx"){
  
  # # Capture input argument names and values
  # input_info <- data.frame(
  #   data = ifelse(is.null(data),"NULL",deparse(substitute(data))),         # Store the name of 'data' as a string
  #   sample_size = sample_size,                # Numeric value
  #   noise_mean = noise_mean,                  # Numeric value
  #   noise_sd = noise_sd,                      # Numeric value
  #   noise_fwhm = noise_fwhm,                  # Numeric value
  #   signal = ifelse(is.null(signal),"NULL",signal),        # Numeric value
  #   method = paste(method, collapse = ", "),  # Store 'method' as a string of methods
  #   n_iterations = n_iterations               # Numeric value
  # )
  

  
  # 1) Create an initial “empty” result list with exactly the same names
  init_res <- list(
    method_list       = Initialize_method_list(Methods      = method,
                                               Conti_size   = ifelse(is.null(domain_points), Continuum_size, domain_points),
                                               Iter_number  = n_iterations)#,
    #est_noise_fwhm    = numeric(),   # will collect scalars
    #est_data_fwhm     = numeric()    # will collect scalars
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
                     
                     # # estimate those FWHMs
                     # noise_fwhm_estimate <- estimate_fwhm(
                     #   residuals_data(t(generated_data$data1),
                     #                  t(generated_data$data2))
                     # )
                     # data_fwhm  <- estimate_fwhm(
                     #   rbind(t(generated_data$data1),
                     #         t(generated_data$data2))
                     # )
                     
                     
                     # drop the heavy raw data
                     rm(generated_data)
                     gc()
                     
                     # return a named list matching init_res
                     list(
                       method_list    = ml#,
                       # est_noise_fwhm = noise_fwhm_estimate,
                       # est_data_fwhm  = data_fwhm
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
    #write_estimated_noisefwhm_to_fst(loop$est_noise_fwhm, base_path = file_name)
    #write_estimated_datafwhm_to_fst(loop$est_data_fwhm, base_path = file_name)
    
  }
  
  
  # loop gc()
  rm(loop)
  gc()
  
  # 
  # return(list(Pvalues_methods = loop$method_list, Power_results = power_results,
  #             Input_Summary = input_info, File = file_name, Estimated_Noise_FWHM = mean(loop$est_noise_fwhm),
  #             Estimated_Data_FWHM = mean(loop$est_data_fwhm)))
  
}



