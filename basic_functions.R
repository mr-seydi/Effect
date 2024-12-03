
#Fmax/SnPM function
source("Fmax.R")


#############Loading Library#####################

#install.packages("reticulate")
library(reticulate) # to read python code
#pip install spm1d
spm1d <- reticulate::import("spm1d")
numpy <- reticulate::import("numpy")
#pip install power1d
power1d <- reticulate::import("power1d")
scipy.ndimage <- reticulate::import("scipy.ndimage")

#Writing to excel
library(openxlsx)

################Data with NA#########################
completed_data <- function(x, y, defined_domain=c(0,100)) {
  
  if (length(defined_domain) != 2) {
    stop("Domain must have a form like c(starting point,ending point)")
  }
  start_domain <- defined_domain[1]
  end_domain <- defined_domain[2]
  
  # Perform linear interpolation using approx() function
  xnew <- seq(start_domain, end_domain, by = 1)  # New x values with steps of 1
  interpolated <- approx(na.omit(x), na.omit(y), xout = xnew)  # Interpolate y values
  
  # Return new y values
  return(interpolated$y)
}


###################Functions###########################

sigma_to_fwhm <- function(sigma){
  return(sigma*(2 * sqrt(2 * log(2))))
}

fwhm_to_sigma <- function(fwhm){
  return(fwhm / (2 * sqrt(2 * log(2))))
}


ksmooth_gaussian <- function(x, fwhm) {
  
  SD = fwhm_to_sigma(fwhm)
  #smoothed_vals = ksmooth(x = 1:length(x), y = x, bandwidth = SD, kernel = "normal")$y
  smoothed_vals = scipy.ndimage$gaussian_filter1d(x, SD, mode='wrap')#$tolist()
}


#unsmoothed guassian noise curve
noise_guassian_curve <- function(number_of_curves, continuum_size){ 
  data <- matrix(rnorm(number_of_curves*continuum_size),
                 nrow = number_of_curves, ncol = continuum_size)
  return(data)
  
}



#apply this scaling factor to any smoothed data to ensure it has unit variance after smoothing
# from _set_scale function in https://github.com/0todd0000/power1d/blob/master/src/power1d/random.py#L35

set_scale <- function(nNodes, SD) {
  # Define a small epsilon to prevent division by zero
  eps <- .Machine$double.eps
  
  # Step 1: Define a Gaussian kernel
  t <- seq(-0.5 * (nNodes - 1), 0.5 * (nNodes - 1), length.out = nNodes)
  gf <- exp(-(t^2) / (2 * SD^2 + eps))
  
  # Step 2: Normalize the Gaussian kernel
  gf <- gf / sum(gf)
  
  # Step 3: Calculate the expected variance of the smoothed data
  # Perform FFT and compute power spectrum
  AG <- fft(gf)
  Pag <- Mod(AG)^2  # Equivalent to AG * Conj(AG)
  
  # Calculate the autocovariance by inverse FFT
  COV <- Re(fft(Pag, inverse = TRUE)) / length(Pag)
  svar <- COV[1]  # Variance of the smoothed field
  
  # Step 4: Calculate the scaling factor
  SCALE <- sqrt(1.0 / svar)
  
  return(SCALE)
}


smoothed_gussian_curves <- function(data, mu, sig, fwhm) {
  
  # Step 1: Smooth each curve in the data
  smoothed_data <- ksmooth_gaussian(data, fwhm)
  
  
  # Step 2: Normalize the smoothed data to have unit variance
  nNodes <- ncol(smoothed_data)
  SD <- fwhm_to_sigma(fwhm)
  scale_factor <- set_scale(nNodes, SD)
  
  # Step 3: Scale the smoothed data
  smoothed_data_scaled <- smoothed_data * scale_factor
  
  # Step 4: Transform to have mean = mu and standard deviation = sig
  smoothed_data_final <- smoothed_data_scaled * sig + mu
  
  return(t(smoothed_data_final))
}



Noise_generator <- function(Sample_size, Continuum_size, Noise_mu, Noise_sig, Noise_fwhm){
  
  noise1 <- noise_guassian_curve(number_of_curves = Sample_size,
                                 continuum_size = Continuum_size)
  noise1 <- smoothed_gussian_curves(noise1, Noise_mu, Noise_sig, Noise_fwhm)
  
  sd_noise <- apply(noise1, 1, sd)
  mean_noise <- apply(noise1, 1, mean)
  
  return(list(noise1 = noise1, SD = sd_noise, Mean = mean_noise))
}

# Generating data (Baseline+noise+signal) or (Baseline+noise) or (Baseline+signal)
data_generator <- function(data,signal,noise) {
  sample_size <- dim(noise)[2]
  
  if(missing(data) & missing(signal)){
    data_out <- noise
  } else if (missing(data)){
    signal_baseline <- matrix(rep(signal,time=sample_size),
                              ncol = sample_size)
    data_out <- signal_baseline + noise
  }
  else if (missing(signal)) {
    data_baseline <- matrix(rep(data,time=sample_size),
                            ncol = sample_size)
    data_out <- data_baseline + noise
  } else if (missing(noise)) {
    data_baseline <- matrix(rep(data,time=sample_size),
                            ncol = sample_size)
    signal_baseline <- matrix(rep(signal,time=sample_size),
                              ncol = sample_size)
    data_out <- data_baseline + signal_baseline
  } else {
    data_baseline <- matrix(rep(data,time=sample_size),
                            ncol = sample_size)
    signal_baseline <- matrix(rep(signal,time=sample_size),
                              ncol = sample_size)
    data_out <- data_baseline + signal_baseline + noise 
  }
  return(data_out)
}



gaussian_pulse <- function(center, fwhm, continuum_size) {
  sigma = fwhm_to_sigma(fwhm)
  x_values = seq(0, continuum_size-1, by = 1)
  dens <- dnorm(x_values, mean = center, sd = sigma)
  return(list(density_val=dens, x_values=x_values))
}

amplitude_pulse <- function(data, amp){
  scaling_factor = amp / max(data)
  y_values = scaling_factor * data
  return(y_values)
}


square_pulse <- function(start_end_pulse, start_height,
                         pulse_height, continuum_size=101) {
  
  if (length(start_end_pulse) != 2 ||
      start_end_pulse[2]>(continuum_size-1) ||
      start_end_pulse[1] > start_end_pulse[2] ) {
    stop("start_end_pulse must have a form like c(start, end) and be within the continuum_size")
  }
  start = start_end_pulse[1]
  end = start_end_pulse[2]
  pulse = rep(start_height, continuum_size)
  pulse[start:end] = pulse_height
  return(pulse)
}



##############Methods functions###########

Initialize_method_list <- function(Methods, Conti_size=101, Iter_number=100){
  method_list <- list()
  for (M in Methods) {
    method_list[[M]] <- matrix(,nrow = Conti_size, ncol = 0)
  }
  return(method_list)
}

Power_data_generator <- function(Sample_size, Data,
                                 Signal, Conti_size = 101,
                                 Noise_mu, Noise_sig, Noise_fwhm){
  
  noise1 <- noise_guassian_curve(number_of_curves = Sample_size,
                                 continuum_size = Conti_size)
  noise2 <- noise_guassian_curve(number_of_curves = Sample_size,
                                 continuum_size = Conti_size)
  noise1 <- smoothed_gussian_curves(noise1, Noise_mu, Noise_sig, Noise_fwhm)
  noise2 <- smoothed_gussian_curves(noise2, Noise_mu, Noise_sig, Noise_fwhm)
  
  if (is.null(Data)){
    data1 <- data_generator(signal = Signal, noise = noise1)
    data2 <- data_generator(noise = noise2)    
  }
  else if (is.null(Signal)) {
    data1 <- data_generator(data = Data[,1], noise = noise1)
    data2 <- data_generator(data = Data[,2], noise = noise2)
  } else {
    data1 <- data_generator(data = Data, signal = Signal, noise = noise1)
    data2 <- data_generator(data = Data, noise = noise2)      
  }
  
  return(list(data1 = data1, data2 = data2))
}

Pvalue_calculator <- function(method_list, data1, data2){
  
  Methods <- names(method_list)
  
  for (M in Methods) {
    Pvalues <- Pval_method(sampel1 = t(data1), sample2 = t(data2),
                           method = M)
    #p_values dimension is continuum_size*Iter_number
    method_list[[M]] <- cbind(method_list[[M]], Pvalues)
  }
  return(method_list) #Filled in method list
}


Power_calculator <- function(Pvalues, Iter_number, Alpha){
  
  Methods <- names(Pvalues)
  
  for (M in Methods) {
    
    
    # Check if the method is either "ERL" or "IATSE"
    #Because those methods do not return p-values
    if (M %in% c("ERL", "IATSE")) {
      pvalue_less_alpha <- Pvalues[[M]]
      # For "ERL" and "IATSE", only run this code
      power <- sum(colSums(pvalue_less_alpha) > 0) / Iter_number
    } else {
      pvalue_less_alpha <- Pvalues[[M]] < Alpha
      power <- sum(colSums(pvalue_less_alpha) > 0) / Iter_number
    }
    
    Pvalues[[M]] <- power
  }
  
  return(Pvalues)
  
}



Pval_method <- function(sampel1,sample2,method) {
  if (method=="IWT") {
    pval <- IWT(sampel1,sample2)
  } else if (method=="TWT"){
    pval <- TWT(sampel1,sample2)
  } else if (method=="Parametric_SPM"){
    pval <- p_spm(sampel1,sample2)
  } else if (method=="Nonparametric_SPM"){
    pval <- p_snpm(sampel1,sample2)
  } else if (method=="ERL"){
    pval <- ERL(sampel1,sample2)
  } else if (method=="IATSE"){
    pval <- IATSE(sampel1,sample2)
  } else {
    stop("Choose a method between options")
  }
  return(pval)
}


p_spm <- function(data1, data2){
  spm  <- spm1d$stats$ttest2(data1, data2, equal_var=FALSE)
  p_val <- spm1d$rft1d$f$sf((spm$z)^2, spm$df, spm$Q, spm$fwhm, withBonf=TRUE)
  return(p_val)
}

p_snpm <- function(data1, data2, B = 1000){
  
  n1 = dim(data1)[1]
  n2 = dim(data2)[1]
  
  
  group12 = factor(c(rep(1,n1),rep(2,n2)))
  data_group12 <- rbind(data1,data2)
  
  # Create a data frame that includes both data_group12 and group12
  combined_data <- data.frame(data_group12, group12)
  
  # Pass the formula with the combined data to Fmax
  Fmax_pval <- Fmax(data_group12 ~ group12, DATA = combined_data)
  return(Fmax_pval$adjusted_pval_F)
  
}







############## Parallel processing functions ####################
# Custom combine function to accumulate matrices across iterations
parallel_combine_function <- function(x, y) {
  for (M in names(x)) {
    # Combine matrices from the same method in both x and y
    x[[M]] <- cbind(x[[M]], y[[M]])
  }
  return(x)
}

# Handle the Excel file creation and saving for the results of parallel processing
write_results_to_excel <- function(loop, power_results, input_info, file_name) {
  # Create a new Excel workbook
  wb <- createWorkbook()
  
  # Write each method's p-values matrix to a separate sheet, named as "Method=PowerValue"
  for (method_name in names(loop)) {
    # Get the power result for this method
    power_value <- round(power_results[[method_name]], 2)
    
    # Create a sheet name like "TWT=0.08"
    sheet_name <- paste0(method_name, "=", format(power_value, nsmall = 2))
    
    # Add a new sheet
    addWorksheet(wb, sheet_name)
    
    # Write the p-values matrix to the sheet
    writeData(wb, sheet = sheet_name, loop[[method_name]])
  }
  
  # Add a final sheet for the Input_Summary
  addWorksheet(wb, "Input_Summary")
  
  # Write the input_info dataframe to the Input_Summary sheet
  writeData(wb, sheet = "Input_Summary", input_info)
  
  # Save the workbook to the specified file name
  saveWorkbook(wb, file_name, overwrite = TRUE)
}


############simulation function####################
centered_ranges <- function(percentages, domain = c(0, 100)) {
  midpoint <- mean(domain)
  range_widths <- (percentages / 100) * diff(domain)
  
  starts <- midpoint - range_widths / 2
  ends <- midpoint + range_widths / 2
  
  result <- data.frame(
    Percentage = percentages,
    Start = starts,
    End = ends
  )
  
  return(result)
}



