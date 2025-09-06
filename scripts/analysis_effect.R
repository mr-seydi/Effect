
rm(list=ls())
source("R/sources.R")

set.seed(12345)
# Define the input parameters
noise_fwhm_values <- seq(5,50,by=5)

# geometry Square
cent_ranges <- centered_ranges(seq(5, 100, by = 5))
# dSquare
left<-centered_ranges(seq(5, 100, by = 5), domain=c(0,50))
right<-centered_ranges(seq(5, 100, by = 5), domain=c(50,100))
cent_ranges_dSquare <- cbind(left,right)
# Gaussian fwhm
pfwhm <- seq(5,100,by=5)


noise_sd_values <- 1
max_effect_values <- 0.5

methods <- c("Parametric_SPM", "Nonparametric_SPM")






Geometries <- c("Square", "dSquare", "G")

results_list <- list()

for (geom_type in Geometries) {
  
  results <- data.frame(Geom_percentage = numeric(),
                        NFWHM = numeric(),
                        NSD = numeric(),
                        Power_Parametric_SPM = numeric(),
                        Power_Nonparametric_SPM = numeric(),
                        Sensitivity_Parametric_SPM = numeric(),
                        Sensitivity_Nonparametric_SPM = numeric())
  
  for(p_range in 1:nrow(cent_ranges)){
    
    if (geom_type == "G") {
      #generate pulse
      Pulse <- gaussian_pulse(center=50, fwhm=pfwhm[p_range], continuum_size=101)
      Pulse <- amplitude_pulse(Pulse$density_val, amp=max_effect_values)
      
    } else if (geom_type == "Square") {
      #generate pulse
      Pulse <- square_pulse(start_end_pulse = c(cent_ranges$Start[p_range],
                                                cent_ranges$End[p_range]),
                            start_height = 0, pulse_height = max_effect_values)
      
    } else if (geom_type == "dSquare") {
      #generate pulse
      one_side <- square_pulse(start_end_pulse=c(left$Start[p_range],left$End[p_range]), start_height=0,
                               pulse_height=max_effect_values)
      other_side <- square_pulse(start_end_pulse=c(right$Start[p_range],right$End[p_range]), start_height=0,
                                 pulse_height=-max_effect_values)
      Pulse = one_side + other_side
    } else {
      stop("Invalid geometry type specified.")
    }
    
    
    
    for (noise_fwhm in noise_fwhm_values) {
      
      cat("Geometry:", geom_type, 
          "Geom Percentage:", cent_ranges$Percentage[p_range], 
          "Noise FWHM:", noise_fwhm, "\n")  
      
      if (geom_type== "G"){
        path1 <- paste(paste0("Outputs/effect",max_effect_values,"/", geom_type), "GeometryFWHM", cent_ranges$Percentage[p_range],
                       "NoiseFWHM", noise_fwhm, methods[1], sep = "_")
        path2 <- paste(paste0("Outputs/effect",max_effect_values,"/", geom_type), "GeometryFWHM", cent_ranges$Percentage[p_range],
                       "NoiseFWHM", noise_fwhm, methods[2], sep = "_")
      } else {
        path1 <- paste(paste0("Outputs/effect",max_effect_values,"/", geom_type), "GeometryPercentage", cent_ranges$Percentage[p_range],
                       "NoiseFWHM", noise_fwhm, methods[1], sep = "_")
        path2 <- paste(paste0("Outputs/effect",max_effect_values,"/", geom_type), "GeometryPercentage", cent_ranges$Percentage[p_range],
                       "NoiseFWHM", noise_fwhm, methods[2], sep = "_")
      }
      
      
      
      output_SPM <- read_fst(  paste0(path1, ".fst"))
      output_SnPM <- read_fst(  paste0(path2, ".fst"))
      
      dim(output_SPM)
      dim(output_SnPM)
      
      Simulation_number <- 5000
      
      output_SPM <- output_SPM[,sample(1:ncol(output_SPM),Simulation_number,replace = FALSE)]
      output_SnPM <- output_SnPM[,sample(1:ncol(output_SnPM),Simulation_number,replace = FALSE)]
      
      
      output <- list(output_SPM, output_SnPM)
      names(output) <- methods
      
      Power <- Power_calculator(Pvalues=output, Iter_number=Simulation_number, Alpha=0.05)
      
      if (geom_type == "G"){
        center <- 50
        signal_sigma <- fwhm_to_sigma(pfwhm[p_range])
        D1 <- c(center - 2*signal_sigma, center + 2*signal_sigma)
        D1 <- max(1, trunc(D1[1])):min(trunc(D1[2]),101)
        D0 <- (1:101)[-D1]
        
      } else {
        D0 <- which(Pulse==0)
        D1 <- which(Pulse!=0)
      }
      
      
      
      Sensitivity <- Sensetivity_calculator(Pvalues=output, D1=D1, Alpha=0.05)
      
      
      results <- rbind(results, 
                       data.frame(Geom_percentage = cent_ranges$Percentage[p_range],
                                  NFWHM = noise_fwhm,
                                  NSD = noise_sd_values,
                                  Parametric_SPM = Power$Parametric_SPM,
                                  Nonparametric_SPM = Power$Nonparametric_SPM,
                                  Sensitivity_Parametric_SPM = Sensitivity$Parametric_SPM,
                                  Sensitivity_Nonparametric_SPM = Sensitivity$Nonparametric_SPM))
      
      
      # remove and use gc()
      rm(output_SPM, output_SnPM, output, Power)
      
      
    }
  }
  
  results_list[[geom_type]] <- results
}



# Save results to excel files
max_effect=max_effect_values

for (geom_type in names(results_list)) {
  file_name <- paste0("Outputs/excel_results/Simulation_", geom_type, "_NoiseSD_",
                      noise_sd_values, "_Maxeffect_", max_effect, ".xlsx")
  write.xlsx(results_list[[geom_type]], file = file_name, rowNames = FALSE)
}
