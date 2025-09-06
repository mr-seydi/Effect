
rm(list=ls())
source("R/sources.R")


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
methods <- c("Parametric_SPM", "Nonparametric_SPM")




Geometries <- c("Square", "dSquare", "G")

results_list <- list()

for (geom_type in Geometries) {
  
  results <- data.frame(Geom_percentage = numeric(),
                        NFWHM = numeric(),
                        NSD = numeric(),
                        Est_NFWHM = numeric(),
                        Est_dataFWHM = numeric())
  
  for(p_range in 1:nrow(cent_ranges)){
    
    
    for (noise_fwhm in noise_fwhm_values) {
      
      cat("Geometry:", geom_type, 
          "Geom Percentage:", cent_ranges$Percentage[p_range], 
          "Noise FWHM:", noise_fwhm, "\n")  
     
      if (geom_type== "G"){
        path1 <- paste(paste0("Outputs/sd1/", geom_type), "GeometryFWHM", cent_ranges$Percentage[p_range],
                       "NoiseFWHM", noise_fwhm, "datafwhm_est", sep = "_")
        path2 <- paste(paste0("Outputs/sd1/", geom_type), "GeometryFWHM", cent_ranges$Percentage[p_range],
                       "NoiseFWHM", noise_fwhm, "noisefwhm_est", sep = "_")
      } else {
        path1 <- paste(paste0("Outputs/sd1/", geom_type), "GeometryPercentage", cent_ranges$Percentage[p_range],
                       "NoiseFWHM", noise_fwhm,"datafwhm_est", sep = "_")
        path2 <- paste(paste0("Outputs/sd1/", geom_type), "GeometryPercentage", cent_ranges$Percentage[p_range],
                       "NoiseFWHM", noise_fwhm, "noisefwhm_est", sep = "_")
      }
      
      
      
      output_datafwhm <- read_fst(  paste0(path1, ".fst"), as.data.table = TRUE)
      output_noisefwhm <- read_fst(  paste0(path2, ".fst"), as.data.table = TRUE)
      

      results <- rbind(results, data.frame(
        Geom_percentage = cent_ranges$Percentage[p_range],
        NFWHM = noise_fwhm,
        NSD = noise_sd_values,
        Est_NFWHM = mean(unlist(output_noisefwhm)),
        Est_dataFWHM = mean(unlist(output_datafwhm))
      ))
      
      # remove and use gc()
      rm(output_datafwhm, output_noisefwhm)
      
      
    }
  }
  
  results_list[[geom_type]] <- results
}



# Save results to excel files
max_effect=1

for (geom_type in names(results_list)) {
  file_name <- paste0("Outputs/excel_results/Simulation_", geom_type, "_NoiseSD_",
                      noise_sd_values, "_Maxeffect_", max_effect, "_EstimatedFWHMs", ".xlsx")
  write.xlsx(results_list[[geom_type]], file = file_name, rowNames = FALSE)
}

