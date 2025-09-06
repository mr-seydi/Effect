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
#c(4:14)
# plot(JCF, main = "JCF",type = "l",ylim=c(0,1))
#c(13:30)
# plot(Angle, main = "Angle",type = "l",ylim=c(0,10))
#c(1:101)
# plot(Moment, main = "Moment",type = "l",ylim=c(-1,1))
#c(11:92)
# plot(MF, main = "MF",type = "l")
#c(29:55)
# plot(EMG, main = "EMG", type = "l")
#c(1:5,26:48,94:101)

geom <- cbind(vGRFPhan, JCF, Angle, Moment, MF, EMG)
#apply(geom, 2,max)
#apply(geom, 2,min)
D1_prime <- list(
  c(4:14), # vGRFPhan
  c(13:30), # JCF
  c(1:101), # Angle
  c(11:92), # Moment
  c(29:55), # MF
  c(1:5,26:48,94:101) # EMG
)

names(D1_prime) <- colnames(geom)

set.seed(12345)
N_sim <- 5000
sample_size <- 10

methods <- c("Parametric_SPM", "Nonparametric_SPM")




results_list <- list()

for (g in 1:ncol(geom)){
  
  #noise_sd_values <- max(geom[,g])
  results <- data.frame(Data = character(),
                        NFWHM = numeric(),
                        Power_Parametric_SPM = numeric(),
                        Power_Nonparametric_SPM = numeric(),
                        Sensitivity_Parametric_SPM = numeric(),
                        Sensitivity_Nonparametric_SPM = numeric(),
                        Functional_power_Parametric_SPM = numeric(),
                        Functional_power_Nonparametric_SPM = numeric())
  
  for (noise_fwhm in noise_fwhm_values) {
    file_name <- paste("Outputs/RealData/data", colnames(geom)[g],
                       "NoiseFWHM", noise_fwhm, sep = "_")
    SPM_data <- read.fst(paste0(file_name,"_","Parametric_SPM",".fst"))
    SnPM_data <- read.fst(paste0(file_name,"_","Nonparametric_SPM",".fst"))
    
    SPM_data <- SPM_data[,sample(1:ncol(SPM_data),N_sim,replace = FALSE)]
    SnPM_data <- SnPM_data[,sample(1:ncol(SnPM_data),N_sim,replace = FALSE)]
    
    
    # Print current loop status
    cat("Geom",colnames(geom)[g],
      "Noise FWHM:", noise_fwhm, "\n")
    
    # Combine results for both methods
    combined_list <- list(SPM_data, SnPM_data)
    names(combined_list) <- methods
    
    Omnibus_power <- Power_calculator(Pvalues=combined_list, Iter_number=N_sim, Alpha=0.05)
    
    D1 <- which(geom[,g]!=0)
    Sensitivity <- Sensetivity_calculator(Pvalues=combined_list, D1=D1, Alpha=0.05)
    

    Functional_power <- Sensetivity_calculator(Pvalues=combined_list, D1=D1_prime[[g]], Alpha=0.05)
    
    results <- rbind(results, 
                     data.frame(
                       Data = colnames(geom)[g],
                       NFWHM = noise_fwhm,
                       Power_Parametric_SPM = Omnibus_power$Parametric_SPM,
                       Power_Nonparametric_SPM = Omnibus_power$Nonparametric_SPM,
                       Sensitivity_Parametric_SPM = Sensitivity$Parametric_SPM,
                       Sensitivity_Nonparametric_SPM = Sensitivity$Nonparametric_SPM,
                       Functional_power_Parametric_SPM = Functional_power$Parametric_SPM,
                       Functional_power_Nonparametric_SPM = Functional_power$Nonparametric_SPM
                     ))
    
    
    # remove and use gc()
    rm(SPM_data, SnPM_data, combined_list, Omnibus_power, Sensitivity, Functional_power)
    
  }
  
  results_list[[colnames(geom)[g]]] <- results
}



library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

realdata_power_plot <- function(df,plot_title){
  # Assuming your data is stored in a dataframe called df
  # Here's how to reshape it
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("Power_") | starts_with("Sensitivity_") | starts_with("Functional_power_"),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    mutate(
      Type = case_when(
        grepl("Power_", Metric) & !grepl("Functional", Metric) ~ "Omnibus power",
        grepl("Sensitivity", Metric) ~ "Sensitivity",
        grepl("Functional_power", Metric) ~ "Functional power"
      ),
      Method = case_when(
        grepl("Parametric", Metric) ~ "SPM",
        grepl("Nonparametric", Metric) ~ "SnPM"
      )
    )
  
  # Set order of factor levels
  df_long <- df_long %>%
    mutate(
      Type = factor(Type, levels = c("Omnibus power", "Sensitivity", "Functional power")),
      Method = factor(Method, levels = c("SPM", "SnPM"))
    )
  
  # Plot with manual control over colors and line types
  plot <- ggplot(df_long, aes(x = NFWHM, y = Value, color = Type, linetype = Method)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(
      values = c("Omnibus power" = "darkgreen", "Sensitivity" = "darkorange", "Functional power" = "darkviolet")
    ) +
    scale_linetype_manual(
      values = c("SPM" = "solid", "SnPM" = "dotted")
    ) +
    labs(x = "NFWHM", y = "Value", title = plot_title) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
  # change y-axis label
  plot <- plot + 
    labs(y = "Probability", 
         x = "Noise FWHM")
  
  return(plot)
  
  
}

# plot for all data
vGRF <- realdata_power_plot(results_list$vGRFPhan, "vGRF") + #remove legend
  theme(legend.position = "none") # remove legend
JCF <- realdata_power_plot(results_list$JCF, "JCF") + #remove legend
  theme(legend.position = "none") # remove legend
HFA <- realdata_power_plot(results_list$Angle, "HFA") + #remove legend
  theme(legend.position = "none") # remove legend
KJM <- realdata_power_plot(results_list$Moment, "KJM") + #remove legend
  theme(legend.position = "none") # remove legend
MF <- realdata_power_plot(results_list$MF, "MF") + #remove legend
  theme(legend.position = "none") # remove legend
EMG <- realdata_power_plot(results_list$EMG, "EMG") + #remove legend
  theme(legend.position = "none") # remove legend

# Combine all plots into one
## -------------------------------
## Row 1: Three Data Plots (vGRF, JCF, Angle)
## -------------------------------


# Arrange the three plots and add the legend below
row1 <- plot_grid(EMG, HFA, JCF, ncol = 3)
row1_without_legend <- plot_grid(row1, ncol = 1, rel_heights = c(1, 0.2))

## -------------------------------
## Row 2: Three Data Plots (Moment, MF, EMG)
## -------------------------------



# Extract a common legend from one of the plots (they share the same aesthetic)
grob_legend <- ggplotGrob(realdata_power_plot(results_list$EMG, "EMG"))

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]


# Arrange the three plots and add the legend below
row2 <- plot_grid(KJM, MF, vGRF, ncol = 3)
row2_with_legend <- plot_grid(row2, shared_legend, ncol = 1, rel_heights = c(1, 0.2))

## -------------------------------
## Combine All Rows into a Single Layout
## -------------------------------

final_plot <- plot_grid(row1_without_legend, row2_with_legend, ncol = 1)

# Display the final plot
print(final_plot)
#ggsave("Outputs/Plots/ISB_Power_new.jpeg", final_plot, width = 190, height = 120, units = "mm", dpi = 1200)


####
# Plot all the SPM results together in one plot wiht different coloor for different dataset
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
results_all <- do.call(rbind, results_list)
results_all$Data <- factor(results_all$Data, levels = c("vGRFPhan", "JCF", "Angle", "Moment", "MF", "EMG"))
levels(results_all$Data) <- c("vGRF", "JCF", "HFA", "KJM", "MF", "EMG")
results_all_long <- results_all %>%
  pivot_longer(
    cols = starts_with("Power_") | starts_with("Sensitivity_") | starts_with("Functional_power_"),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Type = case_when(
      grepl("Power_", Metric) & !grepl("Functional", Metric) ~ "Omnibus power",
      grepl("Sensitivity", Metric) ~ "Sensitivity",
      grepl("Functional_power", Metric) ~ "Functional power"
    ),
    Method = case_when(
      grepl("Parametric", Metric) ~ "SPM",
      grepl("Nonparametric", Metric) ~ "SnPM"
    )
  )
# Set order of factor levels
results_all_long <- results_all_long %>%
  mutate(
    Type = factor(Type, levels = c("Omnibus power", "Sensitivity", "Functional power")),
    Method = factor(Method, levels = c("SPM", "SnPM"))
  )
# Plot with manual control over colors and line types
plot_all <- ggplot(results_all_long, aes(x = NFWHM, y = Value, color = Data, linetype = Method)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(
    values = c("vGRF" = "blue", "JCF" = "red", "HFA" = "green", "KJM" = "purple", "MF" = "orange", "EMG" = "brown")
  ) +
  scale_linetype_manual(
    values = c("SPM" = "solid", "SnPM" = "dotted")
  ) +
  labs(x = "NFWHM", y = "Value", title = "Power and Sensitivity across Datasets") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  ) +
  facet_wrap(~ Type, ncol = 1) # Facet by Type to create separate panels
# change y-axis label
plot_all <- plot_all + 
  labs(y = "Probability", 
       x = "Noise FWHM")
print(plot_all)

# plot omnibus power only and get two separated plots based on the Method
plot_omnibus <- ggplot(results_all_long %>% filter(Type == "Omnibus power"), 
                       aes(x = NFWHM, y = Value, color = Data, linetype = Method)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(
    values = c("vGRF" = "darkblue", "JCF" = "darkred", "HFA" = "darkgreen", "KJM" = "purple", "MF" = "darkorange", "EMG" = "darkgrey")
  ) +
  scale_linetype_manual(
    values = c("SPM" = "solid", "SnPM" = "solid")
  ) +
  labs(x = "NFWHM", y = "Value", title = "Omnibus Power across Datasets",
       color = "Dataset:") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )
# change y-axis label
plot_omnibus <- plot_omnibus + 
  labs(y = "Omnibus Power", 
       x = "Noise FWHM")
print(plot_omnibus)
# separate the plot based on the Method
plot_omnibus_SPM <- plot_omnibus + 
  geom_line(linewidth = 1.5) +
  geom_point(data = results_all_long %>% 
               filter(Type == "Omnibus power", Method == "SPM"), 
             aes(x = NFWHM, y = Value, color = Data), 
             size = 2) +   # ✅ points only for SPM
  scale_linetype_manual(values = c("SPM" = "solid")) +
  guides(linetype = "none") +
  labs(title = "SPM") +
  # remove legend
  theme(legend.position = "none")
  
plot_omnibus_SnPM <- plot_omnibus +
  geom_line(linewidth = 1.5) +
  geom_point(data = results_all_long %>% 
               filter(Type == "Omnibus power", Method == "SnPM"), 
             aes(x = NFWHM, y = Value, color = Data), 
             size = 2) +   # ✅ points only for SPM
  scale_linetype_manual(values = c("SnPM" = "solid")) +
  guides(linetype = "none") +
  labs(title = "SnPM") +
  # remove legend
  theme(legend.position = "none")


plot_legend <- plot_omnibus + 
  geom_line(linewidth = 1.5) +
  geom_point(data = results_all_long %>% 
               filter(Type == "Omnibus power", Method == "SPM"), 
             aes(x = NFWHM, y = Value, color = Data), 
             size = 2) +   # ✅ points only for SPM
  scale_linetype_manual(values = c("SPM" = "solid")) +
  guides(linetype = "none") +
  labs(title = "SPM")+
  # add title for legend
  theme(legend.position = "bottom")
# Convert the plot to a grob object
grob_legend <- ggplotGrob(plot_legend)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

# arrange the two plots side by side and the share legend below
final_omnibus_plot <- plot_grid(plot_omnibus_SPM, plot_omnibus_SnPM, ncol = 2)
final_omnibus_plot_with_legend <- plot_grid(final_omnibus_plot, shared_legend, ncol = 1, rel_heights = c(1, 0.2))
print(final_omnibus_plot_with_legend)
# ggsave("Outputs/PaperPlots/Plot_omnibus_realdata.jpeg", final_omnibus_plot_with_legend, width = 190, height = 120, units = "mm", dpi = 1200)
# ggsave("Outputs/PaperPlots/Plot_omnibus_realdata.tiff", final_omnibus_plot_with_legend, width = 190, height = 120, units = "mm", dpi = 1200, device = "tiff")
