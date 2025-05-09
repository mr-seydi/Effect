source("basic_functions.R")
library(readxl)
library(tidyr)
library(ggplot2)
power_plot_SnPM <- function(result_data,Ylim=c(0,1),Title="", legend="none"){
  #plot different curves representing the percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
  ggplot(result_data, aes(x = noise_fwhm, y = Nonparametric_SPM, color = as.factor(height_domain))) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = Title,
         x = "Noise FWHM", y = "Power", color = "Effect size") +
    #ylim between 0 and 1
    scale_y_continuous(limits = Ylim) +
    # remove legend
    theme(legend.position = legend) +
    guides(colour = guide_legend(nrow = 1))
}
power_plot_SPM <- function(result_data,Ylim=c(0,1),Title="", legend="none"){
  #plot different curves representing the percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
  ggplot(result_data, aes(x = noise_fwhm, y = Parametric_SPM, color = as.factor(height_domain))) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = Title,
         x = "Noise FWHM", y = "Power", color = "Effect size") +
    #ylim between 0 and 1
    scale_y_continuous(limits = Ylim) +
    # remove legend
    theme(legend.position = legend) +
    guides(colour = guide_legend(nrow = 2))
}

Pulse_plot <- function(Puls_data_long,Title_legend="Effect size"){
  ggplot(Puls_data_long, aes(x = Domain, y = Effect, group = Curve, linetype = Curve)) +
    geom_line(color = "black", linewidth = 0.3) +  # Use black color for all lines
    theme_classic() +
    # x-axis values 0, 50, 100
    scale_x_continuous(breaks = c(0, 50, 100)) +
    labs(title = "",
         x = "Domain", y = "Effect size", linetype = Title_legend) +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                          labels = c("0.1", "0.5", "1")) # Change labels # Custom line types
}


###############################################################
for (sd_vals in 5:9){
  print(sd_vals)
  data_out <- read_excel(paste0("results/Simulation_effect_sd0",sd_vals,".xlsx"))
  # colnames(data_out)
  data_1 <- data_out[data_out$percentage_domain == 5,]
  data_3 <- data_out[data_out$percentage_domain == 50,]
  data_9 <- data_out[data_out$percentage_domain == 100,]
  ################################################################
  centered_ranges_values <- centered_ranges(seq(5, 100, by = 5))
  five_percent <- centered_ranges_values[centered_ranges_values$Percentage == 5,]
  fifty_percent <- centered_ranges_values[centered_ranges_values$Percentage == 50,]
  hundred_percent <- centered_ranges_values[centered_ranges_values$Percentage == 100,]
  ##############################################
  effect_size <- seq(0.1, 1, by = 0.1)
  Pulse_5 <- matrix(0, nrow = 101 , ncol = length(effect_size))
  Pulse_50 <- matrix(0, nrow = 101 , ncol = length(effect_size))
  Pulse_100 <- matrix(0, nrow = 101 , ncol = length(effect_size))
  for(i in 1:length(effect_size)){
    Pulse_5[,i] <- square_pulse (start_end_pulse=c(five_percent$Start,five_percent$End),
                                 start_height=0, pulse_height=effect_size[i])
    Pulse_50[,i] <- square_pulse (start_end_pulse=c(fifty_percent$Start,fifty_percent$End),
                                  start_height=0, pulse_height=effect_size[i])
    Pulse_100[,i] <- square_pulse (start_end_pulse=c(hundred_percent$Start,hundred_percent$End),
                                   start_height=0, pulse_height=effect_size[i])
  }
  # Example: Assuming Pulse is a matrix with columns representing different curves
  # Convert to data frame and reshape
  Pulse_long_5 <- data.frame(Domain = 0:100, Pulse_5) %>%
    pivot_longer(-Domain, names_to = "Curve", values_to = "Effect")
  Pulse_long_50 <- data.frame(Domain = 0:100, Pulse_50) %>%
    pivot_longer(-Domain, names_to = "Curve", values_to = "Effect")
  Pulse_long_100 <- data.frame(Domain = 0:100, Pulse_100) %>%
    pivot_longer(-Domain, names_to = "Curve", values_to = "Effect")
  
  # Add a curve order column based on the column order in the matrix
  Pulse_long_5$Curve <- factor(Pulse_long_5$Curve, levels = unique(Pulse_long_5$Curve))
  Pulse_long_5$order <- as.numeric(Pulse_long_5$Curve) # Order for transparency and size mapping
  Pulse_long_5 <- dplyr::filter(Pulse_long_5, Pulse_long_5$Curve %in% c("X1", "X5", "X10")) # Filter to show only a few curves)
  
  Pulse_long_50$Curve <- factor(Pulse_long_50$Curve, levels = unique(Pulse_long_50$Curve))
  Pulse_long_50$order <- as.numeric(Pulse_long_50$Curve) # Order for transparency and size mapping
  Pulse_long_50 <- dplyr::filter(Pulse_long_50, Pulse_long_50$Curve %in% c("X1", "X5", "X10")) # Filter to show only a few curves)
  
  Pulse_long_100$Curve <- factor(Pulse_long_100$Curve, levels = unique(Pulse_long_100$Curve))
  Pulse_long_100$order <- as.numeric(Pulse_long_100$Curve) # Order for transparency and size mapping
  Pulse_long_100 <- dplyr::filter(Pulse_long_100, Pulse_long_100$Curve %in% c("X1", "X5", "X10")) # Filter to show only a few curves)
  
  ##############################################################################
  #plotting the power curves
  PS_1_SPM <- power_plot_SPM(data_1,Title="")
  PS_1_SnPM <- power_plot_SnPM(data_1,Title="")
  PS_3_SPM <- power_plot_SPM(data_3,Title="")
  PS_3_SnPM <- power_plot_SnPM(data_3,Title="")
  PS_9_SPM <- power_plot_SPM(data_9,Title="")
  PS_9_SnPM <- power_plot_SnPM(data_9,Title="")
  Pulse_5 <- Pulse_plot(Pulse_long_5)
  Pulse_50 <- Pulse_plot(Pulse_long_50)
  Pulse_100 <- Pulse_plot(Pulse_long_100)
  
  library(gridExtra)
  grid.arrange(PS_1_SPM,PS_1_SnPM,Pulse_5,PS_3_SPM,PS_3_SnPM,Pulse_50,PS_9_SPM,PS_9_SnPM,Pulse_100,ncol=3)
  
  
  
  library(gridExtra)
  library(grid)
  #install.packages("cowplot")
  library(cowplot)
  library(ggplot2)
  
  
  # Create a legend using one of the plots
  legend_plot <- power_plot_SPM(data_1, Title = "", legend = "bottom")
  # Convert the plot to a grob object
  grob_legend <- ggplotGrob(legend_plot)
  
  # Extract legend component from the gtable
  legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
  shared_legend <- grob_legend$grobs[[legend_index]]
  
  # Combine plots without legend
  plots <- list(PS_1_SPM, PS_1_SnPM, Pulse_5,
                PS_3_SPM, PS_3_SnPM, Pulse_50,
                PS_9_SPM, PS_9_SnPM, Pulse_100)
  
  # Arrange plots in a grid
  grid_plots <- arrangeGrob(grobs = plots, ncol = 3)
  
  # Add column labels
  column_labels <- arrangeGrob(
    grobs = lapply(c("SPM", "SnPM", "Geometry"), function(label) {
      textGrob(label, gp = gpar(fontsize = 12, fontface = "bold"))
    }),
    ncol = 3
  )
  
  # Combine column labels and plots
  grid_with_labels <- arrangeGrob(
    column_labels, grid_plots,
    ncol = 1,
    heights = unit.c(unit(1, "cm"), unit(0.9, "npc"))
  )
  
  # Add shared legend at the bottom
  final_plot <- plot_grid(
    grid_with_labels, cowplot::plot_grid(shared_legend),
    ncol = 1, rel_heights = c(0.9, 0.1)
  )
  
  # Display the final plot
  grid.newpage()
  grid.draw(final_plot)
  print(paste0("results/Plots/plot_effect_size_sd0",sd_vals,".jpeg"))
  ggsave(paste0("results/Plots/plot_effect_size_sd0",sd_vals,".jpeg"), final_plot, width = 190, height = 196, units = "mm", dpi = 600)
  
}

