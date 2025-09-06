rm(list=ls())
source("R/utilities.R")
source("R/Plot_functions.R")
library(readxl)
library(tidyr)
library(ggplot2)

###############################################################
data_square <- read_excel("Outputs/excel_results/Simulation_Square_NoiseSD_1_Maxeffect_1.xlsx")
data_dsquare <- read_excel("Outputs/excel_results/Simulation_dSquare_NoiseSD_1_Maxeffect_1.xlsx")
data_G <- read_excel("Outputs/excel_results/Simulation_G_NoiseSD_1_Maxeffect_1.xlsx")
################################################################
left<-centered_ranges(seq(5, 100, by = 5), domain=c(0,50))
right<-centered_ranges(seq(5, 100, by = 5), domain=c(50,100))
c_ranges <- cbind(left,right)
Pulse_two_square <- matrix(0, nrow = 101 , ncol = nrow(c_ranges))
for (i in 1:nrow(c_ranges)){
  one_side <- square_pulse(start_end_pulse=c(left$Start[i],left$End[i]), start_height=0,
                           pulse_height=1)
  other_side <- square_pulse(start_end_pulse=c(right$Start[i],right$End[i]), start_height=0,
                             pulse_height=-1)
  Pulse_two_square[,i] <- one_side + other_side
}

# Convert to data frame and reshape
Pulse_long_two_square <- data.frame(Domain = 0:100, Pulse_two_square) %>%
  pivot_longer(-Domain, names_to = "Curve", values_to = "Effect")
# Add a curve order column based on the column order in the matrix
Pulse_long_two_square$Curve <- factor(Pulse_long_two_square$Curve, levels = unique(Pulse_long_two_square$Curve))
Pulse_long_two_square$order <- as.numeric(Pulse_long_two_square$Curve) # Order for transparency and size mapping
#filter Curve %in% c("X1", "X10", "X20")
Pulse_long_two_square <- dplyr::filter(Pulse_long_two_square, Pulse_long_two_square$Curve %in% c("X1", "X10", "X20")) # Filter to show only a few curves)

##############################################
effect1 <- centered_ranges (seq(5, 100, by = 5))
Pulse <- matrix(0, nrow = 101 , ncol = nrow(effect1))
for (i in 1:nrow(effect1)){
  Pulse[,i] <- square_pulse (start_end_pulse=c(effect1$Start[i],effect1$End[i]),
                             start_height=0, pulse_height=1)
}
# Example: Assuming Pulse is a matrix with columns representing different curves
# Convert to data frame and reshape
Pulse_long <- data.frame(Domain = 0:100, Pulse) %>%
  pivot_longer(-Domain, names_to = "Curve", values_to = "Effect")
# Add a curve order column based on the column order in the matrix
Pulse_long$Curve <- factor(Pulse_long$Curve, levels = unique(Pulse_long$Curve))
Pulse_long$order <- as.numeric(Pulse_long$Curve) # Order for transparency and size mapping

Pulse_long <- dplyr::filter(Pulse_long, Pulse_long$Curve %in% c("X1", "X10", "X20")) # Filter to show only a few curves)
##################################################
Pfwhm <- seq(5, 100, by = 5)
Pulse_G <- matrix(0, nrow = 101 , ncol = length(Pfwhm))
for (i in 1:length(Pfwhm)){
  P_G <- gaussian_pulse(center=50, fwhm=Pfwhm[i], continuum_size=101)
  P_G <- amplitude_pulse(data=P_G$density_val, amp=1)
  Pulse_G[,i] <- P_G
}
# Convert to data frame and reshape
Pulse_long_GP <- data.frame(Domain = 0:100, Pulse_G) %>%
  pivot_longer(-Domain, names_to = "Curve", values_to = "Effect")
# Add a curve order column based on the column order in the matrix
Pulse_long_GP$Curve <- factor(Pulse_long_GP$Curve, levels = unique(Pulse_long_GP$Curve))
Pulse_long_GP$order <- as.numeric(Pulse_long_GP$Curve) # Order for transparency and size mapping
Pulse_long_GP <- dplyr::filter(Pulse_long_GP, Curve %in% c("X1", "X10", "X20")) # Filter to show only a few curves)
##################################################
#plotting the power curves
PS_1_SPM <- power_plot_SPM(data_square,Title="")
PS_1_SnPM <- power_plot_SnPM(data_square,Title="")
PS_3_SPM <- power_plot_SPM(data_dsquare,Title="")
PS_3_SnPM <- power_plot_SnPM(data_dsquare,Title="")
PS_9_SPM <- power_plot_SPM(data_G,Title="")
PS_9_SnPM <- power_plot_SnPM(data_G,Title="")
Pulse_Sq1 <- Pulse_plot(Pulse_long)
Pulse_Sq2 <- Pulse_plot(Pulse_long_two_square)
Pulse_GP <- Pulse_plot(Pulse_long_GP,Title_legend="SFWHM %")

library(gridExtra)
grid.arrange(PS_1_SPM,PS_1_SnPM,Pulse_Sq1,PS_3_SPM,PS_3_SnPM,Pulse_Sq2,PS_9_SPM,PS_9_SnPM,Pulse_GP,ncol=3)



library(gridExtra)
library(grid)
#install.packages("cowplot")
library(cowplot)
library(ggplot2)


# Create a legend using one of the plots
legend_plot <- power_plot_SPM(data_square, Title = "", legend = "bottom")
# Convert the plot to a grob object
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

# Combine plots without legend
plots <- list(PS_1_SPM, PS_1_SnPM, Pulse_Sq1,
              PS_3_SPM, PS_3_SnPM, Pulse_Sq2,
              PS_9_SPM, PS_9_SnPM, Pulse_GP)

# Arrange plots in a grid
grid_plots <- arrangeGrob(grobs = plots, ncol = 3)

# Add column labels
column_labels <- arrangeGrob(
  grobs = lapply(c("SPM", "SnPM", "Effect"), function(label) {
    textGrob(label)
  }),
  ncol = 3
)

# Combine column labels and plots
grid_with_labels <- arrangeGrob(
  column_labels, grid_plots,
  ncol = 1,
  heights = unit.c(unit(0.5, "cm"), unit(0.9, "npc"))
)

# Add shared legend at the bottom
final_plot <- plot_grid(
  grid_with_labels, cowplot::plot_grid(shared_legend),
  ncol = 1, rel_heights = c(0.9, 0.1)
)

# Display the final plot
grid.newpage()
grid.draw(final_plot)


# ggsave("Outputs/PaperPlots/Plot2_sd1effect1.jpeg", final_plot, width = 190, height = 196, units = "mm", dpi = 1200)
# ggsave("Outputs/PaperPlots/Plot2_sd1effect1.tiff", final_plot, width = 190, height = 196, units = "mm", dpi = 1200, device = "tiff")





##############sensetivity############


###############################################################
data_square <- read_excel("Outputs/excel_results/Simulation_Square_NoiseSD_1_Maxeffect_1.xlsx")
data_dsquare <- read_excel("Outputs/excel_results/Simulation_dSquare_NoiseSD_1_Maxeffect_1.xlsx")
data_G <- read_excel("Outputs/excel_results/Simulation_G_NoiseSD_1_Maxeffect_1.xlsx")

#plotting the power curves
PS_1_SPM <- sensetivity_plot_SPM(data_square,Title="")
PS_1_SnPM <- sensetivity_plot_SnPM(data_square,Title="")
PS_3_SPM <- sensetivity_plot_SPM(data_dsquare,Title="")
PS_3_SnPM <- sensetivity_plot_SnPM(data_dsquare,Title="")
PS_9_SPM <- sensetivity_plot_SPM(data_G,Title="")
PS_9_SnPM <- sensetivity_plot_SnPM(data_G,Title="")
Pulse_Sq1 <- Pulse_plot(Pulse_long)
Pulse_Sq2 <- Pulse_plot(Pulse_long_two_square)
Pulse_GP <- Pulse_plot(Pulse_long_GP,Title_legend="SFWHM %")

library(gridExtra)
grid.arrange(PS_1_SPM,PS_1_SnPM,Pulse_Sq1,PS_3_SPM,PS_3_SnPM,Pulse_Sq2,PS_9_SPM,PS_9_SnPM,Pulse_GP,ncol=3)



library(gridExtra)
library(grid)
#install.packages("cowplot")
library(cowplot)
library(ggplot2)


# Create a legend using one of the plots
legend_plot <- power_plot_SPM(data_square, Title = "", legend = "bottom")
# Convert the plot to a grob object
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

# Combine plots without legend
plots <- list(PS_1_SPM, PS_1_SnPM, Pulse_Sq1,
              PS_3_SPM, PS_3_SnPM, Pulse_Sq2,
              PS_9_SPM, PS_9_SnPM, Pulse_GP)

# Arrange plots in a grid
grid_plots <- arrangeGrob(grobs = plots, ncol = 3)

# Add column labels
column_labels <- arrangeGrob(
  grobs = lapply(c("SPM", "SnPM", "Effect"), function(label) {
    textGrob(label)
  }),
  ncol = 3
)

# Combine column labels and plots
grid_with_labels <- arrangeGrob(
  column_labels, grid_plots,
  ncol = 1,
  heights = unit.c(unit(0.5, "cm"), unit(0.9, "npc"))
)

# Add shared legend at the bottom
final_plot <- plot_grid(
  grid_with_labels, cowplot::plot_grid(shared_legend),
  ncol = 1, rel_heights = c(0.9, 0.1)
)

# Display the final plot
grid.newpage()
grid.draw(final_plot)

# ggsave("Outputs/PaperPlots/Plot3_sd1effect1.jpeg", final_plot, width = 190, height = 196, units = "mm", dpi = 1200)
# ggsave("Outputs/PaperPlots/Plot3_sd1effect1.tiff", final_plot, width = 190, height = 196, units = "mm", dpi = 1200, device = "tiff")





















##############FWHM############

data_FWHM <- function(result_data,Ylim=c(0,65),Title="", legend="none"){
  #plot different curves representing the Geom_percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
  ggplot(result_data, aes(x = NFWHM, y = Est_dataFWHM, color = as.factor(Geom_percentage))) +
    geom_line() + geom_point() +
    # Add diagonal dashed line from (0,0) to (x,x)
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    theme_minimal() +
    labs(title = Title,
         x = "Noise FWHM", y = "Estimated Data FWHM", color = "%") +
    #ylim between 0 and 1
    scale_y_continuous(limits = Ylim) +
    # remove legend
    theme(legend.position = legend) +
    guides(colour = guide_legend(nrow = 1))
}
Noise_FWHM <- function(result_data,Ylim=c(0,65),Title="", legend="none"){
  #plot different curves representing the Geom_percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
  ggplot(result_data, aes(x = NFWHM, y = Est_NFWHM, color = as.factor(Geom_percentage))) +
    geom_line() + geom_point() +
    # Add diagonal dashed line from (0,0) to (x,x)
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    theme_minimal() +
    labs(title = Title,
         x = "Noise FWHM", y = "Estimated Noise FWHM", color = "%") +
    #ylim between 0 and 1
    scale_y_continuous(limits = Ylim) +
    # remove legend
    theme(legend.position = legend) +
    guides(colour = guide_legend(nrow = 2))
}

###############################################################
data_square <- read_excel("Outputs/excel_results/Simulation_Square_NoiseSD_1_Maxeffect_1_EstimatedFWHMs.xlsx")
data_dsquare <- read_excel("Outputs/excel_results/Simulation_dSquare_NoiseSD_1_Maxeffect_1_EstimatedFWHMs.xlsx")
data_G <- read_excel("Outputs/excel_results/Simulation_G_NoiseSD_1_Maxeffect_1_EstimatedFWHMs.xlsx")

#plotting the power curves
PS_1_SPM <- Noise_FWHM(data_square,Title="")
PS_1_SnPM <- data_FWHM(data_square,Title="")
PS_3_SPM <- Noise_FWHM(data_dsquare,Title="")
PS_3_SnPM <- data_FWHM(data_dsquare,Title="")
PS_9_SPM <- Noise_FWHM(data_G,Title="")
PS_9_SnPM <- data_FWHM(data_G,Title="")
Pulse_Sq1 <- Pulse_plot(Pulse_long)
Pulse_Sq2 <- Pulse_plot(Pulse_long_two_square)
Pulse_GP <- Pulse_plot(Pulse_long_GP,Title_legend="SFWHM %")

library(gridExtra)
grid.arrange(PS_1_SPM,PS_1_SnPM,Pulse_Sq1,PS_3_SPM,PS_3_SnPM,Pulse_Sq2,PS_9_SPM,PS_9_SnPM,Pulse_GP,ncol=3)



library(gridExtra)
library(grid)
#install.packages("cowplot")
library(cowplot)
library(ggplot2)


# Create a legend using one of the plots
legend_plot <- Noise_FWHM(data_square, Title = "", legend = "bottom")
# Convert the plot to a grob object
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

# Combine plots without legend
plots <- list(PS_1_SPM, PS_1_SnPM, Pulse_Sq1,
              PS_3_SPM, PS_3_SnPM, Pulse_Sq2,
              PS_9_SPM, PS_9_SnPM, Pulse_GP)

# Arrange plots in a grid
grid_plots <- arrangeGrob(grobs = plots, ncol = 3)

# Add column labels
column_labels <- arrangeGrob(
  grobs = lapply(c("Estimated Noise FWHM", "Estimated Data FWHM", "Geometry"), function(label) {
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
#ggsave("/Users/more0056/Desktop/FWHMs.jpeg", final_plot, width = 190, height = 196, units = "mm", dpi = 600)
