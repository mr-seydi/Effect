source("basic_functions.R")
library(readxl)
library(tidyr)
library(ggplot2)
power_plot_SnPM <- function(result_data,Ylim=c(0,1),Title="", legend="none"){
  #plot different curves representing the percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
  ggplot(result_data, aes(x = noise_fwhm, y = Nonparametric_SPM, color = as.factor(percentage))) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = Title,
         x = "Noise FWHM", y = "Power", color = "%") +
    #ylim between 0 and 1
    scale_y_continuous(limits = Ylim) +
    # remove legend
    theme(legend.position = legend) +
    guides(colour = guide_legend(nrow = 1))
}
power_plot_SPM <- function(result_data,Ylim=c(0,1),Title="", legend="none"){
  #plot different curves representing the percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
  ggplot(result_data, aes(x = noise_fwhm, y = Parametric_SPM, color = as.factor(percentage))) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = Title,
         x = "Noise FWHM", y = "Power", color = "%") +
    #ylim between 0 and 1
    scale_y_continuous(limits = Ylim) +
    # remove legend
    theme(legend.position = legend) +
    guides(colour = guide_legend(nrow = 2))
}

Pulse_plot <- function(Puls_data_long,Title_legend="Domain %"){
  ggplot(Puls_data_long, aes(x = Domain, y = Effect, group = Curve, linetype = Curve)) +
    geom_line(color = "black", linewidth = 0.3) +  # Use black color for all lines
    theme_classic() +
    # x-axis values 0, 50, 100
    scale_x_continuous(breaks = c(0, 50, 100)) +
    labs(title = "",
         x = "Domain", y = "Effect size", linetype = Title_legend) +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                          labels = c("5", "50", "100")) # Change labels # Custom line types
}

###############################################################
data_1 <- read_excel("results/Simulation1_out.xlsx")
data_3 <- read_excel("results/Simulation_SqP1.xlsx")
data_9 <- read_excel("results/Simulation_GP1_out.xlsx")
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
PS_1_SPM <- power_plot_SPM(data_1,Title="")
PS_1_SnPM <- power_plot_SnPM(data_1,Title="")
PS_3_SPM <- power_plot_SPM(data_3,Title="")
PS_3_SnPM <- power_plot_SnPM(data_3,Title="")
PS_9_SPM <- power_plot_SPM(data_9,Title="")
PS_9_SnPM <- power_plot_SnPM(data_9,Title="")
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
legend_plot <- power_plot_SPM(data_1, Title = "", legend = "bottom")
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

ggsave("results/Plots/plot_final1.jpeg", final_plot, width = 190, height = 196, units = "mm", dpi = 600)
