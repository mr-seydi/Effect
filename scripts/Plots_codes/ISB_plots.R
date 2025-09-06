source("R/Sources.R")
set.seed(123)
Noise_data_1 <- noise_guassian_curve(number_of_curves=10, continuum_size=101)
NFWHM_10_1 <- smoothed_gussian_curves(data=Noise_data_1, mu=0, sig=1, fwhm=10)
NFWHM_30_1 <- smoothed_gussian_curves(data=Noise_data_1, mu=0, sig=1, fwhm=30)
NFWHM_50_1 <- smoothed_gussian_curves(data=Noise_data_1, mu=0, sig=1, fwhm=50)

Noise_data_2 <- noise_guassian_curve(number_of_curves=10, continuum_size=101)
NFWHM_10_2 <- smoothed_gussian_curves(data=Noise_data_2, mu=0, sig=1, fwhm=10)
NFWHM_30_2 <- smoothed_gussian_curves(data=Noise_data_2, mu=0, sig=1, fwhm=30)
NFWHM_50_2 <- smoothed_gussian_curves(data=Noise_data_2, mu=0, sig=1, fwhm=50)


P_G <- gaussian_pulse(center=50, fwhm=50, continuum_size=101)
P_G <- amplitude_pulse(data=P_G$density_val, amp=1)



PG_plot1 <- sample_plot(data_type = "baseline", Org_data = NULL,
                       Signal_curve = P_G, noise1_data = NFWHM_10_1,
                       noise2_data = NFWHM_10_2,Title = "", legend = "none",xlab = "")
PG_plot2 <- sample_plot(data_type = "baseline", Org_data = NULL,
                        Signal_curve = P_G, noise1_data = NFWHM_30_1,
                        noise2_data = NFWHM_30_2,Title = "", legend = "none",ylab="")
PG_plot3 <- sample_plot(data_type = "baseline", Org_data = NULL,
                       Signal_curve = P_G, noise1_data = NFWHM_50_1,
                       noise2_data = NFWHM_50_2,Title = "", legend = "none",xlab = "",ylab="")


####################
library(gridExtra)
library(grid)
#install.packages("cowplot")
library(cowplot)
library(ggplot2)


# Create a legend using one of the plots
legend_plot <-   sample_plot(data_type = "baseline", Org_data = NULL,
                             Signal_curve = P_G, noise1_data = NFWHM_10_1,
                             noise2_data = NFWHM_10_2,Title = "", legend = "bottom")
# Convert the plot to a grob object
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

# Combine plots without legend
plots <- list(PG_plot1, PG_plot2, PG_plot3)

# Arrange plots in a grid
grid_plots <- arrangeGrob(grobs = plots, ncol = 3)


# Add column labels with manual horizontal adjustment for specific columns
column_labels <- arrangeGrob(
  textGrob("NFWHM = 10", gp = gpar(fontsize = 12, fontface = "bold"), hjust = "centre"),  # Adjust horizontally to the left
  textGrob("NFWHM = 30", gp = gpar(fontsize = 12, fontface = "bold"), just = "centre"),     # Default position (centered)
  textGrob("NFWHM = 50", gp = gpar(fontsize = 12, fontface = "bold"), just = "centre"), # Adjust horizontally to the right
  ncol = 3
)

# combine the plots and the legend
final_plot <- plot_grid(column_labels, grid_plots, shared_legend, ncol = 1, rel_heights = c(0.1, 1, 0.1))

# Display the final plot
grid.newpage()
grid.draw(final_plot)



############################
power_plot_SPM <- function(result_data,Ylim=c(0,1),Title="", legend="none"){
  #plot different curves representing the percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
  ggplot(result_data, aes(x = noise_fwhm, y = Parametric_SPM, color = as.factor(percentage))) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = Title,
         x = "NFWHM", y = "Power", color = "SFWHM %") +
    #ylim between 0 and 1
    scale_y_continuous(limits = Ylim) +
    # remove legend
    theme(legend.position = legend) +
    guides(colour = guide_legend(nrow = 2))
}
Pulse_plot <- function(Puls_data_long, Title_legend = "Domain %") {
  ggplot(Puls_data_long, aes(x = Domain, y = Effect, group = Curve,
                             #linetype = Curve,
                             color = Curve)) +
    geom_line(linewidth = 0.6) +  # Allow colors to be mapped dynamically
    theme_classic() +
    # x-axis values 0, 50, 100
    scale_x_continuous(breaks = c(0, 50, 100)) +
    labs(title = "",
         x = "Domain", y = "Effect size",
         #linetype = Title_legend,
         color = Title_legend) +
    scale_y_continuous(limits = c(0, 1)) +
    #scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                          #labels = c("5", "50", "100")) + # Custom line types
    scale_color_manual(values = c("#F8766D", "#00C1A3", "#FF6A98"),  # Custom colors
                       labels = c("5", "50", "100")) + # Ensure labels match
    theme(legend.position = "none")
}
#colors
# "#F8766D" "#EA8331" "#D89000" "#C09B00" "#A3A500" "#7CAE00" "#39B600" "#00BB4E" "#00BF7D" "#00C1A3" "#00BFC4"
#"#00BAE0" "#00B0F6" "#35A2FF" "#9590FF" "#C77CFF" "#E76BF3" "#FA62DB" "#FF62BC" "#FF6A98"

data_9 <- read_excel("results/Simulation_GP1_out.xlsx")

#####################
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
######################
PS_9_SPM <- power_plot_SPM(data_9,Title="")
Pulse_GP <- Pulse_plot(Pulse_long_GP,Title_legend="SFWHM %")

library(gridExtra)
grid.arrange(PS_9_SPM,Pulse_GP,ncol=2)



library(gridExtra)
library(grid)
#install.packages("cowplot")
library(cowplot)
library(ggplot2)


# Create a legend using one of the plots
legend_plot <- power_plot_SPM(data_9, Title = "", legend = "bottom")
# Convert the plot to a grob object
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

# Combine plots without legend
plots <- list(PS_9_SPM, Pulse_GP)

# Arrange plots in a grid
grid_plots <- arrangeGrob(grobs = plots, ncol = 2)

# Add column labels
column_labels <- arrangeGrob(
  grobs = lapply(c("SPM", "Geometry"), function(label) {
    textGrob(label, gp = gpar(fontsize = 16, fontface = "bold"))
  }),
  ncol = 2
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
