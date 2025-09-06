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

NFWHM_10_curve <- Noise_plot(NFWHM_10_1) + ylim(c(-3.3,3.3))
NFWHM_30_curve <- Noise_plot(NFWHM_30_1) + ylim(c(-3.3,3.3))
NFWHM_50_curve <- Noise_plot(NFWHM_50_1) + ylim(c(-3.3,3.3))

SP <- square_pulse (start_end_pulse=c(47.5,52.5),
              start_height=0, pulse_height=1)


one_side <- square_pulse(start_end_pulse=c(12.50, 37.50), start_height=0,
                         pulse_height=1)
other_side <- square_pulse(start_end_pulse=c(62.50, 87.50), start_height=0,
                           pulse_height=-1)
Pulse_two_square <- one_side + other_side

P_G <- gaussian_pulse(center=50, fwhm=100, continuum_size=101)
P_G <- amplitude_pulse(data=P_G$density_val, amp=1)

SP_signal_plot <- pulse_single_plot(signal_data=SP, legend_name="5%", legend_title="Domain")  + ylim(c(-3.3,3.3))

P2S_signal_plot <- pulse_single_plot(signal_data=Pulse_two_square, legend_name="50%", legend_title="Domain") + ylim(c(-3.3,3.3))

PG_signal_plot <- pulse_single_plot(signal_data=P_G, legend_name="100%", legend_title="SFWHM") +ylim(c(-3.3,3.3))

  

SP_plot <- sample_plot(data_type = "baseline", Org_data = NULL,
                       Signal_curve = SP, noise1_data = NFWHM_10_1,
                        noise2_data = NFWHM_10_2,Title = "", legend = "none") + ylim(c(-3.3,3.3))
P2S_plot <- sample_plot(data_type = "baseline", Org_data = NULL,
                        Signal_curve = Pulse_two_square, noise1_data = NFWHM_30_1,
                        noise2_data = NFWHM_30_2,Title = "", legend = "none") + ylim(c(-3.3,3.3))
PG_plot <- sample_plot(data_type = "baseline", Org_data = NULL,
                       Signal_curve = P_G, noise1_data = NFWHM_50_1,
                       noise2_data = NFWHM_50_2,Title = "", legend = "none") + ylim(c(-3.3,3.3))


####################
library(gridExtra)
library(grid)
#install.packages("cowplot")
library(cowplot)
library(ggplot2)


# Create a legend using one of the plots
legend_plot <-   sample_plot(data_type = "baseline", Org_data = NULL,
                        Signal_curve = SP, noise1_data = NFWHM_10_1,
                        noise2_data = NFWHM_10_2,Title = "", legend = "bottom")
# Convert the plot to a grob object
grob_legend <- ggplotGrob(legend_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]

# Combine plots without legend
plots <- list(NFWHM_10_curve, SP_signal_plot, SP_plot,
              NFWHM_30_curve, P2S_signal_plot, P2S_plot,
              NFWHM_50_curve, PG_signal_plot, PG_plot)

# Arrange plots in a grid
grid_plots <- arrangeGrob(grobs = plots, ncol = 3)


# Add column labels with manual horizontal adjustment for specific columns
column_labels <- arrangeGrob(
  textGrob("Noise", hjust = -0.6),  # Adjust horizontally to the left
  textGrob("Effect", just = "centre"),     # Default position (centered)
  textGrob("Sample", just = "centre"), # Adjust horizontally to the right
  ncol = 3
)

# Add row labels
row_labels <- arrangeGrob(
  grobs = lapply(c("Noise FWHM = 10", "Noise FWHM = 30", "Noise FWHM = 50"), function(label) {
    textGrob(label, rot = 90)
  }),
  nrow = 3
)

# Combine row labels and plots
grid_with_labels <- arrangeGrob(
  row_labels, grid_plots,
  ncol = 2,
  widths = unit.c(unit(1, "cm"), unit(0.9, "npc"))
)

# Combine column labels, plots with row labels, and shared legend
final_plot <- plot_grid(
  arrangeGrob(column_labels, grid_with_labels, ncol = 1, heights = unit.c(unit(1, "cm"), unit(0.9, "npc"))),
  cowplot::plot_grid(shared_legend),
  ncol = 1, rel_heights = c(0.95, 0.05)
)

# Display the final plot
grid.newpage()
grid.draw(final_plot)

#ggsave("Outputs/PaperPlots/Plot1_SamplePlots.jpeg", final_plot, width = 190, height = 196, units = "mm", dpi = 1200)
#ggsave("Outputs/PaperPlots/Plot1_SamplePlots.tiff", final_plot, width = 190, height = 196, units = "mm", dpi = 1200, device = "tiff")



