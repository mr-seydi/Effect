source("basic_functions.R")
source("Data_functions.R")
library(readxl)
real_data_results <- read_excel("results/Simulation_realdata_sdMax(geom).xlsx")
#change vGRFPhan to vGRF in geom column
real_data_results$geom <- gsub("vGRFPhan", "VGRF", real_data_results$geom)
real_data_results$geom <- gsub("Angle", "HFA", real_data_results$geom)
real_data_results$geom <- gsub("Moment", "KJM", real_data_results$geom)


Data_plot(vGRF_data_Phan("both"),TITLE = "VGRF")
Data_plot(JCF_data("both"),TITLE = "JCF")
Data_plot(Angle_data("both"),TITLE = "HFA")
Data_plot(Moment_data("both")[, c(2, 1)],TITLE = "KJM")
Data_plot(MF_data("both")[, c(2, 1)],TITLE = "MF")
Data_plot(EMG_data("both")[, c(2, 1)],TITLE = "EMG")


##############

library(ggplot2)

# Plot for Parametric SPM
parametric_plot <- ggplot(real_data_results, aes(x = noise_fwhm, y = Parametric_SPM, color = geom)) +
  geom_line(linewidth = 1) +
  geom_point() + theme_minimal() +
  labs(
    title = "SPM",
    x = "Noise FWHM",
    y = "Power",
    color = "Geometry"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  # remove legend
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
  

# Plot for Nonparametric SPM
nonparametric_plot <- ggplot(real_data_results, aes(x = noise_fwhm, y = Nonparametric_SPM, color = geom)) +
  geom_line(linewidth = 1) +
  geom_point() + theme_minimal() +
  labs(
    title = "SnPM",
    x = "Noise FWHM",
    y = "Power",
    color = "Geometry"
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  # remove legend
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

# Print the plots (in an interactive session, they will display separately)
print(parametric_plot)
print(nonparametric_plot)


########################
library(ggplot2)
library(cowplot)

## -------------------------------
## Row 1: Two SPM Plots (Parametric & Nonparametric)
## -------------------------------

# Remove individual legends
p1 <- parametric_plot + theme(legend.position = "none")
p2 <- nonparametric_plot + theme(legend.position = "none")

# Extract a common legend from one of the plots (they share the same aesthetic)
grob_legend <- ggplotGrob(parametric_plot)

# Extract legend component from the gtable
legend_index <- which(sapply(grob_legend$grobs, function(x) x$name) == "guide-box")
shared_legend <- grob_legend$grobs[[legend_index]]


# Arrange the two plots side by side and add the common legend below
row1 <- plot_grid(p1, p2, ncol = 2)
row1_with_legend <- plot_grid(row1, shared_legend, ncol = 1, rel_heights = c(1, 0.2))

## -------------------------------
## Row 2: Three Data Plots (vGRF, JCF, Angle)
## -------------------------------



# Create the three plots and remove their legends
p3 <- Data_plot(EMG_data("both")[, c(2, 1)], TITLE = "EMG") + theme(legend.position = "none")
p4 <- Data_plot(Angle_data("both"), TITLE = "HFA") + theme(legend.position = "none")
p5 <- Data_plot(JCF_data("both"), TITLE = "JCF") + theme(legend.position = "none")


# Arrange the three plots and add the legend below
row2 <- plot_grid(p3, p4, p5, ncol = 3)
row2_without_legend <- plot_grid(row2, ncol = 1, rel_heights = c(1, 0.2))

## -------------------------------
## Row 3: Three Data Plots (Moment, MF, EMG)
## -------------------------------

# Create the three plots and remove their legends
p6 <- Data_plot(Moment_data("both")[, c(2, 1)], TITLE = "KJM") + theme(legend.position = "none")
p7 <- Data_plot(MF_data("both")[, c(2, 1)], TITLE = "MF") + theme(legend.position = "none")
p8 <- Data_plot(vGRF_data_Phan("both"), TITLE = "VGRF") + theme(legend.position = "none")



# Extract a common legend from one of the plots (they share the same aesthetic)
grob_legend_3 <- ggplotGrob(Data_plot(Moment_data("both")[, c(2, 1)], TITLE = "Moment") +
                              theme(legend.position = "bottom"))

# Extract legend component from the gtable
legend_index_3 <- which(sapply(grob_legend_3$grobs, function(x) x$name) == "guide-box")
shared_legend_3 <- grob_legend_3$grobs[[legend_index_3]]


# Arrange the three plots and add the legend below
row3 <- plot_grid(p6, p7, p8, ncol = 3)
row3_with_legend <- plot_grid(row3, shared_legend_3, ncol = 1, rel_heights = c(1, 0.2))

## -------------------------------
## Combine All Rows into a Single Layout
## -------------------------------

final_plot <- plot_grid(row1_with_legend, row2_without_legend, row3_with_legend, ncol = 1)

# Display the final plot
print(final_plot)
ggsave("results/Plots/plot_real_data.jpeg", final_plot, width = 190, height = 196, units = "mm", dpi = 600)
