source("R/Plot_functions.R")
source("R/sources.R")

# D1_prime <- list(
#   c(4,14), # vGRFPhan
#   c(13,30), # JCF
#   c(1,101), # Angle
#   c(11,92), # Moment
#   c(29,55), # MF
#   rbind(c(1,5),c(26,48),c(94,101)) # EMG
# )

Data_plot(vGRF_data_Phan("both"),TITLE = "VGRF")
Data_plot(JCF_data("both"),TITLE = "JCF")
Data_plot(Angle_data("both"),TITLE = "HFA")
Data_plot(Moment_data("both")[, c(2, 1)],TITLE = "KJM")
Data_plot(MF_data("both")[, c(2, 1)],TITLE = "MF")
Data_plot(EMG_data("both")[, c(2, 1)],TITLE = "EMG")

# Data_plot(vGRF_data_Phan("both"),TITLE = "VGRF", interval= D1_prime[[1]])
# Data_plot(JCF_data("both"),TITLE = "JCF", interval = D1_prime[[2]])
# Data_plot(Angle_data("both"),TITLE = "HFA", interval = D1_prime[[3]])
# Data_plot(Moment_data("both")[, c(2, 1)],TITLE = "KJM", interval = D1_prime[[4]])
# Data_plot(MF_data("both")[, c(2, 1)],TITLE = "MF", interval = D1_prime[[5]])
# Data_plot(EMG_data("both")[, c(2, 1)],TITLE = "EMG", interval = D1_prime[[6]])



##############

library(ggplot2)


########################
library(ggplot2)
library(cowplot)

## -------------------------------
## Row 2: Three Data Plots (vGRF, JCF, Angle)
## -------------------------------

p3 <- Data_plot(EMG_data("both")[, c(2, 1)], TITLE = "EMG") + theme(legend.position = "none")
p4 <- Data_plot(Angle_data("both"), TITLE = "HFA") + theme(legend.position = "none")
p5 <- Data_plot(JCF_data("both"), TITLE = "JCF") + theme(legend.position = "none")

# Create the three plots and remove their legends
# p3 <- Data_plot(EMG_data("both")[, c(2, 1)], TITLE = "EMG", interval = D1_prime[[6]]) + theme(legend.position = "none")
# p4 <- Data_plot(Angle_data("both"), TITLE = "HFA", interval = D1_prime[[3]] ) + theme(legend.position = "none")
# p5 <- Data_plot(JCF_data("both"), TITLE = "JCF", interval = D1_prime[[2]]) + theme(legend.position = "none")


# Arrange the three plots and add the legend below
row2 <- plot_grid(p3, p4, p5, ncol = 3)
row2_without_legend <- plot_grid(row2, ncol = 1, rel_heights = c(1, 0.2))

## -------------------------------
## Row 3: Three Data Plots (Moment, MF, EMG)
## -------------------------------


p6 <- Data_plot(Moment_data("both")[, c(2, 1)], TITLE = "KJM") + theme(legend.position = "none")
p7 <- Data_plot(MF_data("both")[, c(2, 1)], TITLE = "MF") + theme(legend.position = "none")
p8 <- Data_plot(vGRF_data_Phan("both"), TITLE = "vGRF") + theme(legend.position = "none")

# Create the three plots and remove their legends
# p6 <- Data_plot(Moment_data("both")[, c(2, 1)], TITLE = "KJM", interval = D1_prime[[4]]) + theme(legend.position = "none")
# p7 <- Data_plot(MF_data("both")[, c(2, 1)], TITLE = "MF", interval = D1_prime[[5]]) + theme(legend.position = "none")
# p8 <- Data_plot(vGRF_data_Phan("both"), TITLE = "vGRF", interval = D1_prime[[1]]) + theme(legend.position = "none")



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

final_plot <- plot_grid(row2_without_legend, row3_with_legend, ncol = 1)

# Display the final plot
print(final_plot)
ggsave("Outputs/Plots/ISB_data_new1.jpeg", final_plot, width = 190, height = 120, units = "mm", dpi = 1200)
