# read the data
#install.packages("readxl")
library(readxl)
data_1 <- read_excel("results/Simulation1_out.xlsx")
data_2 <- read_excel("results/Simulation2_out.xlsx")
data_3 <- read_excel("results/Simulation_SqP1.xlsx")
data_4 <- read_excel("results/Simulation_SqP2.xlsx")
data_5 <- read_excel("results/Simulation1half_out.xlsx")
data_6 <- read_excel("results/Simulation_SqP1half.xlsx")
data_7 <- read_excel("results/Simulationhalf_out.xlsx")
data_8 <- read_excel("results/Simulation_SqPhalf.xlsx")
data_9 <- read_excel("results/Simulation_GP1_out.xlsx")
data_10 <- read_excel("results/Simulation_GP1half_out.xlsx")
data_11 <- read_excel("results/Simulation_GPhalf_out.xlsx")
#####plot#####
#install.packages("ggplot2")
library(ggplot2)

power_plot <- function(result_data,Ylim=c(0,1),Title=""){
  #plot different curves representing the percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
  ggplot(result_data, aes(x = noise_fwhm, y = Nonparametric_SPM, color = as.factor(percentage))) +
    geom_line() + geom_point() + theme_minimal() +
    labs(title = Title,
         x = "Noise FWHM", y = "Power", color = "Domain %") +
    #ylim between 0 and 1
    scale_y_continuous(limits = Ylim) 
}


power_plot(data_1,Title="Square Pulse with max 1")
power_plot(data_2,Title="Square Pulse with max 2")
power_plot(data_3,Title="Two Square Pulse with max 1 and min -1")
power_plot(data_4,Title="Two Square Pulses with max 2 and min -2")
power_plot(data_5,Title="Square Pulse with max 1.5")
power_plot(data_6,Ylim=c(0,1),Title = "Two Square Pulses with max 1.5 and min -1.5")
power_plot(data_7,Ylim = c(0,1),Title="Square Pulse with max 0.5")
power_plot(data_8,Ylim = c(0,1),Title = "Two Square Pulses with max 0.5 and min -0.5")
power_plot(data_9,Ylim = c(0,1),Title = "Gaussian Pulse with max 1")
power_plot(data_10,Ylim = c(0,1),Title = "Gaussian Pulse with max 1.5")
power_plot(data_11,Ylim = c(0,1),Title = "Gaussian Pulse with max 0.5")
########plot effect#########
source("basic_functions.R")
effect1 <- centered_ranges (seq(5, 100, by = 5))
Pulse <- matrix(0, nrow = 101 , ncol = nrow(effect1))
for (i in 1:nrow(effect1)){
  Pulse[,i] <- square_pulse (start_end_pulse=c(effect1$Start[i],effect1$End[i]),
                         start_height=0, pulse_height=1)
}
#plot all column of the Pulse matrix as curves with x axis as "Domain" form 0 to 100
#and y axis as "Effect" from -1 to 1
library(ggplot2)
library(tidyr)

# Example: Assuming Pulse is a matrix with columns representing different curves
# Convert to data frame and reshape
Pulse_long <- data.frame(Domain = 0:100, Pulse) %>%
  pivot_longer(-Domain, names_to = "Curve", values_to = "Effect")
# Add a curve order column based on the column order in the matrix
Pulse_long$Curve <- factor(Pulse_long$Curve, levels = unique(Pulse_long$Curve))
Pulse_long$order <- as.numeric(Pulse_long$Curve) # Order for transparency and size mapping

Pulse_long <- filter(Pulse_long, Curve %in% c("X1", "X10", "X20")) # Filter to show only a few curves)
# Plot
ggplot(Pulse_long, aes(x = Domain, y = Effect, group = Curve, linetype = Curve)) +
  geom_line(color = "black", size = 1) +  # Use black color for all lines
  theme_classic() +
  labs(title = "",
       x = "Domain", y = "Effect size", linetype = "Domain %") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        labels = c("5", "50", "100")) # Change labels # Custom line types


######################################################
source("basic_functions.R")
left<-centered_ranges(seq(5, 100, by = 5), domain=c(0,50))
right<-centered_ranges(seq(5, 100, by = 5), domain=c(50,100))
centered_ranges <- cbind(left,right)
Pulse <- matrix(0, nrow = 101 , ncol = nrow(effect1))
for (i in 1:nrow(effect1)){
  one_side <- square_pulse(start_end_pulse=c(left$Start[i],left$End[i]), start_height=0,
                           pulse_height=1)
  other_side <- square_pulse(start_end_pulse=c(right$Start[i],right$End[i]), start_height=0,
                             pulse_height=-1)
  Pulse[,i] <- one_side + other_side
}

# Convert to data frame and reshape
Pulse_long <- data.frame(Domain = 0:100, Pulse) %>%
  pivot_longer(-Domain, names_to = "Curve", values_to = "Effect")
# Add a curve order column based on the column order in the matrix
Pulse_long$Curve <- factor(Pulse_long$Curve, levels = unique(Pulse_long$Curve))
Pulse_long$order <- as.numeric(Pulse_long$Curve) # Order for transparency and size mapping

Pulse_long <- filter(Pulse_long, Curve %in% c("X1", "X10", "X20")) # Filter to show only a few curves)
# Plot
ggplot(Pulse_long, aes(x = Domain, y = Effect, group = Curve, linetype = Curve)) +
  geom_line(color = "black", size = 1) +  # Use black color for all lines
  theme_classic() +
  labs(title = "",
       x = "Domain", y = "Effect size", linetype = "Domain %") +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_linetype_manual(values = c("dotted", "dashed", "solid"),
                        labels = c("5", "50", "100")) # Change labels # Custom line types



