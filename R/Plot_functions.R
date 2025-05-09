#################plots####################
# power_plot_SnPM <- function(result_data,Ylim=c(0,1),Title="", legend="none"){
#   #plot different curves representing the percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
#   ggplot(result_data, aes(x = noise_fwhm, y = Nonparametric_SPM, color = as.factor(percentage))) +
#     geom_line() + geom_point() + theme_minimal() +
#     labs(title = Title,
#          x = "Noise FWHM", y = "Power", color = "%") +
#     #ylim between 0 and 1
#     scale_y_continuous(limits = Ylim) +
#     # remove legend
#     theme(legend.position = legend) +
#     guides(colour = guide_legend(nrow = 1))
# }
# power_plot_SPM <- function(result_data,Ylim=c(0,1),Title="", legend="none"){
#   #plot different curves representing the percentage column with x-axis as the noise fwhm and y-axis as the Parametric_SPM
#   ggplot(result_data, aes(x = noise_fwhm, y = Parametric_SPM, color = as.factor(percentage))) +
#     geom_line() + geom_point() + theme_minimal() +
#     labs(title = Title,
#          x = "Noise FWHM", y = "Power", color = "%") +
#     #ylim between 0 and 1
#     scale_y_continuous(limits = Ylim) +
#     # remove legend
#     theme(legend.position = legend) +
#     guides(colour = guide_legend(nrow = 2))
# }

# Pulse_plot <- function(Puls_data_long,Title_legend="Domain %",Ylim=c(-1,1)){
#   ggplot(Puls_data_long, aes(x = Domain, y = Effect, group = Curve, linetype = Curve)) +
#     geom_line(color = "black", linewidth = 0.3) +  # Use black color for all lines
#     scale_x_continuous(breaks = c(0,50,100)) +
#     theme_classic() +
#     labs(title = "",
#          x = "Domain", y = "Effect size", linetype = Title_legend) +
#     scale_y_continuous(limits = Ylim,breaks = seq(Ylim[1],Ylim[2],0.5)) +
#     scale_linetype_manual(values = c("dotted", "dashed", "solid"),
#                           labels = c("5", "50", "100")) # Change labels # Custom line types
# }

Noise_plot <- function(Noise_curves, Title=""){
  cont_size = dim(Noise_curves)[1]
  n_curves = dim(Noise_curves)[2]
  # Create a data frame for ggplot
  plot_data <- data.frame(
    x_values = rep(0:(cont_size - 1), n_curves),
    y_values = as.vector(Noise_curves),  # Flatten the matrix
    line_group = factor(rep(1:n_curves, each = cont_size))  # Create a group for each column
  )
  
  # Generate the plot using ggplot2
  ggplot(plot_data, aes(x = x_values, y = y_values, group = line_group, color = line_group)) +
    geom_line(linewidth = 1) +  # Set line thickness
    scale_color_manual(values = colorRampPalette(c("darkblue",
                                                   "lightblue"))(n_curves),
                       labels = c(1:n_curves)) +  # Navy blue shades
    labs(title = Title, x = "Domain", y = "Value") +  # Add labels
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
    theme(legend.position = "none")+
    #increase the font size of the labels and axis numbers
    theme(
      # axis.text.x = element_text(size = 12),
      # axis.text.y = element_text(size = 12),
      # axis.title.x = element_text(size = 14),
      # axis.title.y = element_text(size = 14),
      # plot.title = element_text(size = 16),
      #remove legend title
      legend.title = element_blank())
}



sample_plot <- function(data_type = "baseline", Org_data = NULL, Signal_curve,
                        noise1_data, noise2_data, Title="", legend="none", xlab="Domain", ylab="Value"){
  
  if (data_type == "baseline") {
    # Generate data with and without pulse
    if(is.null(Org_data)){
      Sample1 <- data_generator(signal = Signal_curve, noise = noise1_data)
      Sample2 <- data_generator(noise = noise2_data)
    } else {
      Sample1 <- data_generator(data = Org_data, signal = Signal_curve, noise = noise1_data)
      Sample2 <- data_generator(data = Org_data, noise = noise2_data)
    }
    
    sample_label_1 <- "Group 1"
    sample_label_2 <- "Group 2"
    
    colors_plot_data <- setNames(c("tomato", "cadetblue"),
                                 c(sample_label_1, 
                                   sample_label_2))
    
  } else {
    # For two_sample case
    
    Sample1 <- data_generator(data = Org_data[, 1], noise = noise1_data)
    Sample2 <- data_generator(data = Org_data[, 2], noise = noise2_data)
    
    # Use the same labels as in pulse_plot
    sample_label_1 <- colnames(Org_data)[1]
    sample_label_2 <- colnames(Org_data)[2]
    
    colors_plot_data <- setNames(c("tomato", "cadetblue"),
                                 c(sample_label_2, 
                                   sample_label_1))
  }
  
  # Calculate mean for each group
  sample1_mean <- rowMeans(Sample1)
  sample2_mean <- rowMeans(Sample2)
  
  # Create a long format data frame for ggplot
  plot_data <- data.frame(
    x_values = rep(0:(nrow(Sample1)-1), ncol(Sample1) * 2),  # Repeat index for each column and dataset
    y_values = c(as.vector(Sample1), as.vector(Sample2)),  # Flatten both datasets
    label = factor(rep(c(sample_label_1, sample_label_2),
                       each = nrow(Sample1) * ncol(Sample1))),  # Labels from Org_data
    line_group = factor(rep(1:ncol(Sample1), each = nrow(Sample1), times = 2))  # Line group for each column
  )
  
  # Create a separate data frame for the mean lines
  mean_data <- data.frame(
    x_values = rep(0:(nrow(Sample1)-1), 2),
    y_values = c(sample1_mean, sample2_mean),
    label = factor(c(rep(sample_label_1, nrow(Sample1)), rep(sample_label_2, nrow(Sample1))), 
                   levels = c(sample_label_1, sample_label_2))
  )
  
  # Create the plot using ggplot2
  ggplot() +
    # First layer: Individual lines
    geom_line(data = plot_data, aes(x = x_values, y = y_values, group = interaction(line_group, label), color = label), linewidth = 1, alpha = 0.4) +
    # Second layer: Mean lines without group aesthetic
    geom_line(data = mean_data, aes(x = x_values, y = y_values, color = label), linewidth = 2) +
    scale_color_manual(values = colors_plot_data) +  # Set colors for both sample labels
    labs(title = Title, x = xlab, y = ylab) +  # Add labels
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the plot title
    theme(legend.position = legend)+  # Move legend to bottom
    #increase the font size of the labels and axis numbers
    theme(
      # axis.text.x = element_text(size = 12),
      # axis.text.y = element_text(size = 12),
      # axis.title.x = element_text(size = 14),
      # axis.title.y = element_text(size = 14),
      # plot.title = element_text(size = 16),
      #increase legends font and element size
      legend.text = element_text(size = 12),
      legend.title = element_blank())
  
} 


pulse_single_plot <- function(signal_data, legend_name, legend_title){
  ggplot() +
    geom_line(aes(x = 0:(length(signal_data)-1), y = signal_data, color = legend_name), linewidth = 1) +
    labs(title = "", x = "Domain", y = "Value", color = legend_title) +
    scale_color_manual(values = setNames(c("black"), legend_name)) +
    # x-axis values 0, 50, 100
    scale_x_continuous(breaks = c(0, 50, 100)) +
    theme_classic()+
    #theme_minimal() +
    theme(legend.position = "right",
          legend.text = element_text(size = 12))
}

Data_plot <- function(dataset, TITLE){
  cont_size <- dim(dataset)[1]
  
  # Create a data frame with the two-sample data, excluding 'Pulse'
  plot_data <- data.frame(
    x_values = rep(0:(cont_size - 1), 2),  # Repeat x_values for 2 lines
    y_values = c(dataset[, 1], dataset[, 2]),  # Combine all y-values of the two columns
    legend = factor(rep(c("Group 1", "Group 2"), each = dim(dataset)[1]))  # Control factor levels
  )
  
  # Create a data frame for the difference (Geometry)
  diff_data <- data.frame(
    x_values = 0:(cont_size - 1),
    y_values = dataset[, 2] - dataset[, 1],
    legend = factor(rep("Geometry", cont_size))  # Ensure Geometry is included in legend
  )
  
  # Combine both datasets
  combined_data <- rbind(plot_data, diff_data)
  
  # Define color and line type for all lines
  color_values <- setNames(c("tomato", "cadetblue", "black"),
                           c("Group 1", "Group 2", "Geometry"))
  linetype_values <- setNames(c("solid", "solid", "dotted"),
                              c("Group 1", "Group 2", "Geometry"))
  
  # Create the plot using ggplot
  ggplot(combined_data, aes(x = x_values, y = y_values, color = legend, linetype = legend)) +
    geom_line(linewidth = 1) +  # Plot lines for each group
    labs(title = TITLE, x = "Domain", y = "Value") +  # Labels
    scale_color_manual(values = color_values) +  # Line colors
    scale_linetype_manual(values = linetype_values) +  # Line types
    theme_minimal() +  # Use a minimal theme
    theme(plot.title = element_text(hjust = 0.5)) +  # Center the title
    theme(legend.position = "bottom") +
    theme(      
      # axis.text.x = element_text(size = 12),
      # axis.text.y = element_text(size = 12),
      # axis.title.x = element_text(size = 14),
      # axis.title.y = element_text(size = 14),
      # plot.title = element_text(size = 16),
      # legend.text = element_text(size = 12),
      legend.title = element_blank())  # Remove legend title
}
