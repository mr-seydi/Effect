source("basic_functions.R")
set.seed(123)
n_sim=2500
noise_binded_10=matrix(NA,nrow = 101, ncol = n_sim)
noise_binded_30=matrix(NA,nrow = 101, ncol = n_sim)
noise_binded_50=matrix(NA,nrow = 101, ncol = n_sim)
for (i in 1:n_sim){
  noise_binded_10[,i]=Noise_generator(Sample_size=10, Continuum_size=101, Noise_mu=0, Noise_sig=1, Noise_fwhm=10)$SD
  noise_binded_30[,i]=Noise_generator(Sample_size=10, Continuum_size=101, Noise_mu=0, Noise_sig=1, Noise_fwhm=30)$SD
  noise_binded_50[,i]=Noise_generator(Sample_size=10, Continuum_size=101, Noise_mu=0, Noise_sig=1, Noise_fwhm=50)$SD
}

# Combine all matrices into a data frame
df_10 <- data.frame(Domain = 0:100, as.data.frame(noise_binded_10))
df_30 <- data.frame(Domain = 0:100, as.data.frame(noise_binded_30))
df_50 <- data.frame(Domain = 0:100, as.data.frame(noise_binded_50))

df_10$FWHM <- "10"
df_30$FWHM <- "30"
df_50$FWHM <- "50"

# Merge all datasets
df_combined <- rbind(df_10, df_30, df_50)

# Reshape to long format using melt function from reshape2
df_long <- reshape2::melt(df_combined, id.vars = c("Domain", "FWHM"))

# Plot using ggplot
boxplots=ggplot(df_long, aes(x = factor(Domain), y = value, fill = FWHM)) +
  geom_boxplot(outlier.size = 0.5) +
  facet_wrap(~FWHM, scales = "free_y", ncol = 1) +
  labs(x = "Domain", y = "Noise SD", title = "", fill = "Noise FWHM") +
  scale_y_continuous(limits = c(-0.5, 2.5), breaks = c(0, 1, 2)) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    strip.text = element_blank(),    # Removes the facet titles
    legend.position = "bottom",    # Moves legend to the bottom
    legend.title = element_text(hjust = 0.5),  # Centers the legend title
    legend.box.just = "center"  # Centers the legend box
  ) +
  scale_fill_manual(values = c("lightblue", "darkgreen", "darkred"))

ggsave("results/Plots/Noise_SD_boxplots.jpeg", boxplots, width = 190, height = 196, units = "mm", dpi = 600)

