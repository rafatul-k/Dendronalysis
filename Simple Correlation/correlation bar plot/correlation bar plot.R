# Load required libraries
library(ggplot2)
library(ggpubr)
library(grid)
library(readr)
# Set working directory
setwd("C:/Users/C Rafatul Kabir/Desktop/hydnocarpus kurzii manuscript/new analysis/Dendro climate growth study/Plots/final rp csv")

# Read data from CSV files
maxcor <- read.csv("maxcor_rp.csv", header = TRUE, sep = ",")
mincor <- read.csv("mincor_rp.csv", header = TRUE, sep = ",")
meancor <- read.csv("meancor_rp.csv", header = TRUE, sep = ",")
precipcor <- read.csv("precipcor_rp.csv", header = TRUE, sep = ",")
speicor <- read.csv("speicor_rp.csv", header = TRUE, sep = ",")   # Please confirm this is correct
cccor <- read.csv("cccor_rp.csv", header = TRUE, sep = ",")   # Please confirm this is correct

# Define the order of bars
bar_order <- c("May_PY", "June_PY", "July_PY", "Aug_PY", "Sep_PY", "Oct_PY", 
               "Nov_PY", "Dec_PY", "Jan", "Feb", "March", "April", "May", 
               "June", "July", "Aug", "Sep", "Oct", "Nov", "PM", "M", "PoM", "DS")

# Define a function to plot each barplot
plot_barplot <- function(data, title) {
  data <- data[match(bar_order, data$parameter), ]  # Order by predefined bar order
  data$significance <- ifelse(data$p < 0.05, "Significant", "Not Significant")
  
  p <- ggplot(data, aes(x = factor(parameter, levels = bar_order), y = r, fill = significance)) +
    geom_bar(stat = "identity", color = "white", width = 0.4) +
    scale_fill_manual(values = c("Significant" = "#ece84e", "Not Significant" = "#d3d3d3"), guide = FALSE) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      legend.position = "none"
    )
  return(p)
}

# Create individual plots
p1 <- plot_barplot(maxcor, "Tmax")
p2 <- plot_barplot(mincor, "Tmin")
p3 <- plot_barplot(meancor, "Tmean")
p4 <- plot_barplot(precipcor, "Precipitation")
p5 <- plot_barplot(speicor, "SPEI")
p6 <- plot_barplot(cccor, "Cloud Cover")

# Arrange plots in 2 columns × 3 rows with panel labels
combined_plot <- ggarrange(
  p1, p2, p3, p4, p5, p6,
  labels = c("A", "B", "C", "D", "E", "F"),
  ncol = 2, nrow = 3,
  align = "hv",
  common.legend = TRUE,
  legend = "bottom"
)

# Add shared Y-axis label using annotate_figure
final_plot <- annotate_figure(
  combined_plot,
  left = text_grob("Correlation coefficient", rot = 90, size = 14, face = "bold")
)

# Save as high-resolution TIFF
tiff("LNP_bar4.tiff", width = 11, height = 9, units = "in", res = 600, compression = "lzw")
print(final_plot)
dev.off()

