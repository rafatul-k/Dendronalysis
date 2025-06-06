#Isotope chronology by Chowdhury Rafatul Kabir
#M.S., B.Sc.in Forestry (SUST)
#Chronoogy plots


# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(zoo)
library(ggthemes)
library(ggpubr)

# Custom plot function
plot_chronology <- function(data, title_expr, line_color, ribbon_color, y_label) {
  summary_df <- data %>%
    mutate(
      P5 = rollapply(Chronology, width = 5, FUN = function(x) quantile(x, 0.05), fill = NA, align = "center"),
      P95 = rollapply(Chronology, width = 5, FUN = function(x) quantile(x, 0.95), fill = NA, align = "center")
    )
  
  ggplot(summary_df, aes(x = Year, y = Chronology)) +
    geom_ribbon(aes(ymin = P5, ymax = P95), fill = ribbon_color, alpha = 0.3) +
    geom_line(color = line_color, size = 1) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dotted", color = "black") +
    theme_minimal(base_size = 14) +
    labs(
      title = title_expr,
      x = "Year",
      y = y_label
    ) +
    theme(
      plot.title = element_text(size = 16, face = "italic", hjust = 0.5),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.grid.minor = element_blank()
    )
}

# Read Excel data
opt_data <- read_excel("opt.xlsx")
cfc_data <- read_excel("cfc.xlsx")

# Create plot for Polylepis tarapacana (δ¹⁸O)
plot1 <- plot_chronology(
  opt_data,
  title_expr = expression(italic("Polylepis tarapacana")),
  line_color = "#5c78df",
  ribbon_color = "#5c78df",
  y_label = expression(delta^{18}*O~"(‰)")
)

# Create plot for Fitzroya cupressoides (δ¹³C)
plot2 <- plot_chronology(
  cfc_data,
  title_expr = expression(italic("Fitzroya cupressoides")),
  line_color = "#FC8D62",
  ribbon_color = "#FC8D62",
  y_label = expression(delta^{13}*C~"(‰)")
)

# Combine and save the plots
tiff("Isotope_Chronologies11.tiff", width = 10, height = 8, units = "in", res = 300)
ggarrange(plot1, plot2, ncol = 1, nrow = 2, labels = c("A", "B"))
dev.off()


