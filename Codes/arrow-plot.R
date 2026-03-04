# required packages
library(ggplot2)
library(dplyr)

# importing data
pisa_data <- read.csv("https://raw.githubusercontent.com/gungorMetehan/fdvi4p/refs/heads/main/Data/PISAdata_mathematics_G20.csv", header = T, sep = ",")

# data wrangling
pisa_data <- pisa_data %>%
  mutate(diff = PISA2022 - PISA2018,
         direction = ifelse(diff > 0, "Increase", "Decrease")) %>%
  arrange(diff) %>%
  mutate(Countries = factor(Countries, levels = Countries),
         Country_num = as.numeric(Countries))

# OECD mean index
oecd_idx <- which(levels(pisa_data$Countries) == "International Average (OECD)")

# plotting
p_caption1 <- "\n\nData Source: oecd.org\n\n"
p_caption2 <- "Visualization by Metehan Güngör\n\n"

arrowplot <- ggplot(pisa_data, aes(y = Country_num)) +
  # a strip for International Average (OECD)
  annotate("rect", xmin = -Inf, xmax = Inf, 
           ymin = oecd_idx - 0.5, ymax = oecd_idx + 0.5, 
           fill = "gray95", alpha = 0.5) +
  annotate("segment", x = -Inf, xend = Inf, 
           y = c(oecd_idx - 0.5, oecd_idx + 0.5), 
           yend = c(oecd_idx - 0.5, oecd_idx + 0.5), 
           linetype = "dashed", color = "gray60") +
  # starting points
  geom_point(aes(x = PISA2018), color = "gray70", size = 2) +
  # arrows
  geom_segment(aes(x = PISA2018, xend = PISA2022, 
                   y = Country_num, yend = Country_num, 
                   color = direction),
               arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
               linewidth = 0.8) +
  # colors
  scale_color_manual(values = c("Increase" = "#B7DE5D", "Decrease" = "#B84D90")) +
  scale_y_continuous(breaks = pisa_data$Country_num, labels = pisa_data$Countries) +
  # limits for x-axis
  scale_x_continuous(limits = c(300, 600)) +
  # theme
  theme_minimal() +
  labs(
    title = "Change in PISA Mathematics Scores in G20 Countries (2018 - 2022)",
    subtitle = "Direction and amount of change in Mathematics scores from 2018 to 2022",
    x = NULL, 
    y = NULL,
    caption = c(p_caption1, p_caption2)
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10), 
    plot.title = element_text(face = "bold", size = 14),
    text = element_text(family = "AvantGarde"),
    plot.caption = element_text(family = "AvantGarde", size = 10, vjust = 0, hjust = c(0, 1)),
    legend.position = "none"
  )

# saving as SVG
install.packages("sjPlot")
library(sjPlot)
save_plot("arrowplot.svg", fig = arrowplot, width = 50, height = 25)