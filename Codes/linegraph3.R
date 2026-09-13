# required packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)

# the dataset
Sweden_PIRLS <- data.frame(
  cycles = c(2001, 2001, 2006, 2006, 2011, 2011, 2016, 2016, 2021, 2021),
  genders = factor(rep(c("Girls", "Boys"), times = 5)),
  scores = c(572, 550, 559, 541, 549, 535, 563, 548, 551, 536)
)

# a tibble for the gray shaded area
area <- Sweden_PIRLS %>%
  pivot_wider(names_from = genders, values_from = scores) %>%
  mutate(ymax = pmax(Boys, Girls), ymin = pmin(Boys, Girls))

# international averages (Int'l Girls & Boys)
intl_girls <- data.frame(cycles = c(2001, 2006, 2011, 2016, 2021), scores = c(510, 509, 520, 520, 509))
intl_boys <- data.frame(cycles = c(2001, 2006, 2011, 2016, 2021), scores = c(490, 492, 504, 501, 493))

# plot title, subtitle, and caption
p_title <- "Average Reading Achievement by Gender: SWEDEN"
p_subtitle <- "In all PIRLS cycles, fourth grade girls from Sweden have higher average achievement than boys.\nSweden is not alone in this regard. Gender differences in reading are a common finding in international assessments with girls usually outperforming boys."
p_caption <- "Data Source: iea.nl<br>Visualization by Metehan Güngör"

# plotting
myplot <- ggplot(data = Sweden_PIRLS, mapping = aes(x = cycles, y = scores, group = genders, color = genders)) +
  
  # drawing vertical dotted lines in the background
  geom_vline(xintercept = unique(Sweden_PIRLS$cycles), linetype = "dotted", col = "gray85", linewidth = 0.5) +
  # gray shaded area for Sweden gender gap
  geom_ribbon(data = area, mapping = aes(x = cycles, ymin = ymin, ymax = ymax), alpha = 0.1, fill = "gray50", inherit.aes = FALSE) +
  # international Averages (neutral gray tones, pushed to the background)
  geom_line(data = intl_girls, aes(x = cycles, y = scores), color = "gray60", linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
  geom_point(data = intl_girls, aes(x = cycles, y = scores), color = "gray60", shape = 21, fill = "white", size = 2, inherit.aes = FALSE) +
  geom_line(data = intl_boys, aes(x = cycles, y = scores), color = "gray75", linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE) +
  geom_point(data = intl_boys, aes(x = cycles, y = scores), color = "gray75", shape = 21, fill = "white", size = 2, inherit.aes = FALSE) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white", stroke = 1.2) +
  geom_label(aes(label = scores), fontface = "bold", family = "AvantGarde", label.size = 0, fill = alpha("white", 0.7), show.legend = FALSE) +
  # text annotations for Sweden
  annotate(geom = "text", x = 2021.5, y = 551, hjust = 0, label = "Sweden\nGirls", color = "#006aa7", size = 3.5, fontface = "bold", family = "AvantGarde", lineheight = 0.9) +
  annotate(geom = "text", x = 2021.5, y = 536, hjust = 0, label = "Sweden\nBoys", color = "#d1a700", size = 3.5, fontface = "bold", family = "AvantGarde", lineheight = 0.9) +
  # text annotations for international averages (matching the neutral gray colors)
  annotate(geom = "text", x = 2021.5, y = 509, hjust = 0, label = "Int'l\nGirls", color = "gray50", size = 3, fontface = "italic", family = "AvantGarde", lineheight = 0.9) +
  annotate(geom = "text", x = 2021.5, y = 493, hjust = 0, label = "Int'l\nBoys", color = "gray65", size = 3, fontface = "italic", family = "AvantGarde", lineheight = 0.9) +
  # theme minimal
  theme_minimal() +
  # x-axis scale
  scale_x_continuous(breaks = c(2001, 2006, 2011, 2016, 2021), limits = c(2000.5, 2022.5)) +
  # coloring for Sweden
  scale_color_manual(values = c("#d1a700", "#006aa7")) +
  # y-axis limits
  ylim(450, 600) +
  # title, subtitle, caption, and axis labels
  labs(title = p_title, subtitle = p_subtitle, caption = p_caption, x = "", y = "Mean Score") +
  # custom elegant theme settings with added margins for spacing
  theme(
    text = element_text(family = "AvantGarde"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    # adding elegant solid lines only for the x and y axes (L-shape)
    axis.line = element_line(color = "gray30", linewidth = 0.5),
    legend.position = "none",
    # added margin (b = 10) to create space below the title
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold", margin = margin(b = 10)), 
    # added margin (b = 20) to create space between the subtitle and the plot area
    plot.subtitle = element_text(size = 12, margin = margin(b = 20)),
    plot.caption = element_markdown(size = 10, color = "gray30", vjust = 0, hjust = 0, lineheight = 1.5),
    # added a slight margin
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )

myplot
