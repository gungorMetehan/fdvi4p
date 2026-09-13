# This script simulates Item Parameter Drift (IPD) data and visualizes 
# the changes in item difficulty rankings over time using a bump plot.

# required packages
library(tidyverse)
library(ggbump)

# 1. Reproducible and Controlled Data Generation
set.seed(2026)

items <- paste0("Item_", sprintf("%02d", 1:10))
years <- c(2015, 2018, 2021)

# assign stable base difficulties for non-drifting items so their rankings don't cross
base_b <- seq(1.5, -1.5, length.out = 10)
names(base_b) <- items

ipd_data <- expand_grid(item = items, year = years) |> 
  mutate(
    # add very minor noise (sd = 0.02) to keep stable items completely flat in rankings
    b_param = base_b[item] + rnorm(n(), mean = 0, sd = 0.02)
  )

# introduce dramatic drift only for Item_03 and Item_08
ipd_data <- ipd_data |> 
  mutate(b_param = case_when(
    item == "Item_03" & year == 2015 ~ 1.2,
    item == "Item_03" & year == 2018 ~ 0.1,
    item == "Item_03" & year == 2021 ~ -1.3,
    
    item == "Item_08" & year == 2015 ~ -1.1,
    item == "Item_08" & year == 2018 ~ 0.4,
    item == "Item_08" & year == 2021 ~ 1.4,
    
    TRUE ~ b_param
  ))

# 2. Ranking Format
ranked_items <- ipd_data |> 
  group_by(year) |> 
  mutate(ranking = rank(desc(b_param), ties.method = "first")) |> 
  ungroup()

selected_items <- c("Item_03", "Item_08")

# 3. plotting
plot <- ranked_items |> 
  ggplot(aes(x = year, y = ranking, group = item)) +
  
  # background bumps (stable items)
  geom_bump(linewidth = 0.8, color = "gray85", smooth = 8) +
  # highlighted bumps (drifting items)
  geom_bump(aes(color = item), linewidth = 1.2, smooth = 8,
            data = ~. |> filter(item %in% selected_items)) +
  
  # points with white background outline for clean look
  geom_point(color = "white", size = 5) +
  geom_point(color = "gray85", size = 2.5) +
  geom_point(aes(color = item), size = 2.5, 
             data = ~. |> filter(item %in% selected_items)) +
  
  # item labels (stable items)
  geom_text(aes(label = item), x = 2021.2, hjust = 0,
            color = "gray60", family = "AvantGarde", size = 3.5, fontface = "bold",
            data = ranked_items |> slice_max(year, by = item) |> 
              filter(!item %in% selected_items)) +
  # item labels (highlighted items)
  geom_text(aes(label = item), x = 2021.2, hjust = 0,
            color = "black", family = "AvantGarde", size = 4, fontface = "bold",
            data = ranked_items |> slice_max(year, by = item) |> 
              filter(item %in% selected_items)) +
  
  # custom colors
  scale_color_manual(values = c("Item_03" = "#ff6966", "Item_08" = "#006ab8")) +
  
  # x-axis adjustments
  scale_x_continuous(limits = c(2014.8, 2022.5), expand = c(0.01, 0),
                     breaks = c(2015, 2018, 2021)) +
  # y-axis adjustments
  scale_y_reverse(breaks = 1:10, expand = c(0.02, 0),
                  labels = function(x) paste0(x, ".")) +
  
  # titles, subtitles, and labels
  labs(x = NULL, 
       y = expression(italic(b) ~ "-parameter Rank (1 = Hardest)"),
       title = "Item Parameter Drift of Anchor Items",
       subtitle = "Item_03 becomes significantly easier over time, while Item_08 becomes harder.",
       caption = "Data: Simulated IRT parameters\nVisualization by Metehan Güngör") +
  
  # theme and grid adjustments
  theme_minimal(base_family = "AvantGarde", base_size = 12) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(color = "gray95", linewidth = 0.4),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 15)),
        plot.title.position = "plot",
        plot.title = element_text(size = 15, face = "bold", hjust = 0, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0, margin = margin(b = 20)),
        plot.caption = element_text(size = 9, color = "gray50", hjust = 1, margin = margin(t = 20)))

print(plot)