library(tidyverse)
library(janitor)
library(RColorBrewer)

section_colors <- brewer.pal(3, "Set1")

water_depth <- read_csv(
  "data/water_depth.csv",
  skip = 1,
  na = "N/A",
  col_names = c(
    "N",
    "section",
    "box_id",
    "min",
    "max",
    "mode"
  )
)

water_depth <- water_depth |> 
  mutate(section =
           ifelse(section == "E", "Pool2",
                  ifelse(section == "N", "Pool1", "Pool3")))

# First, strip the < and > symbols and look at the values.

x <- water_depth %>% mutate(across(4:6, ~as.numeric(gsub("<|>", "", .x))))

box_id_levels <- c(
  "S48",
  "S46",
  "S44",
  "S40",
  "S38",
  "S34",
  "S20",
  "S14",
  "S11",
  "S7",
  "E93",
  "E84",
  "E71",
  "E65",
  "E62",
  "E40",
  "E23",
  "E22",
  "E7",
  "E4",
  "N96",
  "N93", 
  "N86",
  "N73",
  "N70",
  "N68",
  "N62",
  "N58",
  "N47",
  "N2"
)

xx <- x |> 
  mutate(box_id = factor(box_id, levels = box_id_levels))

xx |> group_by(section) |> 
  summarise(mn = mean(mode))



xx |> group_by(section) |> 
  ggplot() +
  geom_segment(
    aes(
      x = box_id,
      y = min,
      xend = box_id,
      yend = max,
      color = section
    )
  ) +
  # geom_point(
  #   aes(
  #     x = box_id,
  #     y = min,
  #     colour = section
  #   ),
  #   size = 1
  # ) +
  # geom_point(
  #   aes(
  #     x = box_id,
  #     y = max,
  #     colour = section
  #   ),
  #   size = 1
  # ) +
  geom_point(
    aes(
      x = box_id,
      y = mode,
      color = section
    ),
#    shape = 0,
    size = 2
  ) +
  coord_flip() +
  labs(
    y = "Water depth (m)",
    x = "Nest Box",
    color = "Section"
    ) +
  theme_minimal() +
  scale_color_brewer(
    palette = "Set1",
    labels = c("Pool 1", "Pool 2", "Pool 3")
  ) +
  theme(text = element_text(family = "Linux Libertine O"))


ggsave(
  file = "water_depth_plot.png",
  width = 1600,
  height = 1600,
  units = "px",
  dpi = 300,
  bg = "white"
)




# x |> 
#   ggplot() +
#   geom_point(
#     aes(section, min),
#     fill = "white",
#     position = position_jitter(width = 0.3),
#     shape = 25,
#     size = 2) +
#   # geom_point(
#   #   aes(section, mode), 
#   #   fill = "gray50",
#   #   position = position_jitter(width = 0.3),
#   #   pch = 22,
#   #   size = 2) +
#   geom_point(
#     aes(section, max), 
#     fill = "black",
#     position = position_jitter(width = 0.3),
#     shape = 24,
#     size = 2) +
#   labs(
#     x = "Section",
#     y = "Water width (m)"
#     ) +
#   theme_minimal()
# 
# x |> group_by(section) |> 
#   ggplot() +
#   geom_point(
#     aes(
#       x = min, 
#       y = max, 
#       color = section
#     ),
#     size = 2
#   )
# 
# 


