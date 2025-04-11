# All analysis and plots for water width
# Run 01_read_data.R first
library(tidyverse)
library(janitor)
library(RColorBrewer)


# Wrangle -----------------------------------------------------------------

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

# Leave original water_width data alone, just in case.
ww <- water_width |> 
  mutate(box_id = factor(box_id, levels = box_id_levels))

ww |> group_by(pool) |> 
  summarise(mn = mean(width_mode))



# Analysis ----------------------------------------------------------------

ww_aov <- aov(width_mode ~ pool, data = water_width)
summary(ww_aov)

# No significant difference so Tukey HSD not performed.
# TukeyHSD(ww_aov)

water_width |> 
  group_by(pool) |> 
  reframe(n = mean(width_mode))



# Plots -------------------------------------------------------------------

# Width for all 30 boxes.
ww_plot <- ww |> group_by(pool) |> 
  ggplot() +
  geom_segment(
    aes(
      x =  box_id, 
      y = width_min,
      xend = box_id,
      yend = width_max,
      color = pool
    )
  ) +
  geom_point(
    aes(
      x = box_id,
      y = width_mode,
      color = pool
    ),
  ) +
  coord_flip() +
  labs(
    y = "Water width (m)",
    x = "Nest box",
    color = NULL
    ) +
  theme_minimal() +
  scale_color_brewer(
    palette = "Set1",
    labels = c("Pool 1", "Pool 2", "Pool 3")
  )  +
  theme(text = element_text(family = "Linux Libertine O"))

ggsave(
  file = "water_width_plot.png",
  plot = ww_plot,
  width = 1600,
  height = 1600,
  units = "px",
  dpi = 300,
  bg = "white"
)



# 
#   
# x <- egg1 |>
#   select(-c(total_eggs, total_abandoned, total_hatched, total_lost, box_id)) |>
#   pivot_longer(
#     cols = contains(c("wood", "merg")),
#     names_to = c("species", ".value"),
#     names_sep = "_"
#   ) |> 
#   select(-total) |> 
#   pivot_longer(
#     cols = c(hatched, abandoned, lost),
#     names_to = "outcome",
#     values_to = "number"
#   )
# 
# x |> 
#   group_by(section, species) |> 
#   filter(outcome == "hatched") |> 
#   ggplot(aes(color = section, shape = species)) +
#   geom_jitter(aes(x = section, y = number))
# 
# # Plots -------------------------------------------------------------------
# 
# # Simple plot of total eggs per section.
# egg1 |>
#   group_by(section) |>
#   ggplot(aes(color = section)) +
#   geom_jitter(aes(x = section, y = egg_total),
#               width = 0.1,
#               height = 0) +
#   xlab("Duck Creek Section") +
#   ylab("Total Eggs in Nest Box") +
#   scale_color_brewer(palette = "Dark2") +
#   theme_minimal()
