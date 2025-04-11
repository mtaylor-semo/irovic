# All analysis and plots for water depth.
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

# Leave original water_depth data alone, just in case.
wd <- water_depth |> 
  mutate(box_id = factor(box_id, levels = box_id_levels))

wd |> group_by(pool) |> 
  summarise(mn = mean(depth_mode))


# Analysis ----------------------------------------------------------------

wd_aov <- aov(depth_mode ~ pool, data = wd)
summary(wd_aov)

# Significant difference so determine which are diff.
wd_hsd <- TukeyHSD(wd_aov)

wd_hsd_tbl <- tidy(wd_hsd)

wd_hsd_tbl

# Plots -------------------------------------------------------------------

# Data distribution plot
water_depth_plot <- wd |> group_by(pool) |> 
  ggplot() +
  geom_segment(
    aes(
      x = box_id,
      y = depth_min,
      xend = box_id,
      yend = depth_max,
      color = pool
    )
  ) +
  geom_point(
    aes(
      x = box_id,
      y = depth_mode,
      color = pool
    ),
    size = 2
  ) +
  coord_flip() +
  labs(
    y = "Water depth (m)",
    x = "Nest Box",
    color = NULL
    ) +
  theme_minimal() +
  scale_color_brewer(
    palette = "Set1",
    labels = c("Pool 1", "Pool 2", "Pool 3")
  ) +
  theme(text = element_text(family = "Linux Libertine O"))

ggsave(
  file = "water_depth_plot.png",
  plot = water_depth_plot,
  width = 1600,
  height = 1600,
  units = "px",
  dpi = 300,
  bg = "white"
)


# Tukey HSD plot for appenxix

wd_hsd_plot <- wd_hsd_tbl |>
  ggplot() +
  geom_vline(
    xintercept = 0,
    color = "grey",
    linewidth = 0.5,
    linetype = 2
  ) +
  geom_segment(aes(x = conf.low, y = contrast, xend = conf.high)) +
  geom_point(aes(x = estimate, y = contrast), size = 2) +
  scale_x_continuous(limits = c(-2, 2), breaks = c(-2, -1, 0, 1, 2)) +
  labs(x = "Differences in mean depth of pools", y = "Comparison") +
  theme_minimal() +
  theme(text = element_text(family = "Linux Libertine O")) +
  theme(panel.grid = element_blank())

wd_hsd_plot

ggsave(
  filename = "hsd_wd_plot.png",
  plot = wd_hsd_plot,
  width = 1600,
  height = 900,
  dpi = 300,
  units = "px",
  bg = "white"
)


