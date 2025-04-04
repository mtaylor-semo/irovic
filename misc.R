library(broom)
library(RColorBrewer)
library(scales)

# have to run eggs.R to get the data for these analyses.
hatched_eggs |> 
  group_by(species) |> 
  reframe(n = range(number))

t.test(number ~ species, data = hatched_eggs, var.equal = TRUE)

summary(aov(number ~ species * section, data = hatched_eggs))

aov(number ~ species * section, data = hatched_eggs)

aov(species ~ coverage, data = merged1)



selected1 <- selected1 |>
  mutate(destiny = str_remove(destiny, "wood_")) |>
  mutate(destiny = str_remove(destiny, "merg_")) |>
  pivot_wider(names_from = destiny, values_from = number) |>
  select(-c(lost, abandoned))


# Use water depth and width for all 30 boxes to
# get a sense of the section
water_depth <- xx
wd <- aov(mode ~ section, data = water_depth)
summary(wd)
water_depth |> 
  group_by(section) |> 
  reframe(n = mean(mode))

wd_hsd <- TukeyHSD(wd)

wd_hsd_tbl <- tidy(wd_hsd)

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
  theme(panel.grid = element_blank())

wd_hsd_plot

ggsave(
  filename = "hsd_wd_plot.png",
  # plot = "wd_hsd_plot",
  width = 1600,
  height = 900,
  dpi = 300,
  units = "px",
  bg = "white"
)



ww <- aov(width ~ section, data = water_width)
summary(ww)
TukeyHSD(ww)
water_width |> 
  group_by(section) |> 
  reframe(n = mean(width))



summary(aov(coverage ~ section, data = selected1))

summary(aov(log_coverage ~ section, data = selected1))
tuke <- aov(coverage ~ section, data = selected1)

coverage_hsd <- TukeyHSD(tuke, conf.level = 0.95)
plot(TukeyHSD(tuke, conf.level = 0.95))


coverage_hsd_tbl <- tidy(coverage_hsd)

coverage_hsd_tbl

coverage_hsd_plot <- coverage_hsd_tbl |> 
  ggplot() +
  geom_vline(
    xintercept = 0,
    color = "grey",
    linewidth = 0.5,
    linetype = 2) +
  geom_segment(
    aes(
      x = conf.low,
      y = contrast,
      xend = conf.high)
  ) +
  geom_point(
    aes(
      x = estimate,
      y = contrast),
    size = 2
  ) +
  scale_x_continuous(
    breaks = c(-0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6)
  ) +
  labs(
    x = "Differences in mean coverage between between pools",
    y = "Comparison") +
  theme_minimal() +
  theme(panel.grid = element_blank())

coverage_hsd_plot

ggsave(
  filename = "hsd_coverage_plot.png",
  plot = coverage_hsd_plot,
  width = 1600,
  height = 900,
  dpi = 300,
  units = "px",
  bg = "white"
)

cov_means <- selected1 |> 
  group_by(section) |> 
  reframe(cov_mean = mean(coverage)) |> 
  mutate(
    section = if_else(
      section == "E", "Pool2", if_else(
        section == "N", "Pool1", "Pool3"
      )
    ))



coverage |> 
  mutate(
    section = if_else(
      section == "E", "Pool2", if_else(
        section == "N", "Pool1", "Pool3"
      )
    )) |> 
  ggplot() +
  geom_point(
    aes(
      x = section,
      y = coverage,
      color = section)
  ) +
  geom_point(
    data = cov_means,
    aes(
      x = section,
      y = cov_mean,
      color = section
    ),
    size = 3,
    shape = 15
  ) +
  scale_color_brewer(
    palette = "Set1",
    name = "Section",
    labels = c("Pool 1", "Pool 2", "Pool 3")
  ) +
  scale_y_continuous(
    limits = c(0,32000),
    breaks = c(5000, 10000, 15000, 20000, 25000, 30000),
    labels = comma
  ) +
  scale_x_discrete(labels = c("Pool 1", "Pool 2", "Pool 3")) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank()) +
  labs(
    x = "Duck Creek section",
    y = "Tree Coverage (m²)"
  )

ggsave(
  filename = "tree_coverage_plot.png",
  height = 900,
  width = 1600,
  units = "px",
  dpi = 300,
  bg = "White"
)
