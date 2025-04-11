# Analysis of tree coverage.
# Run 01_read_data.R before running this script.
library(tidyverse)
library(broom)
library(janitor)
library(scales)
library(RColorBrewer)



# Wrangling ---------------------------------------------------------------

# Get means for use in coverage plot
cov_means <- coverage |> 
  group_by(pool) |> 
  reframe(cov_mean = mean(coverage))



# Analysis ----------------------------------------------------------------

# Tukey HSD plots below coverage plot
summary(aov(log_coverage ~ pool, data = coverage))
coverage_tuke <- aov(log_coverage ~ pool, data = coverage)

coverage_hsd <- TukeyHSD(coverage_tuke, conf.level = 0.95)

coverage_hsd_tbl <- tidy(coverage_hsd)

coverage_hsd_tbl


# Tree Coverage Plot ------------------------------------------------------


tree_cover <- coverage |> 
  ggplot() +
  geom_point(
    aes(
      x = pool,
      y = coverage,
      color = pool)
  ) +
  geom_point(
    data = cov_means,
    aes(
      x = pool,
      y = cov_mean,
      color = pool
    ),
    size = 3,
    shape = 15
  ) +
  scale_color_brewer(
    palette = "Set1",
    name = NULL,
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
  theme(axis.text.x = element_text(size = 11)) +
  theme(text = element_text(family = "Linux Libertine O")) +
  labs(
    x = NULL,
    y = "Tree Coverage (m²)"
  )

tree_cover

ggsave(
  filename = "tree_coverage_plot.png",
  plot = tree_cover,
  height = 900,
  width = 1600,
  units = "px",
  dpi = 300,
  bg = "White"
)


# Tukey HSD plot for appendix ---------------------------------------------

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
  theme(text = element_text(family = "Linux Libertine O")) +
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




# coverage_sum <- coverage |> 
#   group_by(section) |> 
#   summarize(
#     min = min(coverage),
#     max = max(coverage)
#     )
# 
# fred <- left_join(x = coverage, y = coverage_sum)
# 
# fred |> 
#   group_by(section) |> 
#   ggplot() +
#   scale_color_brewer(palette = "Dark2") +
#   geom_segment(
#     aes(
#       x = section,
#       y = min,
#       xend = section,
#       yend = max,
#       color = section
#     ),
#     color = "gray70"
#   ) +
#   geom_point(
#     aes(x = section, y = coverage, color = section)
#   ) +
#   scale_y_log10() +
#   labs(
#     x = "Section",
#     y = "Areal coverage (sq. m)"
#   ) +
#   theme_minimal() +
#   theme(text = element_text(family = "Linux Libertine O")) +
#   coord_flip()
# 

