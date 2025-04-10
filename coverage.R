library(tidyverse)
library(janitor)
library(RColorBrewer)

section_colors <- brewer.pal(3, "Dark2")

coverage <- read_csv(
  "data/coverage.csv",
  skip = 1,
  na = "N/A",
  col_names = c(
    "N",
    "section",
    "box_id",
    "coverage"
  )
)

x <- coverage |> 
  mutate(log_coverage = log10(coverage))

x_sum <- x |> 
  group_by(section) |> 
  summarize(
    min = min(coverage),
    max = max(coverage)
    )

fred <- left_join(x = x, y = x_sum)

fred |> 
  group_by(section) |> 
  ggplot() +
  scale_color_brewer(palette = "Dark2") +
  geom_segment(
    aes(
      x = section,
      y = min,
      xend = section,
      yend = max,
      color = section
    ),
    color = "gray70"
  ) +
  geom_point(
    aes(x = section, y = coverage, color = section)
  ) +
  scale_y_log10() +
  labs(
    x = "Section",
    y = "Areal coverage (sq. m)"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Linux Libertine O")) +
  coord_flip()


