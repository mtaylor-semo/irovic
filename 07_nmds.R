# All analysis and plots for NMDS
# Run 01_read_data.R first

library(tidyverse)
library(RColorBrewer)
library(vegan)



# Wrangle -----------------------------------------------------------------

# Remove "Total" columns from eggs data.
eggs_nmds <- eggs |> select(-starts_with(("total_"))) |> 
  drop_na()


# Remove specific columns from water depth and width, and coverage
wd_nmds <-
  water_depth |>
  select(-c(depth_min, depth_max, N)) |>
  rename(depth = depth_mode)

ww_nmds <-
  water_width |>
  mutate(width_mode = replace_na(width_mode, 2.44))

ww_nmds <-
  ww_nmds |>
  select(-c(width_min, width_max, N)) |> 
  rename(width = width_mode)

coverage_nmds <- coverage |> select(-c(N, coverage))

# Merge the four files into single data set for analysis
# Idea from https://stackoverflow.com/a/34393416/3832941
nmds_data_tmp <- list(eggs_nmds, coverage_nmds, wd_nmds, ww_nmds) |>
  reduce(left_join, by = c("box_id", "pool")) |>
  mutate(species = if_else(merg_total < 2, "WODU", "HOME")) |>
  select(-ends_with("_total")) |>
  pivot_longer(cols = wood_hatched:merg_lost,
               names_to = "eggs",
               values_to = "number") |>
  select(-ends_with("_eggs"))

nmds_data <- nmds_data_tmp |>
  select(
    pool,
    box_id,
    attempt,
    species,
    eggs,
    number,
    depth,
    width,
    log_coverage
  ) |>
  filter(rowSums(across(where(is.numeric))) != 0) |>
  filter((species == "HOME" & eggs == "merg_hatched") |
           (species == "HOME" & eggs == "merg_lost") |
           (species == "HOME" & eggs == "merg_abandoned") |
           (species == "WODU" & eggs == "wood_abandoned") |
           (species == "WODU" & eggs == "wood_lost") |
           species == "WODU" & eggs == "wood_hatched"
  )


nmds_data <- nmds_data |>
  mutate(eggs = str_remove(eggs, "wood_")) |>
  mutate(eggs = str_remove(eggs, "merg_")) |>
  pivot_wider(names_from = eggs, values_from = number) |>
  select(-c(lost, abandoned)) |> 
  rename(coverage = log_coverage)

# Remove the data sets no longer needed.
rm(c("eggs_nmds", "coverage_nmds", "wd_nmds", "ww_nmds", "nmds_data_tmp"))


# Analyses ----------------------------------------------------------------


# Was depth significant among the two species
# for boxes with nests. NO
t.test(depth ~ species, data = nmds_data)



# NMDS with 2 axes. Not used.
#
# duckmds2 <- metaMDS(nmds_data[, 5:8], k = 2)
# duck_scores <- as_tibble(scores(duckmds2)$sites)
# scores(duckmds2)
# 
# duck_vars <- as_tibble(scores(duckmds2)$species)
# 
# duck_vars$vars <- rownames(scores(duckmds2)$species)
# 
# # Black = East, Green = South, Red = North
# duck_scores$section <- factor(nmds_data$pool, levels = c("E", "N", "S"))
# duck_scores$box_id <- nmds_data$box_id
# duck_scores$species <- nmds_data$species


# NMDS, k = 3 axes, for plotting (see below).
duckmds3 <- metaMDS(nmds_data[, 5:8], k = 3)
duck_scores <- as_tibble(scores(duckmds3)$sites)

duck_vars <- as_tibble(scores(duckmds3)$species)

duck_vars$vars <- rownames(scores(duckmds3)$species)

# Black = East, Green = South, Red = North
duck_scores$pool <- factor(nmds_data$pool, levels = c("Pool1", "Pool2", "Pool3"))
duck_scores$box_id <- nmds_data$box_id
duck_scores$species <- nmds_data$species



# Plot NMDS Results -------------------------------------------------------


# Reduce the variables to only hatched and width for variation
# of nmds_1-2 plot. Plot axes 1 and 2.
vars1.2 <- duck_vars |> 
  filter(vars == "hatched" |
           vars == "width")


p1.2.2 <- p1.2 + geom_segment(
  data = vars1.2,
  aes(
    x = 0,
    y = 0,
    xend = NMDS1,
    yend = NMDS2
  ),
  color = "gray70",
  linewidth = 0.2
) +
  geom_text(
    data = vars1.2,
    aes(x = NMDS1, y = NMDS2, label = vars),
    size = 2,
    hjust = "outward",
    vjust = "outward",
    family = "Linux Libertine O"
  )

p1.2.2 # Axes 1 and 2.

ggsave(
  "nmds_1-2-2.png",
  width = 1600,
  height = 900,
  units = "px",
  dpi = 300,
  bg = "white"
)



# NMDS 2 vs 3
vars2.3 <- duck_vars |> 
  filter(vars == "depth" |
           vars == "width")

# vars2.3 <- duck_vars |> 
#   filter(vars != "hatched")

p2.3 <- ggplot(data = duck_scores, aes(x = NMDS2, y = NMDS3)) +
  geom_text(
    data = duck_scores,
    aes(x = NMDS2, y = NMDS3, label = box_id),
    color = "gray50",
    size = 1.5,
    hjust = -0.5,
    family = "Linux Libertine O"
  ) +
  geom_point(aes(colour = species, shape = species), size = 1.5) +
  scale_color_brewer(
    palette = "Dark2",
    name = NULL,
    labels = c("Hooded Merganser", "Wood Duck")
  ) +
  scale_shape_discrete(name = NULL,
                       labels = c("Hooded Merganser", "Wood Duck")) +
  scale_x_continuous(
    limits = c(-0.6, 0.6),
    breaks = c(-0.4, -0.2, 0.0, 0.2, 0.4)
  ) +
  scale_y_continuous(
    limits = c(-0.5, 0.5),
    breaks = c(-0.4, -0.2, 0.0, 0.2, 0.4)
  ) +
  theme_minimal() +
  guides(color = guide_legend(position = "inside")) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5.5),
    text = element_text(family = "Linux Libertine O"),
    legend.text = element_text(size = 6, margin = margin(l = 0.1, unit = "pt")),
    legend.title = element_text(size = 6.5),
    legend.key.spacing.y = unit(-7, "pt"),
    legend.position.inside = c(0.8, 0.95)
  ) +
  coord_fixed(ratio = 0.75)


p2.3 <- p2.3 + geom_segment(
  data = vars2.3,
  aes(
    x = 0,
    y = 0,
    xend = NMDS2,
    yend = NMDS3
  ),
  color = "gray70",
  linewidth = 0.2
) +
  geom_text(
    data = vars2.3,
    aes(x = NMDS2, y = NMDS3, label = vars),
    size = 2,
    hjust = "outward",
    vjust = "outward",
    family = "Linux Libertine O"
  )

p2.3

ggsave(
  "nmds_2-3.png",
  width = 1600,
  height = 900,
  units = "px",
  dpi = 300,
  bg = "white"
)




# Not used ----------------------------------------------------------------


# This plots all four variables in 2-D. Ended up not using. Here for
# posterity.
p1.2 <- ggplot(data = duck_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_text(
    data = duck_scores,
    aes(x = NMDS1, y = NMDS2, label = box_id),
    color = "gray50",
    size = 1.5,
    hjust = -0.5,
    family = "Linux Libertine O"
  ) +
  geom_point(aes(colour = species, shape = species), size = 1.5) +
  scale_color_brewer(
    palette = "Dark2",
    name = NULL,
    labels = c("Hooded Merganser", "Wood Duck")
  ) +
  scale_shape_discrete(name = NULL,
                       labels = c("Hooded Merganser", "Wood Duck")) +
  scale_x_continuous(
    limits = c(-0.6, 0.6),
    breaks = c(-0.4, -0.2, 0.0, 0.2, 0.4)
  ) +
  scale_y_continuous(
    limits = c(-0.5, 0.5),
    breaks = c(-0.4, -0.2, 0.0, 0.2, 0.4)
  ) +
  theme_minimal() +
  guides(color = guide_legend(position = "inside")) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5.5),
    legend.text = element_text(size = 6, margin = margin(l = 0.1, unit = "pt")),
    legend.title = element_text(size = 6.5),
    legend.key.spacing.y = unit(-7, "pt"),
    legend.position.inside = c(0.8, 0.95),
    text = element_text(family = "Linux Libertine O")
  ) +
  coord_fixed(ratio = 0.75)


p1.2.4 <- p1.2 + geom_segment(
  data = duck_vars,
  aes(
    x = 0,
    y = 0,
    xend = NMDS1,
    yend = NMDS2
  ),
  color = "gray70",
  linewidth = 0.2
) +
  geom_text(
    data = duck_vars,
    aes(x = NMDS1, y = NMDS2, label = vars),
    size = 2,
    hjust = "outward",
    vjust = "outward"
  )

p1.2.4

ggsave(
  "nmds_1-2-4.png",
  width = 1600,
  height = 900,
  units = "px",
  dpi = 300,
  bg = "white"
)
