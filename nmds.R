library(tidyverse)
library(janitor)
library(RColorBrewer)

library(vegan)

eggs <- read_csv(
  "data/egg_data.csv",
  skip = 1,
  na = "N/A",
  col_names = c(
    "entry",
    "section",
    "box_id",
    "attempt",
    "first_observed",
    "total_eggs",
    "hatched_eggs",
    "abandoned_eggs",
    "lost_eggs",
    "wood_total",
    "wood_hatched",
    "wood_abandoned",
    "wood_lost",
    "merg_total",
    "merg_hatched",
    "merg_abandoned",
    "merg_lost"
  )
) |>
  select(-ends_with("_eggs")) |>
  drop_na()


coverage <- read_csv(
  "data/coverage.csv",
  skip = 1,
  na = "N/A",
  col_names = c("N", "section", "box_id", "coverage")
) |>
  mutate(log_coverage = log10(coverage))

water_depth <- read_csv(
  "data/water_depth.csv",
  skip = 1,
  na = "N/A",
  col_names = c(
    "N",
    "section",
    "box_id",
    "depth_min",
    "depth_max",
    "depth_mode"
  )
)

water_depth <- water_depth %>% mutate(across(4:6, ~ as.numeric(gsub("<|>", "", .x))))


water_depth <-
  water_depth |>
  select(-c(depth_min, depth_max, N)) |>
  rename(depth = depth_mode)

water_width <- read_csv(
  "data/water_width.csv",
  skip = 1,
  na = "N/A",
  col_names = c(
    "N",
    "section",
    "box_id",
    "width_min",
    "width_max",
    "width_mode"
  )
)

water_width <- water_width %>% mutate(across(4:6, ~ as.numeric(gsub("<|>", "", .x))))

water_width <-
  water_width |>
  mutate(width_mode = replace_na(width_mode, 2.44))

water_width <-
  water_width |>
  select(-c(width_min, width_max, N)) |>
  rename(width = width_mode)



# Idea from https://stackoverflow.com/a/34393416/3832941
merged <- list(eggs, coverage, water_depth, water_width) |>
  reduce(left_join, by = c("box_id", "section"))

# Try to convert merged into species

merged1 <- merged |>
  mutate(species = if_else(merg_total < 2, "WODU", "HOME")) |>
  select(-ends_with("_total")) |>
  pivot_longer(cols = wood_hatched:merg_lost,
               names_to = "destiny",
               values_to = "number") |>
  select(-ends_with("_eggs"))

# Try with both species
# selected1 <- merged1 |>
#   select(
#     section,
#     box_id,
#     attempt,
#     species,
#     destiny,
#     number,
#     depth_mode,
#     width_mode,
#     coverage,
#     log_coverage
#   ) |>
#   filter(rowSums(across(where(is.numeric))) != 0) |>
#   filter((species == "HOME" & destiny == "merg_hatched") |
#            species == "WODU" & destiny == "wood_hatched")

# Try with both species
selected1 <- merged1 |>
  select(
    section,
    box_id,
    attempt,
    species,
    destiny,
    number,
    depth,
    width,
    coverage,
    log_coverage
  ) |>
  filter(rowSums(across(where(is.numeric))) != 0) |>
  filter((species == "HOME" & destiny == "merg_hatched") |
           (species == "HOME" & destiny == "merg_lost") |
           (species == "HOME" & destiny == "merg_abandoned") |
           (species == "WODU" & destiny == "wood_abandoned") |
           (species == "WODU" & destiny == "wood_lost") |
           species == "WODU" & destiny == "wood_hatched"
  )


selected1 <- selected1 |>
  mutate(destiny = str_remove(destiny, "wood_")) |>
  mutate(destiny = str_remove(destiny, "merg_")) |>
  pivot_wider(names_from = destiny, values_from = number) |>
  select(-c(lost, abandoned, coverage)) |> 
  rename(coverage = log_coverage) |> 
  mutate(section = ifelse(
    section == "N", "Pool 1", ifelse(
      section == "E", "Pool 2", "Pool 3"
    )
  ))


# Was depth significant among the two species
# for boxes with nests
t.test(depth ~ species, data = selected1)




# NMDS, 2 axes, with plot.
duckmds2 <- metaMDS(selected1[, 5:8], k = 2)
duck_scores <- as_tibble(scores(duckmds2)$sites)
scores(duckmds2)

duck_vars <- as_tibble(scores(duckmds2)$species)

duck_vars$vars <- rownames(scores(duckmds2)$species)

# Black = East, Green = South, Red = North
duck_scores$section <- factor(selected1$section, levels = c("E", "N", "S"))
duck_scores$box_id <- selected1$box_id
duck_scores$species <- selected1$species


# NMDS, 3 axes, with plot.
duckmds3 <- metaMDS(selected1[, 5:8], k = 3)
duck_scores <- as_tibble(scores(duckmds3)$sites)

duck_vars <- as_tibble(scores(duckmds3)$species)

duck_vars$vars <- rownames(scores(duckmds3)$species)

# Black = East, Green = South, Red = North
duck_scores$section <- factor(selected1$section, levels = c("E", "N", "S"))
duck_scores$box_id <- selected1$box_id
duck_scores$species <- selected1$species




