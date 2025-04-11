# Read in the base data sets for use with other scripts.
# This script should run before any other.
# All csv files should be in a folder named "data."
library(tidyverse)
library(janitor)

# Read Tree Coverage data -------------------------------------------------

coverage <- read_csv(
  "data/coverage.csv",
  skip = 1,
  na = "N/A",
  col_names = c("N", "pool", "box_id", "coverage")
) |>
  mutate(log_coverage = log10(coverage)) |>
  mutate(pool = if_else(pool == "E", "Pool2", if_else(pool == "N", "Pool1", "Pool3")))


# Read Water Depth data ---------------------------------------------------

water_depth <- read_csv(
  "data/water_depth.csv",
  skip = 1,
  na = "N/A",
  col_names = c("N", "pool", "box_id", "depth_min", "depth_max", "depth_mode")
)

water_depth <- water_depth |>
  mutate(pool =
           ifelse(pool == "E", "Pool2", ifelse(pool == "N", "Pool1", "Pool3")))

water_depth <- water_depth %>% mutate(across(4:6, ~ as.numeric(gsub("<|>", "", .x))))



# Read Water Width data ---------------------------------------------------

water_width <- read_csv(
  "data/water_width.csv",
  skip = 1,
  na = "N/A",
  col_names = c("N", "pool", "box_id", "width_min", "width_max", "width_mode")
) |>
  mutate(pool =
           ifelse(pool == "E", "Pool2", ifelse(pool == "N", "Pool1", "Pool3")))


water_width <- water_width %>% mutate(across(4:6, ~ as.numeric(gsub("<|>", "", .x))))

water_width <-
  water_width |>
  mutate(width_mode = replace_na(width_mode, 2.44))

# water_width <-
#   water_width |>
#   select(-c(width_min, width_max, N)) |>
#   rename(width = width_mode)



# Egg data ----------------------------------------------------------------

eggs <- read_csv(
  "data/egg_data.csv",
  skip = 1,
  na = "N/A",
  col_names = c(
    "entry",
    "pool",
    "box_id",
    "attempt",
    "first_observed",
    "total_eggs",
    "total_hatched",
    "total_abandoned",
    "total_lost",
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
  mutate(pool =
           ifelse(pool == "E", "Pool2", ifelse(pool == "N", "Pool1", "Pool3")))


# Dumped data -------------------------------------------------------------
#
# These data are from hand rearrangment of above data for 
# Kirschman GLM analysis. Use it to plot dumped data

dumped <- read_csv(
  "data/glm_data.csv",
  skip = 1,
  col_names = c(
    "box_id",
    "pool",
    "attempt",
    "spp_code",
    "orig",
    "dump_nesting",
    "dumped_num",
    "hatched_num",
    "depth_mode",
    "width_mode",
    "coverage"
  )
) |>
  mutate(
    pool = if_else(
      pool == "E", "Pool2", if_else(
        pool == "N", "Pool1", "Pool3"
      )
    ))


# 
# eggs <- read_csv(
#   "data/egg_data.csv",
#   skip = 1,
#   na = "N/A",
#   col_names = c(
#     "entry",
#     "section",
#     "box_id",
#     "attempt",
#     "first_observed",
#     "total_eggs",
#     "hatched_eggs",
#     "abandoned_eggs",
#     "lost_eggs",
#     "wood_total",
#     "wood_hatched",
#     "wood_abandoned",
#     "wood_lost",
#     "merg_total",
#     "merg_hatched",
#     "merg_abandoned",
#     "merg_lost"
#   )
# ) |>
#   select(-ends_with("_eggs")) |>
#   drop_na()
