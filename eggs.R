library(tidyverse)
library(janitor)

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
  mutate(
    pool = if_else(
      pool == "E", "Pool2", if_else(
        pool == "N", "Pool1", "Pool3"
      )
    ))

egg1 <-
  eggs |>
  mutate(box = str_extract(box_id, "[0-9]+")) |>
  mutate(first_observed = as.Date(first_observed, "%d-%b")) |> 
  select(-entry) |>
  relocate(box, .after = section) |> 
  mutate(
    section = if_else(
      section == "E", "Pool2", if_else(
        section == "N", "Pool1", "Pool3"
      )
    ))
  


# dumped <- read_csv(
#   "data/glm_data.csv",
#   skip = 1,
#   col_names = c(
#     "box_id",
#     "section",
#     "attempt",
#     "spp_code",
#     "orig",
#     "dump_nesting",
#     "dumped_num",
#     "hatched_num",
#     "depth_mode",
#     "width_mode",
#     "coverage"
#   )
# )
# 
# dumped1 <- dumped |> 
#   mutate(box = str_extract(box_id, "[0-9]+")) |> 
#   relocate(box, .after = section) |> 
#   select(-c("box_id", "dump_nesting", "depth_mode", "width_mode", "coverage"))

x <- egg1 |>
  select(-c(total_eggs, total_abandoned, total_hatched, total_lost, box_id)) |>
  pivot_longer(
    cols = contains(c("wood", "merg")),
    names_to = c("species", ".value"),
    names_sep = "_"
  ) |> 
  select(-total) |> 
  pivot_longer(
    cols = c(hatched, abandoned, lost),
    names_to = "outcome",
    values_to = "number"
  )

# Plots -------------------------------------------------------------------

# Simple plot of total eggs per section.
# egg1 |>
#   group_by(section) |>
#   filter(total_eggs > 0) |> 
#   ggplot(aes(color = section)) +
#   geom_jitter(aes(x = section, y = total_eggs),
#               width = 0.1,
#               height = 0) +
#   xlab("Duck Creek Section") +
#   ylab("Total Eggs in Nest Box") +
#   scale_color_brewer(palette = "Dark2") +
#   theme_minimal()

hatched_eggs <- x |> 
  filter(outcome == "hatched" &
           number > 0)

merg_abandoned <- x |> 
  filter(box == 34 & species == "merg")

wood_abandoned <- x |> 
  filter(box %in% c(93, 4) & species == "wood" & section == "Pool2") |> 
  filter(!(box == 93 & attempt == 1))

e22_wodu <- tibble(
  section = "Pool2",
  box = "22",
  attempt = 2,
  first_observed = as_date("2024-06-02"),
  species = "wood",
  outcome = "hatched",
  number = 0
)

# s14_wodu <- tibble(
#   section = "South",
#   box = "14",
#   attempt = 1,
#   first_observed = as_date("2024-03-23"),
#   species = "wood",
#   outcome = "hatched",
#   number = 13
# )

hatched_eggs <- bind_rows(hatched_eggs, merg_abandoned, wood_abandoned, e22_wodu) |> 
  filter(outcome == "hatched") |> 
  mutate(number = ifelse(species == "merg" & box == 7, 11, number)) |> 
  filter(!(
    species == "wood" & 
      box == 7 &
      section == "Pool3" &
      attempt == 1))

mean_hatched <- hatched_eggs |> 
  group_by(section, species) |> 
  reframe(mean = mean(number))

mean_hatched

set.seed(42)

hatched_plot <- hatched_eggs |>
  group_by(section) |>
  ggplot(aes(shape = species, color = species)) +
  geom_jitter(
    aes(x = section, y = number),
    size = 2,
    width = 0.22,
    height = 0
  ) +
  xlab(NULL) +
  ylab("Number of hatched eggs") +
  scale_x_discrete(labels = c("Pool 1", "Pool 2", "Pool 3")) +
  scale_color_brewer(
    palette = "Dark2",
    name = NULL,
    labels = c("Hooded Merganser", "Wood Duck")) +
  scale_shape_discrete(
    name = NULL,
    labels = c("Hooded Merganser", "Wood Duck")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11)) +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(family = "Linux Libertine O"))

hatched_plot <- hatched_plot +
  geom_point(
    data = mean_hatched,
    aes(
      x = section,
      y = mean,
      color = species
    ),
    shape = 15,
    size = 3,
    show.legend = FALSE
  )

hatched_plot


ggsave(
  "hatched_eggs_plot.png",
  plot = hatched_plot,
  width = 1600,
  height = 900,
  units = "px",
  dpi = 300,
  bg = "white"
)

# Vector of weekly dates, beginning on earliest first_observed date
# for use in next plot.
egg_breaks <- seq(from = as.Date("2024-03-23"), to = as.Date("2024-06-08"),
                   by = "week")
egg1 |> 
  ggplot() +
  geom_point(
    aes(
      x = first_observed, 
      y = wood_hatched,
      color = section),
    position = position_jitter(width = 0.5, height = 0),
    size = 2,
    shape = 19
    ) +
  geom_point(
    aes(
      x = first_observed, 
      y = merg_hatched,
      color = section),
    position = position_jitter(width = 0.5, height = 0),
    size = 2,
    shape = 15
  ) +
  scale_x_date(
    breaks = egg_breaks, 
    minor_breaks = "days",
    date_labels = "%b %d") +
  labs(
    x = "Date eggs first observed in nest box",
    y = "Total eggs hatched") +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  annotate(
    geom = "text",
    x = as.Date("2024-05-06"),
    y = 5,
    label = "Circles: Woody. Squares: Hoody.")

