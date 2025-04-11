# Plots for egg data. 
# Run 01_read_data.R first
library(tidyverse)
library(janitor)
library(RColorBrewer)


# Wrangle -----------------------------------------------------------------

eggs_tmp <-
  eggs |>
  mutate(box = str_extract(box_id, "[0-9]+")) |>
  mutate(first_observed = as.Date(first_observed, "%d-%b")) |> 
  select(-entry) |>
  relocate(box, .after = pool)

eggs_tmp <- eggs_tmp |>
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

# Need to filter a few instances individually to separate
# them from similar data. Then put them back.
hatched_eggs <- eggs_tmp |> 
  filter(outcome == "hatched" &
           number > 0)

merg_abandoned <- eggs_tmp |> 
  filter(box == 34 & species == "merg")

wood_abandoned <- eggs_tmp |> 
  filter(box %in% c(93, 4) & species == "wood" & pool == "Pool2") |> 
  filter(!(box == 93 & attempt == 1))

e22_wodu <- tibble(
  pool = "Pool2",
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

# Final tibble for plotting
hatched_eggs <- bind_rows(hatched_eggs, merg_abandoned, wood_abandoned, e22_wodu) |> 
  filter(outcome == "hatched") |> 
  mutate(number = ifelse(species == "merg" & box == 7, 11, number)) |> 
  filter(!(
    species == "wood" & 
      box == 7 &
      pool == "Pool3" &
      attempt == 1))

# Get the means for plotting
# Mean eggs hatched by species by pool
mean_hatched <- hatched_eggs |> 
  group_by(pool, species) |> 
  reframe(mean = mean(number))

mean_hatched


# Analyses ----------------------------------------------------------------

t.test(number ~ species, data = hatched_eggs, var.equal = TRUE)

# Species by Pool interaction was significant.
summary(aov(number ~ species * pool, data = hatched_eggs))

eggs_by_species_aov <- aov(number ~ species * pool, data = hatched_eggs)

# See below for interaction plot



# Plots -------------------------------------------------------------------

hatched_plot <- hatched_eggs |>
  group_by(pool) |>
  ggplot(aes(shape = species, color = species)) +
  geom_point(
    aes(x = pool, y = number),
    size = 2,
    position = position_jitter(width = 0.22, height = 0, seed = 7) #7!
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
      x = pool,
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


# Interaction plots. First is base R.
# Second is ggplot version.

interaction.plot(
  x.factor = hatched_eggs$pool,
  trace.factor = hatched_eggs$species,
  response = hatched_eggs$number,
  ylab = "Mean number eggs hatched",
  trace.label = "Species"
)

mean_hatched |> 
  group_by(species) |> 
  ggplot() +
  geom_point(
    aes(x = pool, y = mean, color = species),
    size = 1) +
  geom_line(aes(x = pool, y = mean, group = species, color = species)) +
  scale_color_brewer(
    palette = "Dark2", 
    labels = c("Hooded Merganser", "Wood Duck")) +
  scale_y_continuous(
    breaks = c(2, 6, 10, 14)) +
  labs(
    x = NULL,
    y = "Mean number hatched eggs",
    color = "Species") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  theme(text = element_text(family = "Linux Libertine O"))

# Not Used ----------------------------------------------------------------



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
      color = pool),
    position = position_jitter(width = 0.5, height = 0),
    size = 2,
    shape = 19
    ) +
  geom_point(
    aes(
      x = first_observed, 
      y = merg_hatched,
      color = pool),
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


