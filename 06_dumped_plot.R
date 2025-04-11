# Plot the dumped data
# Run 01_read_data.R first
library(tidyverse)

dumped1 <- dumped |>
  mutate(box = str_extract(box_id, "[0-9]+")) |>
  relocate(box, .after = pool) |>
  mutate(species = if_else(spp_code == "M", "merg", "wood")) |> 
  select(-c(
    "spp_code",
    "dump_nesting",
    "depth_mode",
    "width_mode",
    "coverage"
  )) |>
  filter(dumped_num > 0)

pool <- c("Pool2", "Pool2", "Pool2", "Pool2", "Pool1", "Pool1", "Pool3", "Pool3", "Pool3", "Pool3", "Pool3")
dumped_num <- c(1, 3, 4, 5, 2, 4, 2, 3, 4, 6, 10)
box <- c("7/65", "62", "22", "62", "93", "70", "14/44", "7", "20", "20", "7")
species <- c("wood", "wood", "wood", "wood", "merg", "wood", "wood", "wood", "wood", "wood", "merg")


# Separate tibble to make labels of box numbers that are used
# in the final plot.
dlabs <- tibble(pool, dumped_num, box, species)

dumped_plot <- dumped1 |>
  group_by(pool) |>
  ggplot(aes(shape = species, color = species)) +
  geom_point(data = dumped1, aes(x = pool, y = dumped_num), size = 2) +
  scale_y_continuous(breaks = c(2, 4, 6, 8 , 10)) +
  xlab(NULL) +
  ylab("Number of dumped eggs") +
  scale_color_brewer(
    palette = "Dark2",
    name = NULL,
    labels = c("Hooded Merganser", "Wood Duck")
  ) +
  scale_x_discrete(labels = c("Pool 1", "Pool 2", "Pool 3")) +
  scale_shape_discrete(name = NULL,
                       labels = c("Hooded Merganser", "Wood Duck")) +
  theme_minimal() +
  theme(text = element_text(family = "Linux Libertine O")) +
  theme(axis.text.x = element_text(size = 11)) +
  theme(panel.grid.minor = element_blank())

dumped_plot <- dumped_plot +
  geom_text(
    data = dlabs,
    aes(x = pool, y = dumped_num, label = box),
    nudge_x = 0.25,
    color = "black",
    show.legend = FALSE,
    family = "Linux Libertine O",
    size = 3
  )


dumped_plot

ggsave(
  "dumped_eggs_plot.png",
  plot = dumped_plot,
  width = 1600,
  height = 900,
  units = "px",
  dpi = 300,
  bg = "white"
)
