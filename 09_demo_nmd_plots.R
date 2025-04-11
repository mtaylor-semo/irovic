
# Demo plot ---------------------------------------------------------------

# This is based on Frank's data so need to run 01_read_data.R and
# nmds.R first to have data to make the demo plot.

# Demo interpretation plot
# Use just four points to illustrate

trimmed_scores <- duck_scores |> 
  filter(box_id == "N93" |
           box_id == "E65" |
           box_id == "S34" |
           box_id == "N70") |> 
  mutate(box_id = replace(box_id, box_id == "N93", "P1")) |> 
  mutate(box_id = replace(box_id, box_id == "E65", "P2")) |> 
  mutate(box_id = replace(box_id, box_id == "S34", "P3")) |> 
  mutate(box_id = replace(box_id, box_id == "N70", "P4")) |> 
  select(-c(NMDS3, pool, species)) |> 
  slice(-2)

# P2, P3, P1, P4
trimmed_scores$x <- c(-0.36, 0.44, -0.47, 0.28)
trimmed_scores$y <- c(-0.08, 0.09, -0.1, 0.05)

trimmed_scores$zx <- c(-0.2, 0.27, -0.11, -0.0)
trimmed_scores$zy <- c(-0.39, 0.53, -0.2, -0.02)

demo_vars <- vars1.2

demo_vars <- demo_vars |> 
  mutate(vars = replace(vars, vars == "width", "Variable B")) |> 
  mutate(vars = replace(vars, vars == "hatched", "Variable A"))


p_demo <- ggplot(data = trimmed_scores, aes(x = NMDS1, y = NMDS2)) +
  geom_text(
    data = trimmed_scores,
    aes(x = NMDS1, y = NMDS2, label = box_id),
    color = "gray50",
    size = 1.5,
    hjust = -0.5
  ) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    limits = c(-0.6, 0.6),
    breaks = c(-0.4, -0.2, 0.0, 0.2, 0.4)
  ) +
  scale_y_continuous(
    limits = c(-0.5, 0.6),
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
    legend.position.inside = c(0.05, 0.95)
  ) +
  coord_fixed(ratio = 0.75)

p_demo <- p_demo +
  geom_segment(
    data = demo_vars,
    aes(
      x = 0,
      y = 0,
      xend = NMDS1,
      yend = NMDS2
    ),
    color = "gray70",
    linewidth = 0.2
  ) + # add dashed line extensions
  geom_segment(
    data = demo_vars,
    aes(
      x = 0,
      y = 0,
      xend = -NMDS1,
      yend = -NMDS2
    ),
    color = "gray70",
    linewidth = 0.2,
    linetype = 2
  ) + # dotted lines for points Variable A
  # geom_segment(
  #   data = trimmed_scores,
  #   aes(
  #     x = x,
  #     y = y,
  #     xend = NMDS1,
  #     yend = NMDS2
  #   ),
  #   color = "gray20",
  #   linewidth = 0.2,
  #   linetype = 3
  # ) + # dotted lines for Variable B
  geom_segment(
    data = trimmed_scores,
    aes(
      x = zx,
      y = zy,
      xend = NMDS1,
      yend = NMDS2
    ),
    color = "gray20",
    linewidth = 0.2,
    linetype = 3
  ) +
  geom_text(
    data = demo_vars,
    aes(x = NMDS1, y = NMDS2, label = vars),
    size = 2,
    hjust = "outward",
    vjust = "outward"
  )

p_demo

ggsave(
  "demo_plot.png",
  width = 1600,
  height = 900,
  units = "px",
  dpi = 300,
  bg = "white"
)



# NMDS plot used if k = 2 for nmds analysis.

# p2 <- ggplot(data = duck_scores, aes(x = NMDS1, y = NMDS2)) +
#   geom_text(
#     data = duck_scores,
#     aes(x = NMDS1, y = NMDS2, label = box_id),
#     color = "gray50",
#     size = 1.5,
#     hjust = -0.5
#   ) +
#   geom_point(aes(colour = species, shape = species), size = 1.5) +
#   scale_color_brewer(
#     palette = "Dark2",
#     name = NULL,
#     labels = c("Hooded Merganser", "Wood Duck")
#   ) +
#   #  scale_color_discrete(
#   #    name = NULL,
#   #    labels = c("Hooded Merganser", "Wood Duck")) +
#   scale_shape_discrete(name = NULL,
#                        labels = c("Hooded Merganser", "Wood Duck")) +
#   theme_minimal() +
#   guides(color = guide_legend(position = "inside")) +
#   theme(
#     panel.grid = element_blank(),
#     axis.text = element_text(size = 5),
#     axis.title = element_text(size = 5.5),
#     legend.text = element_text(
#       size = 6, margin = margin(l = 0.1, unit = "pt")),
#     legend.title = element_text(size = 6.5),
#     legend.key.spacing.y = unit(-7, "pt"),
#     legend.position.inside = c(0.15, 0.9)
#   )
#
#
# p2 <- p2 + geom_segment(
#   data = duck_vars,
#   aes(
#     x = 0,
#     y = 0,
#     xend = NMDS1,
#     yend = NMDS2
#   ),
#   color = "gray70",
#   linewidth = 0.2
# ) +
#   geom_text(
#     data = duck_vars,
#     aes(x = NMDS1, y = NMDS2, label = vars),
#     size = 2,
#     hjust = "outward",
#     vjust = "outward"
#   )
#
# p2
#
# ggsave(
#   "nmds_2axes.png",
#   width = 1600,
#   height = 900,
#   units = "px",
#   dpi = 300,
#   bg = "white"
# )
