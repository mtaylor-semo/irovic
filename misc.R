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






