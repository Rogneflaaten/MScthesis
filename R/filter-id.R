# this script filters out participants missing SR data at pre or post test
#
# total participant number after exclusion is 37
#


source("./R/libs.R")


ids <- sr.data |>
  mutate(id = as.factor(as.numeric(id)),
         timepoint = factor(timepoint, levels = c("pre", "post"))) |>
  group_by(id) |>
  mutate(sample = c(0, 0, 1, 1)) |>
  filter(!is.na(protein)) |>
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = tau:slope.cpa) |>
  group_by(id,
           timepoint, 
           variable) |>
  summarise(value = mean(value, na.rm = T)) |>
  mutate(variable = factor(variable,
                           levels = c("release.rate",
                                      "tau",
                                      "slope.high",
                                      "slope.low",
                                      "slope.cpa"))) |>
  pivot_wider(names_from = timepoint,
              values_from = value) |>
  mutate(change = post - pre) |>
  mutate(across(everything(), ~replace_na(.x, NA))) |>
  filter(!is.na(change),
         variable == "tau") |>
  select(id)

