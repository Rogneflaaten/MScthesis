# analysis of relationship between training intensity and improved SR function
# 
# 


source("./R/libs.R")
source("./R/filter-id.R")

## import and combine data

# extract sex data from the cycling.data dataset
sex.data <- cycling.data |>
  filter(timepoint == 2,
         period == 1,
         test == "sub_25") |>
  select(id, sex)



# make a dataset with sr-, sex- and vo2.session data
intensity <- sr.data |>
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
  filter(!is.na(change)) |>
  select(-post) |>
  pivot_wider(names_from = variable,
              values_from = c(change, pre)) |>
  left_join(vo2.session.data) |>
  left_join(sex.data) 





# release rate
intensity_release <- lm(log(change_release.rate) ~ vo2.percent + pre_release.rate + sex, data = intensity)

# significent relationship: for every increase in vo2.percent the change in release rate decreased by ~9%
summary(intensity_release)

# log-transformed to meet model assumptions
plot(intensity_release)

# save model
saveRDS(intensity_release, "./data/derivedData/SR-intensity/intensity_release.RDS")






# tau
intensity_tau <- lm(log(change_tau) ~ vo2.percent + pre_tau + sex, data = intensity)

# no relationship
summary(intensity_tau)

# log-transformed to meet model assumptions but still bad
plot(intensity_tau)

# save model
saveRDS(intensity_tau, "./data/derivedData/SR-intensity/intensity_tau.RDS")






# slope.high
intensity_slope.high <- lm(log(change_slope.high) ~ vo2.percent + pre_slope.high + sex, data = intensity)

# no relationship
summary(intensity_slope.high)

# log-transformed to meet model assumptions (significant without log())
plot(intensity_slope.high)

# save model
saveRDS(intensity_slope.high, "./data/derivedData/SR-intensity/intensity_slope.high.RDS")







# slope.low
intensity_slope.low <- lm(log(change_slope.low) ~ vo2.percent + pre_slope.low + sex, data = intensity)

# no relationship
summary(intensity_slope.low)

# log-transformed to meet model assumptions (significant without log())
plot(intensity_slope.low)

# save model
saveRDS(intensity_slope.low, "./data/derivedData/SR-intensity/intensity_slope.low.RDS")






# slope.cpa
intensity_slope.cpa <- lm(log(change_slope.cpa) ~ vo2.percent + pre_slope.cpa + sex, data = intensity)

# no relationship
summary(intensity_slope.cpa)

# log-transformed to meet model assumptions
plot(intensity_slope.cpa)

# save model
saveRDS(intensity_slope.cpa, "./data/derivedData/SR-intensity/intensity_slope.cpa.RDS")




####### relationship between vo2 during interval sessions and mhc distribution ###########


mhc.dat <- mhc.data |>
  left_join(sex.data) |>
  right_join(ids) |>
  group_by(id,
           timepoint,
           type) |>
  summarise(value = mean(value, na.rm = T)) |>
  pivot_wider(names_from = type,
              values_from = value) |>
  mutate(id = as.factor(id),
         timepoint = as.factor(if_else(timepoint == 1, "pre", "post"))) |>
  select(id, 
         timepoint,
         type1) |>
  filter(timepoint == "pre") |> 
  left_join(intensity)



mhc.intensity <- lm(vo2.percent ~ type1, data = mhc.dat)

summary(mhc.intensity)

plot(mhc.intensity)

saveRDS(mhc.intensity, "./data/derivedData/SR-intensity/mhc.intensity.RDS")
