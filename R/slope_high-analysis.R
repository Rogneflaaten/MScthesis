# analysis of SR vesicle Ca2+ uptake rates at 600nM calcium concentrations
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


# combine sr.data with sex.data
data <- sr.data |>
  mutate(id = as.factor(as.numeric(id)),
         timepoint = factor(timepoint, levels = c("pre", "post"))) |>
  group_by(id) |>
  mutate(sample = c(0, 0, 1, 1)) |>
  left_join(sex.data) |>
  mutate(sex = factor(sex, levels = c("m", "f"))) |>
  right_join(ids)



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
         mhc = type1)



###### Analysis on slope.high ########

# with sex and sex:timepoint interaction
high_m1 <- lmer(log(slope.high) ~ timepoint * sex + (1 + timepoint | id), data = data)


# significant interaction of sex in change to post testing
summary(high_m1)

# model assumptions looks good
plot_model(high_m1, type='diag')




###### investigate if the observed difference in change between sexes is gone when accounting for MHC

data.mhc <- data |>
  left_join(mhc.dat)

# fit the model
high_m3 <- lmer(log(slope.high) ~ timepoint * se + mhc + (1 + timepoint | id), data = data.mhc)

summary(high_m3)

plot(high_m3)

saveRDS(high_m3, "./data/derivedData/calcium_uptake/high_m3.RDS")



# model with pooled data
high_m2 <- lmer(log(slope.high) ~ timepoint + sex + (1 + timepoint | id), data = data)

summary(high_m2)

#model assumptions looks good
plot_model(high_m2, type='diag')


# save models
saveRDS(high_m1, "./data/derivedData/calcium_uptake/high_m1.RDS")
saveRDS(high_m2, "./data/derivedData/calcium_uptake/high_m2.RDS")




###### descriptive data ######

pooled.data <- data |>
  right_join(ids) |>
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = tau:slope.cpa) |>
  group_by(id,
           timepoint, 
           variable, sex) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  mutate(sex = factor(sex, levels = c("m", "f")),
         variable = factor(variable,
                           levels = c("release.rate",
                                      "tau",
                                      "slope.high",
                                      "slope.low",
                                      "slope.cpa"),
                           labels = c("release",
                                      "tau",
                                      "high",
                                      "low",
                                      "leak"))) |>
  pivot_wider(names_from = timepoint,
              values_from = value) |> 
  group_by(variable) |>
  summarise(pre_m = mean(pre, na.rm = T),
            pre_sd = sd(pre, na.rm = T),
            post_m = mean(post, na.rm = T),
            post_sd = sd(post, na.rm = T)) |>
  filter(variable == "high") |>
  pivot_longer(names_to = "stat",
               values_to = "pooled",
               cols = pre_m:post_sd)



data.sum <- data |>
  right_join(ids) |>
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = tau:slope.cpa) |>
  group_by(id,
           timepoint, 
           variable, sex) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  mutate(sex = factor(sex, levels = c("m", "f")),
         variable = factor(variable,
                           levels = c("release.rate",
                                      "tau",
                                      "slope.high",
                                      "slope.low",
                                      "slope.cpa"),
                           labels = c("release",
                                      "tau",
                                      "high",
                                      "low",
                                      "leak"))) |>
  pivot_wider(names_from = timepoint,
              values_from = value) |> 
  group_by(variable,
           sex) |>
  summarise(pre_m = mean(pre, na.rm = T),
            pre_sd = sd(pre, na.rm = T),
            post_m = mean(post, na.rm = T),
            post_sd = sd(post, na.rm = T)) |>
  filter(variable == "high") |>
  pivot_longer(names_to = "stat",
               values_to = "value",
               cols = pre_m:post_sd) |>
  pivot_wider(names_from = sex,
              values_from = value) |>
  full_join(pooled.data) 


# save dataframe
saveRDS(data.sum, "./data/derivedData/calcium_uptake/high_data.RDS")