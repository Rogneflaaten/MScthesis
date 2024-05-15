# analysis of SR vesicle Ca2+ uptake rates
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




###### Analysis on Tau ########

# with sex and sex:timepoint interaction

tau_m1 <- lmer(log(tau) ~ timepoint * sex + (1 + timepoint | id), data = data)

# no interaction of sex in change to post testing, this effect is disregarded to pool the data
summary(tau_m1)



# model with pooled data
tau_m2 <- lmer(log(tau) ~ timepoint + sex + (1 + timepoint | id), data = data)

summary(tau_m2)

# model assumptiums looks good
plot_model(tau_m2, type='diag')



# save model 
saveRDS(tau_m1, "./data/derivedData/calcium_uptake/tau_m1.RDS")
saveRDS(tau_m2, "./data/derivedData/calcium_uptake/tau_m2.RDS")



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
  filter(variable == "tau") |>
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
  filter(variable == "tau") |>
  pivot_longer(names_to = "stat",
               values_to = "value",
               cols = pre_m:post_sd) |>
  pivot_wider(names_from = sex,
              values_from = value) |>
  full_join(pooled.data) 


# save dataframe
saveRDS(data.sum, "./data/derivedData/calcium_uptake/tau_data.RDS")





























