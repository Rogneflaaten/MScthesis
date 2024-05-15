source("./R/libs.R")

# importing datasets

# calculate change in SR function
sr.df <- sr.data |>
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
                                      "slope.cpa"),
                           labels = c("release",
                                      "tau",
                                      "high",
                                      "low",
                                      "leak"))) |>
  pivot_wider(names_from = timepoint,
              values_from = value) |>
  mutate(pre = log(pre),
         post = log(post),
         change = (exp(post - pre) - 1) * 100) |>
  mutate(across(everything(), ~replace_na(.x, NA))) |>
  filter(!is.na(change)) |>
  select(-post) |>
  pivot_wider(names_from = variable,
              values_from = c(change, pre))



# calculate change in vo2max and combine with sr data 
data <- final.data |>
  select(id:test, vo2) |> 
  filter(test == "max",
         period == 1) |>
  pivot_wider(names_from = timepoint,
              values_from = vo2) |>
  mutate(id = as.factor(id),
         sex = factor(sex, levels = c("m", "f")),
         change_vo2max = post - pre) |>
  right_join(sr.df)


############# analysis of ca^2+^ handling and vo2max ###############


# release rate
vo2max_release <- lm(change_vo2max ~ change_release + pre + sex, data = data)

# no relationship
summary(vo2max_release)

plot(vo2max_release)

saveRDS(vo2max_release, "./data/derivedData/SR-vo2max/vo2max_release.RDS")





# tau
vo2max_tau <- lm(change_vo2max ~ change_tau + pre + sex, data = data)

# no relationship
summary(vo2max_tau)

plot(vo2max_tau)

saveRDS(vo2max_tau, "./data/derivedData/SR-vo2max/vo2max_tau.RDS")





# slope.high
vo2max_high <- lm(change_vo2max ~ change_high + pre + sex, data = data)

# no relationship
summary(vo2max_high)

plot(vo2max_high)

saveRDS(vo2max_high, "./data/derivedData/SR-vo2max/vo2max_high.RDS")






# slope.low
vo2max_low <- lm(change_vo2max ~ change_low + pre + sex, data = data)

# no relationship
summary(vo2max_low)

plot(vo2max_low)

saveRDS(vo2max_low, "./data/derivedData/SR-vo2max/vo2max_low.RDS")





# slope.cpa
vo2max_leak <- lm(change_vo2max ~ change_leak + pre + sex, data = data)

# no relationship
summary(vo2max_leak)

plot(vo2max_leak)

saveRDS(vo2max_leak, "./data/derivedData/SR-vo2max/vo2max_leak.RDS")






########## analysis of pre test values ############

norm.data <- final.data |>
  select(id:test, vo2.kg) |> 
  filter(test == "max",
         period == 1) |>
  pivot_wider(names_from = timepoint,
              values_from = vo2.kg) |>
  mutate(id = as.factor(id),
         sex = factor(sex, levels = c("m", "f")),
         change_vo2max = post - pre) |>
  right_join(sr.df) |>
  select(id,
         sex,
         pre,
         pre_release:pre_tau) |>
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = pre_release:pre_tau) |>
  group_by(variable) |>
  mutate(value = exp(value),
         value = value / max(value, na.rm = T) * 100) |>
  pivot_wider(names_from = variable,
              values_from = value)






vo2max_release.pre <- lm(pre ~ pre_release + sex, data = norm.data)

summary(vo2max_release.pre)

plot(vo2max_release.pre)

saveRDS(vo2max_release.pre, "./data/derivedData/SR-vo2max/vo2max_release.pre.RDS")





vo2max_tau.pre <- lm(pre ~ pre_tau + sex, data = norm.data)

summary(vo2max_tau.pre)

plot(vo2max_tau.pre)

saveRDS(vo2max_tau.pre, "./data/derivedData/SR-vo2max/vo2max_tau.pre.RDS")





vo2max_high.pre <- lm(pre ~ pre_high + sex, data = norm.data)

summary(vo2max_high.pre)

plot(vo2max_high.pre)

saveRDS(vo2max_high.pre, "./data/derivedData/SR-vo2max/vo2max_high.pre.RDS")




vo2max_low.pre <- lm(pre ~ pre_low + sex, data = norm.data)

summary(vo2max_low.pre)

plot(vo2max_low.pre)

saveRDS(vo2max_low.pre, "./data/derivedData/SR-vo2max/vo2max_low.pre.RDS")




vo2max_leak.pre <- lm(pre ~ pre_leak + sex, data = norm.data)

summary(vo2max_leak.pre)

plot(vo2max_leak.pre)

saveRDS(vo2max_leak.pre, "./data/derivedData/SR-vo2max/vo2max_leak.pre.RDS")

