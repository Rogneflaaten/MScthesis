# analysis of relationship between changes in SR  calcium handling and changes in performance
# 
# 

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



# calculate change in performance and combine with sr data 
data <- final.data |>
  select(id:test, watt) |> 
  filter(test == "per",
         period == 1) |>
  pivot_wider(names_from = timepoint,
              values_from = watt) |>
  mutate(id = as.factor(id),
         sex = factor(sex, levels = c("m", "f")),
         change_per = post - pre) |>
  right_join(sr.df)

############# analysis of ca^2+^ handling and performance ###############


# release rate
per_release <- lm(change_per ~ change_release + pre + sex, data = data)

summary(per_release)

plot(per_release)

saveRDS(per_release, "./data/derivedData/SR-performance/per_release.RDS")





# tau
per_tau <- lm(change_per ~ change_tau + pre + sex, data = data)

summary(per_tau)

plot(per_tau)

saveRDS(per_tau, "./data/derivedData/SR-performance/per_tau.RDS")




# slope.high
per_high <- lm(change_per ~ change_high + pre + sex, data = data)

summary(per_high)

plot(per_high)

saveRDS(per_high, "./data/derivedData/SR-performance/per_high.RDS")






# slope.low
per_low <- lm(change_per ~ change_low + pre + sex, data = data)

summary(per_low)

plot(per_low)

saveRDS(per_low, "./data/derivedData/SR-performance/per_low.RDS")







# slope.cpa
per_leak <- lm(change_per ~ change_leak + pre + sex, data = data)

# no relationship
summary(per_leak)

plot(per_leak)

saveRDS(per_leak, "./data/derivedData/SR-performance/per_leak.RDS")






########## analysis of pre test values ############

norm.data <- final.data |> 
  select(id:test, weight, watt) |> 
  filter(test == "per",
         period == 1) |>
  mutate(watt.kg = watt / weight) |>
  select(id:test, watt.kg) |> 
  pivot_wider(names_from = timepoint,
              values_from = watt.kg) |>
  mutate(id = as.factor(id),
         sex = factor(sex, levels = c("m", "f")),
         change_per = post - pre) |>
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






per_release.pre <- lm(pre ~ pre_release + sex, data = norm.data)

summary(per_release.pre)

plot(per_release.pre)

saveRDS(per_release.pre, "./data/derivedData/SR-performance/per_release.pre.RDS")





per_tau.pre <- lm(pre ~ pre_tau + sex, data = norm.data)

summary(per_tau.pre)

plot(per_tau.pre)

saveRDS(per_tau.pre, "./data/derivedData/SR-performance/per_tau.pre.RDS")





per_high.pre <- lm(pre ~ pre_high + sex, data = norm.data)

summary(per_high.pre)

plot(per_high.pre)

saveRDS(per_high.pre, "./data/derivedData/SR-performance/per_high.pre.RDS")




per_low.pre <- lm(pre ~ pre_low + sex, data = norm.data)

summary(per_low.pre)

plot(per_low.pre)

saveRDS(per_low.pre, "./data/derivedData/SR-performance/per_low.pre.RDS")




per_leak.pre <- lm(pre ~ pre_leak + sex, data = norm.data)

summary(per_leak.pre)

plot(per_leak.pre)

saveRDS(per_leak.pre, "./data/derivedData/SR-performance/per_leak.pre.RDS")


