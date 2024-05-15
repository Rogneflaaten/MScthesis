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
  filter(test == "max",
         period == 1) |>
  pivot_wider(names_from = timepoint,
              values_from = watt) |>
  mutate(id = as.factor(id),
         sex = factor(sex, levels = c("m", "f")),
         change_wmax = post - pre) |>
  right_join(sr.df)


############# analysis of ca^2+^ handling and wmax ###############


# release rate
wmax_release <- lm(change_wmax ~ change_release + pre + sex, data = data)

summary(wmax_release)

plot(wmax_release)

saveRDS(wmax_release, "./data/derivedData/SR-wmax/wmax_release.RDS")





# tau
wmax_tau <- lm(change_wmax ~ change_tau + pre + sex, data = data)

summary(wmax_tau)

plot(wmax_tau)

saveRDS(wmax_tau, "./data/derivedData/SR-wmax/wmax_tau.RDS")





# slope.high
wmax_high <- lm(change_wmax ~ change_high + pre + sex, data = data)

summary(wmax_high)

plot(wmax_high)

saveRDS(wmax_high, "./data/derivedData/SR-wmax/wmax_high.RDS")






# slope.low
wmax_low <- lm(change_wmax ~ change_low + pre + sex, data = data)

summary(wmax_low)

plot(wmax_low)

saveRDS(wmax_low, "./data/derivedData/SR-wmax/wmax_low.RDS")







# slope.cpa
wmax_leak <- lm(change_wmax ~ change_leak + pre + sex, data = data)

summary(wmax_leak)

plot(wmax_leak)

saveRDS(wmax_leak, "./data/derivedData/SR-wmax/wmax_leak.RDS")





########## analysis of pre test values ############

norm.data <- final.data |> 
  select(id:test, weight, watt) |> 
  filter(test == "max",
         period == 1) |>
  mutate(watt.kg = watt / weight) |>
  select(id:test, watt.kg) |> 
  pivot_wider(names_from = timepoint,
              values_from = watt.kg) |>
  mutate(id = as.factor(id),
         sex = factor(sex, levels = c("m", "f")),
         change_wmax = post - pre) |>
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






wmax_release.pre <- lm(pre ~ pre_release + sex, data = norm.data)

summary(wmax_release.pre)

plot(wmax_release.pre)

saveRDS(wmax_release.pre, "./data/derivedData/SR-wmax/wmax_release.pre.RDS")





wmax_tau.pre <- lm(pre ~ pre_tau + sex, data = norm.data)

summary(wmax_tau.pre)

plot(wmax_tau.pre)

saveRDS(wmax_tau.pre, "./data/derivedData/SR-wmax/wmax_tau.pre.RDS")





wmax_high.pre <- lm(pre ~ pre_high + sex, data = norm.data)

summary(wmax_high.pre)

plot(wmax_high.pre)

saveRDS(wmax_high.pre, "./data/derivedData/SR-wmax/wmax_high.pre.RDS")




wmax_low.pre <- lm(pre ~ pre_low + sex, data = norm.data)

summary(wmax_low.pre)

plot(wmax_low.pre)

saveRDS(wmax_low.pre, "./data/derivedData/SR-wmax/wmax_low.pre.RDS")




wmax_leak.pre <- lm(pre ~ pre_leak + sex, data = norm.data)

summary(wmax_leak.pre)

plot(wmax_leak.pre)

saveRDS(wmax_leak.pre, "./data/derivedData/SR-wmax/wmax_leak.pre.RDS")

