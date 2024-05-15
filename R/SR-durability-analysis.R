# analysis of relationship between changes in SR calcium handling and durability
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
                                      "slope.cpa"))) |>
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



# calculate the fatigue index and merge with sr data 
durability.data <- final.data |>
  select(id:test, watt) |>
  filter(test == "max" | test == "per",
         period == 1) |>
  pivot_wider(names_from = test,
              values_from = watt) |>
  mutate(fatigue = per / max * 100,
         id = as.factor(id)) |>
  select(-max, -per) |>
  pivot_wider(names_from = timepoint,
              values_from = fatigue) |>
  mutate(index = post - pre,
         sex = factor(sex, levels = c("m", "f"))) |>
  right_join(sr.df)



dur.data <- durability.data |>
  select(id:pre) |>
  pivot_longer(names_to = "timepoint",
               values_to = "value",
               cols = post:pre) |>
  mutate(timepoint = factor(timepoint, levels = c("pre", "post")))


############## analysis of change in durability index ################

durability_change <- lmer(value ~ timepoint + (1 | id), data = dur.data)

summary(durability_change)

plot(durability_change)

saveRDS(durability_change, "./data/derivedData/SR-durability/durability_change.RDS")




dur.sum <- dur.data |>
  group_by(timepoint) |>
  summarise(m = mean(value, na.rm = T),
           sd = sd(value, na.rm = T))


# save dataframe
saveRDS(dur.sum, "./data/derivedData/SR-durability/dur_sum.RDS")




############# analysis of ca^2+^ handling and durability ###############


# release rate
durability_re <- lm(index ~ change_release.rate + pre + sex, data = durability.data)

# no relationship
summary(durability_re)

plot(durability_re)

saveRDS(durability_re, "./data/derivedData/SR-durability/durability_re.RDS")





# tau
durability_tau <- lm(index ~ change_tau + pre + sex, data = durability.data)

# no relationship
summary(durability_tau)

plot(durability_tau)

saveRDS(durability_tau, "./data/derivedData/SR-durability/durability_tau.RDS")





# slope.high
durability_high <- lm(index ~ change_slope.high + pre + sex, data = durability.data)

# no relationship
summary(durability_high)

plot(durability_high)

saveRDS(durability_high, "./data/derivedData/SR-durability/durability_high.RDS")






# slope.low
durability_low <- lm(index ~ change_slope.low + pre + sex, data = durability.data)

summary(durability_low)

plot(durability_low)

saveRDS(durability_low, "./data/derivedData/SR-durability/durability_low.low.RDS")







# slope.cpa
durability_leak <- lm(index ~ change_slope.cpa + pre + sex, data = durability.data)

# no relationship
summary(durability_leak)

plot(durability_leak)

saveRDS(durability_leak, "./data/derivedData/SR-durability/durability_leak.RDS")




########## analysis of pre test values ############

norm.data <- durability.data |>
  select(id,
         sex,
         pre,
         pre_release.rate:pre_tau) |>
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = pre_release.rate:pre_tau) |>
  group_by(variable) |>
  mutate(value = exp(value),
         value = value / max(value, na.rm = T) * 100) |>
  pivot_wider(names_from = variable,
              values_from = value)






dur_release.pre <- lm(pre ~ pre_release.rate + sex, data = norm.data)

summary(dur_release.pre)

plot(dur_release.pre)

saveRDS(dur_release.pre, "./data/derivedData/SR-durability/dur_release.pre.RDS")





dur_tau.pre <- lm(pre ~ pre_tau + sex, data = norm.data)

summary(dur_tau.pre)

plot(dur_tau.pre)

saveRDS(dur_tau.pre, "./data/derivedData/SR-durability/dur_tau.pre.RDS")





dur_high.pre <- lm(pre ~ pre_slope.high + sex, data = norm.data)

summary(dur_high.pre)

plot(dur_high.pre)

saveRDS(dur_high.pre, "./data/derivedData/SR-durability/dur_high.pre.RDS")




dur_low.pre <- lm(pre ~ pre_slope.low + sex, data = norm.data)

summary(dur_low.pre)

plot(dur_low.pre)

saveRDS(dur_low.pre, "./data/derivedData/SR-durability/dur_low.pre.RDS")




dur_leak.pre <- lm(pre ~ pre_slope.cpa + sex, data = norm.data)

summary(dur_leak.pre)

plot(dur_leak.pre)

saveRDS(dur_leak.pre, "./data/derivedData/SR-durability/dur_leak.pre.RDS")



