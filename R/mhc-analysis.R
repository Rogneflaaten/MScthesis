# analysis of MHC composition
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


# import mhc data

mhc.dat <- mhc.data |>
  left_join(sex.data) |>
  right_join(ids) |>
  group_by(id,
           timepoint,
           type,
           sex) |>
  summarise(value = mean(value, na.rm = T)) |>
  pivot_wider(names_from = type,
              values_from = value) |>
  mutate(id = as.factor(id),
         timepoint = as.factor(if_else(timepoint == 1, "pre", "post")),
         sex = factor(sex, levels = c("m", "f"))) |>
  select(id, 
         timepoint, 
         sex,
         mhc = type1) |>
  mutate(mhc = 100 - mhc)
  
  
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
  mutate(change = post - pre) |>
  mutate(across(everything(), ~replace_na(.x, NA))) |>
  filter(!is.na(change)) |>
  select(-post) |>
  pivot_wider(names_from = variable,
              values_from = c(change, pre))


change.data <- mhc.dat |>
  pivot_wider(names_from = timepoint,
              values_from = mhc) |>
  mutate(change_mhc = pre - post) |>
  left_join(sr.df)



###### analysis of group average change in mhc composition ######

mhc_m2 <- lmer(mhc ~ timepoint + sex + (1 | id), data = mhc.dat)

summary(mhc_m2)

plot(mhc_m2)

saveRDS(mhc_m2, "./data/derivedData/mhc_comp/mhc_m2.RDS")


pooled.data <- mhc.dat |>
  group_by(timepoint) |> 
  summarise(m = mean(mhc, na.rm = T),
            sd = sd(mhc, na.rm = T)) |>
  mutate(timepoint = str_c("pooled", timepoint, sep = "_"))


mhc.sum <- mhc.dat |>
  group_by(timepoint,
           sex) |>
  summarise(m = mean(mhc, na.rm = T),
            sd = sd(mhc, na.rm = T)) |>
  full_join(pooled.data)


# save dataframe
saveRDS(mhc.sum, "./data/derivedData/mhc_comp/mhc_sum.RDS")



###### analysis of sex differences in mhc composition ######

mhc_m1 <- lmer(mhc ~ timepoint * sex + (1 | id), data = mhc.dat)

summary(mhc_m1)

plot(mhc_m1)

saveRDS(mhc_m1, "./data/derivedData/mhc_comp/mhc_m1.RDS")



###### analysis of relationship between sr function and mhc composition at baseline ###########

# release rate
mhc_re.pre <- lm(pre_release.rate ~ pre, data = change.data)

summary(mhc_re.pre)

plot(mhc_re.pre)

saveRDS(mhc_re.pre, "./data/derivedData/mhc_comp/mhc_re.pre.RDS")



# tau
mhc_tau.pre <- lm(pre_tau ~ pre, data = change.data)

summary(mhc_tau.pre)

plot(mhc_tau.pre)

saveRDS(mhc_tau.pre, "./data/derivedData/mhc_comp/mhc_tau.pre.RDS")



# slope.high
mhc_slope.high.pre <- lm(pre_slope.high ~ pre, data = change.data)

summary(mhc_slope.high.pre)

plot(mhc_slope.high.pre)

saveRDS(mhc_slope.high.pre, "./data/derivedData/mhc_comp/mhc_slope.high.pre.RDS")



# slope.low
mhc_slope.low.pre <- lm(pre_slope.low ~ pre, data = change.data)

summary(mhc_slope.low.pre)

plot(mhc_slope.low.pre)

saveRDS(mhc_slope.low.pre, "./data/derivedData/mhc_comp/mhc_slope.low.pre.RDS")




# slope.cpa
mhc_slope.cpa.pre <- lm(pre_slope.cpa ~ pre, data = change.data)

summary(mhc_slope.cpa.pre)

plot(mhc_slope.cpa.pre)

saveRDS(mhc_slope.cpa.pre, "./data/derivedData/mhc_comp/mhc_slope.cpa.pre.RDS")






###### analysis of relationship between change in sr function and change in mhc composition ###########

# release rate
mhc_re.change <- lm(log(change_release.rate) ~ change_mhc + pre_release.rate + sex, data = change.data)

# no relationship
summary(mhc_re.change)

plot(mhc_re.change)

saveRDS(mhc_re.change, "./data/derivedData/mhc_comp/mhc_re.change.RDS")


# tau
mhc_tau.change <- lm(log(change_tau) ~ change_mhc + pre_tau + sex, data = change.data)

# no relationship
summary(mhc_tau.change)

plot(mhc_tau.change)

saveRDS(mhc_tau.change, "./data/derivedData/mhc_comp/mhc_tau.change.RDS")



# slope.high
mhc_slope.high.change <- lm(log(change_slope.high) ~ change_mhc + pre_slope.high + sex, data = change.data)

# no relationship
summary(mhc_slope.high.change)

plot(mhc_slope.high.change)

saveRDS(mhc_slope.high.change, "./data/derivedData/mhc_comp/mhc_slope.high.change.RDS")




# slope.low
mhc_slope.low.change <- lm(log(change_slope.low) ~ change_mhc + pre_slope.low + sex, data = change.data)

# relationship: for every %-point increase in MHC1 the change in slope.low decreased by -7.6% 
summary(mhc_slope.low.change)

plot(mhc_slope.low.change)

saveRDS(mhc_slope.low.change, "./data/derivedData/mhc_comp/mhc_slope.low.change.RDS")





# slope.cpa
mhc_slope.cpa.change <- lm(log(change_slope.cpa) ~ change_mhc + pre_slope.cpa + sex, data = change.data)

# no relationship
summary(mhc_slope.cpa.change)

plot(mhc_slope.cpa.change)

saveRDS(mhc_slope.cpa.change, "./data/derivedData/mhc_comp/mhc_slope.cpa.change.RDS")





###### General efficacy data (all fibertypes) #######

type.data <- mhc.data |>
  right_join(ids) |>
  group_by(id,
           timepoint,
           type) |>
  summarise(value = mean(value, na.rm = T)) |>
  pivot_wider(names_from = type,
              values_from = value) |>
  mutate(id = as.factor(id),
         timepoint = as.factor(if_else(timepoint == 1, "pre", "post")))




# type 1 model
type1_m1 <- lmer(type1 ~ timepoint + (1 | id), data = type.data)

summary(type1_m1)

plot(type1_m1)

saveRDS(type1_m1, "./data/derivedData/mhc_comp/type1_m1.RDS")




# type 2a model
type2a_m1 <- lmer(type2a ~ timepoint + (1 | id), data = type.data)

summary(type2a_m1)

plot(type2a_m1)

saveRDS(type2a_m1, "./data/derivedData/mhc_comp/type2a_m1.RDS")




# type 2x model
type2x_m1 <- lmer(type2x ~ timepoint + (1 | id), data = type.data)

summary(type2x_m1)

plot(type2x_m1)

saveRDS(type2x_m1, "./data/derivedData/mhc_comp/type2x_m1.RDS")




# summary dataframe

type.sum <- type.data |>
  pivot_longer(names_to = "type",
               values_to = "value",
               cols = type1:type2x) |>
  group_by(timepoint,
           type) |>
  summarise(m = mean(value, na.rm = T),
            sd = sd(value, na.rm = T))


saveRDS(type.sum, "./data/derivedData/mhc_comp/type.sum.RDS")
