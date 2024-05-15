


source("./R/libs.R")
source("./R/filter-id.R")


per <- final.data |>
  select(id:test,
         watt,
         vo2,
         vo2.kg) |>
  filter(test == "per",
         period == 1) |>
  mutate(timepoint = factor(timepoint, c("pre", "post"))) |>
  right_join(ids)


max <- final.data |>
  select(id:test,
         watt,
         vo2,
         vo2.kg) |>
  filter(test == "max",
         period == 1) |>
  mutate(timepoint = factor(timepoint, c("pre", "post"))) |>
  right_join(ids) 


per_watt <- lmer(watt ~ timepoint + (1 | id), data = per)

summary(per_watt)

per.sum <- per |>
  group_by(timepoint) |>
  summarise(m = mean(watt, na.rm = T),
            sd = sd(watt, na.rm = T))

saveRDS(per.sum, "./data/derivedData/fitness/per.sum.RDS")
saveRDS(per_watt, "./data/derivedData/fitness/per_watt.RDS")


max_watt <- lmer(watt ~ timepoint + (1 | id), data = max)

summary(max_watt)

wmax.sum <- max |>
  group_by(timepoint) |>
  summarise(m = mean(watt, na.rm = T),
            sd = sd(watt, na.rm = T))

saveRDS(wmax.sum, "./data/derivedData/fitness/wmax.sum.RDS")
saveRDS(max_watt, "./data/derivedData/fitness/max_watt.RDS")



max_vo2 <- lmer(vo2 ~ timepoint + (1 | id), data = max)

summary(max_vo2)

vo2max.sum <- max |>
  group_by(timepoint) |>
  summarise(m = mean(vo2, na.rm = T),
            sd = sd(vo2, na.rm = T))

saveRDS(vo2max.sum, "./data/derivedData/fitness/vo2max.sum.RDS")
saveRDS(max_vo2, "./data/derivedData/fitness/max_vo2.RDS")



# calculate the fatigue index and merge with sr data 
durability.data <- final.data |>
  select(id:test, watt) |>
  filter(test == "max" | test == "per",
         period == 1) |>
  pivot_wider(names_from = test,
              values_from = watt) |>
  mutate(dur = per / max * 100,
         id = as.factor(id))


dur_m1 <- lmer(dur ~ timepoint + (1 | id), data = durability.data)

summary(dur_m1)

saveRDS(dur_m1, "./data/derivedData/fitness/dur_m1.RDS")


