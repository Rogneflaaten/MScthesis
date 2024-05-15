


source("./R/libs.R")
source("./R/filter-id.R")


# analysis of ftp over time

ftp.data <- session.data |>
  group_by(id) |>
  filter(!is.na(ftp)) |>
  mutate(timepoint = str_c(seq_len(length(ftp)),
                           "."),
         id = as.factor(id)) |>
  right_join(ids) |>
  filter(timepoint != "1.") |>
  mutate(timepoint = case_when(timepoint == "2." ~ "1.",
                               timepoint == "3." ~ "2.",
                               timepoint == "4." ~ "3.",
                               timepoint == "5." ~ "4.",
                               timepoint == "6." ~ "5."))


ftp_model <- lmer(ftp ~ timepoint + (1|id), data = ftp.data)

summary(ftp_model)

# save ftp model
saveRDS(ftp_model, "./data/derivedData/training/ftp_model.RDS")


ftp.sum <- ftp.data |>
  group_by(timepoint) |>
  summarise(m = mean(ftp, na.rm = T),
            sd = sd(ftp, na.rm = T))


saveRDS(ftp.sum, "./data/derivedData/training/ftp.sum.RDS")



# calculation of number of sessions (nses)
nses <- session.data |>
  mutate(id = as.factor(id)) |>
  right_join(ids) |>
  group_by(id) |>
  filter(!is.na(type)) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(percent = n / 24 * 100) |> print()
  summarise(mean = mean(n, na.rm = T),
            sd = sd(n, na.rm = T),
            min = min(n, na.rm = T),
            max = max(n, na.rm = T),
            mean_percent = mean(percent, na.rm = T),
            sd_percent = sd(percent, na.rm = T),
            min_percent = min(percent, na.rm = T),
            max_percent = max(percent, na.rm = T))


# save the dataframe
saveRDS(nses, "./data/derivedData/training/nses-data.RDS")



vo2ses <- vo2.session.data |>
  right_join(ids) |>
  summarise(m = mean(vo2.percent, na.rm = T),
            sd = sd(vo2.percent, na.rm = T),
            min = min(vo2.percent, na.rm = T),
            max = max(vo2.percent, na.rm = T)) 

saveRDS(vo2ses, "./data/derivedData/training/vo2ses-data.RDS")
