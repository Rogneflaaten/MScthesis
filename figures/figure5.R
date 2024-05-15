

source("./R/figures-source.R")
source("./R/libs.R")





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
  filter(!is.na(protein)) |>
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = tau:slope.cpa) |>
  group_by(id,
           timepoint, 
           variable) |>
  summarise(value = mean(value, na.rm = T)) |>
  left_join(sex.data) |>
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
  mutate(pre = log(pre),
         post = log(post),
         change = (exp(post - pre) - 1) * 100) |>
  mutate(across(everything(), ~replace_na(.x, NA))) |>
  filter(!is.na(change)) 



# import models
release_m2 <- readRDS( "./data/derivedData/calcium_release/release_m2.RDS")
high_m2 <- readRDS("./data/derivedData/calcium_uptake/high_m2.RDS")
low_m2 <- readRDS( "./data/derivedData/calcium_uptake/low_m2.RDS")
tau_m2 <- readRDS("./data/derivedData/calcium_uptake/tau_m2.RDS")
leak_m2 <- readRDS( "./data/derivedData/calcium_leak/leak_m2.RDS")



# save model estimates in a dataframe
estimates.m2 <- data.frame(summary(release_m2)$coef) |>
  full_join(data.frame(summary(tau_m2)$coef)) |>
  full_join(data.frame(summary(high_m2)$coef)) |>
  full_join(data.frame(summary(low_m2)$coef)) |>
  full_join(data.frame(summary(leak_m2)$coef)) |>
  mutate(variable = c(rep("release", 3),
                      rep("tau", 3),
                      rep("high", 3),
                      rep("low", 3),
                      rep("leak", 3)),
         coef = rep(c("intercept",
                      "change",
                      "sexf"),
                    5)) 



# save model 95% confidence intervals in a dataframe
confint.m2 <- data.frame(confint(release_m2)) |>
  full_join(data.frame(confint(tau_m2))) |>
  full_join(data.frame(confint(high_m2))) |>
  full_join(data.frame(confint(low_m2))) |>
  full_join(data.frame(confint(leak_m2))) |>
  mutate(variable = c(rep("release", 7),
                      rep("tau", 7),
                      rep("high", 7),
                      rep("low", 7),
                      rep("leak", 7)),
         coef = rep(c("",
                      "",
                      "",
                      "",
                      "intercept",
                      "change",
                      "sexf"),
                    5)) |>
  rename("lower.ci" = "X2.5..",
         "upper.ci" = "X97.5..") 




# make Plot A
plot.A <- data |>
  ggplot(aes(variable, change)) +
  geom_point(position = position_jitter(width = 0.1,
                                        seed = 1),
             size = 1,
             alpha = 0.3) +
  geom_hline(yintercept = 0,
             linetype = 2,
             size = line_size,
             color = "grey20") +
  scale_y_continuous(limits = c(-100, 300),
                     breaks = c(-100, -50, 0, 50, 100, 150, 200, 250, 300),
                     expand = c(0, 0)) +
  plot_theme() +
  labs(y = "% change from baseline") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_text(size = 8))




# make plot B
plot.B <- estimates.m2 |>
  left_join(confint.m2) |>
  filter(coef == "change") |>
  mutate(variable = factor(variable,
                           levels = c("release",
                                      "tau",
                                      "high",
                                      "low",
                                      "leak"),
                           labels = c(expression("Release~rate"),
                                      expression(Uptake["tau"]),
                                      expression("Uptake[600][nM]"),
                                      expression("Uptake[200][nM]"),
                                      expression("Leak~rate"))),
         Estimate = (exp(Estimate) - 1) * 100,
         lower.ci = (exp(lower.ci) - 1) * 100,
         upper.ci = (exp(upper.ci) - 1) * 100,
         robust = case_when(upper.ci > 0 & lower.ci > 0 ~ "robust",
                            upper.ci < 0 & lower.ci < 0 ~ "robust",
                            .default = "notrobust")) |>
  ggplot(aes(variable, Estimate, color = robust)) +
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                size = line_size,
                width = 0.05,
                color = "Black") +
  geom_point(size = 2.5,
             #color = "#d95f02",
             shape = 18) +
  geom_hline(yintercept = 0,
             linetype = 2,
             size = line_size,
             color = "grey20") +
  scale_y_continuous(limits = c(-35, 24),
                     breaks = c(-32, -24, -16, -8, 0, 8, 16, 24),
                     expand = c(0, 0)) +
  scale_color_manual(values = c("grey20", "#d95f02")) +
  scale_x_discrete(labels = ~parse(text = .x)) +
  plot_theme() +
  labs(y = "Average change (% from baseline)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        legend.position = "none")




## combine plots ##

figure5 <- cowplot::plot_grid(
  plot.A, 
  plot.B,
  nrow = 2,
  align = "v") +
  draw_plot_label(label = c("A)",  "B)"),
                  x = c(0.02, 0.02), 
                  y = c(0.98, 0.48),
                  hjust = 0.5, 
                  vjust = 0.5, 
                  size = label.size)


# save plot as an RDS object
saveRDS(figure5, "./figures/rds/figure5.RDS")


# Width of figure = 1x columns 8.9 cm
# height of figure = full page = 23 cm


# save plot as PDF
ggsave("figures/figure5.pdf", 
       plot = figure5, 
       width = 12, 
       height = 17 * 0.75, 
       dpi = 600,
       units = "cm", 
       device = cairo_pdf)



