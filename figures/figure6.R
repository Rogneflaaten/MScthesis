


source("./R/figures-source.R")
source("./R/libs.R")



# import models
release_m1 <- readRDS( "./data/derivedData/calcium_release/release_m1.RDS")
high_m1 <- readRDS("./data/derivedData/calcium_uptake/high_m1.RDS")
low_m1 <- readRDS( "./data/derivedData/calcium_uptake/low_m1.RDS")
tau_m1 <- readRDS("./data/derivedData/calcium_uptake/tau_m1.RDS")
leak_m1 <- readRDS( "./data/derivedData/calcium_leak/leak_m1.RDS")





# extract model statistics

estimates.m1 <- data.frame(summary(release_m1)$coef) |>
  full_join(data.frame(summary(tau_m1)$coef)) |>
  full_join(data.frame(summary(high_m1)$coef)) |>
  full_join(data.frame(summary(low_m1)$coef)) |>
  full_join(data.frame(summary(leak_m1)$coef)) |>
  mutate(variable = c(rep("release", 4),
                      rep("tau", 4),
                      rep("high", 4),
                      rep("low", 4),
                      rep("leak", 4)),
         coef = rep(c("intercept",
                      "change",
                      "sexf",
                      "sexf:change"),
                    5)) 


confint.m1 <- data.frame(confint(release_m1)) |> 
  full_join(data.frame(confint(tau_m1))) |>
  full_join(data.frame(confint(high_m1))) |>
  full_join(data.frame(confint(low_m1))) |>
  full_join(data.frame(confint(leak_m1))) |>
  mutate(variable = c(rep("release", 8),
                      rep("tau", 8),
                      rep("high", 8),
                      rep("low", 8),
                      rep("leak", 8)),
         coef = rep(c("",
                      "",
                      "",
                      "",
                      "intercept",
                      "change",
                      "sexf",
                      "sexf:change"),
                    5)) |>
  rename("lower.ci" = "X2.5..",
         "upper.ci" = "X97.5..") 


m1.data <- estimates.m1 |>
  left_join(confint.m1) |>
  filter(coef == "sexf:change" | coef == "sexf")





# make figure 4

figure6 <- m1.data |> 
  mutate(variable = factor(variable,
                           levels = c("leak",
                                      "low",
                                      "high",
                                      "tau",
                                      "release"),
                           labels = c(expression("Leak~rate"),
                                      expression("Uptake[200][nM]"),
                                      expression("Uptake[600][nM]"),
                                      expression(Uptake["tau"]),
                                      expression("Release~rate"))),
         Estimate = (exp(Estimate) - 1) * 100,
         lower.ci = (exp(lower.ci) - 1) * 100,
         upper.ci = (exp(upper.ci) - 1) * 100,
         coef = factor(coef, 
                       levels = c("sexf",
                                  "sexf:change"),
                       labels = c("Baseline value",
                                  "Response to training")),
         robust = case_when(upper.ci > 0 & lower.ci > 0 ~ "robust",
                            upper.ci < 0 & lower.ci < 0 ~ "robust",
                            .default = "notrobust")) |> 
  ggplot(aes(Estimate, variable, color = robust)) +
  geom_errorbar(aes(xmin = lower.ci, xmax = upper.ci),
                size = line_size,
                width = 0.08,
                color = "Black") +
  geom_point(size = 2.5,
             shape = 18) +
  facet_wrap(~coef,  
             nrow = 1, 
             strip.position = "top",
             scales = "free_x") +
  geom_vline(xintercept = 0,
             linetype = 2,
             size = line_size,
             color = "gray20") +
  ggh4x::facetted_pos_scales(
    x = list(scale_x_continuous(limits = c(-35, 110),
                                breaks = c(-25, 0, 25, 50, 75, 100),
                                expand = c(0, 0)), 
             scale_x_continuous(limits = c(-61, 31),
                                breaks = c(-60, -45, -30, -15, 0, 15, 30),
                                expand = c(0, 0)))) +
  
  scale_color_manual(values = c("grey20", "#d95f02")) +
  scale_y_discrete(labels = ~parse(text = .x)) +
  plot_theme() +
  labs(x = "% difference between sexes (females compared to males)") +
  theme(panel.border = element_rect(fill = NA),
        axis.title.y = element_blank(),
        strip.background = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8)) 




figure6 <- cowplot::plot_grid(
  figure6) +
  draw_plot_label(label = c("A)","B)"),
                  x = c(0.15, 0.58), 
                  y = c(0.97, 0.97),
                  hjust = 0.5, 
                  vjust = 0.5, 
                  size = label.size)






# save plot as an RDS object
saveRDS(figure6, "./figures/rds/figure6.RDS")


# Width of figure = 1x columns 8.9 cm
# height of figure = full page = 23 cm


# save plot as PDF
ggsave("figures/figure6.pdf", 
       plot = figure6, 
       width = 15, 
       height = 10 * 0.75, 
       dpi = 600,
       units = "cm", 
       device = cairo_pdf)

