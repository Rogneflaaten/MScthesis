


source("./R/figures-source.R")
source("./R/libs.R")
source("./R/filter-id.R")


durability.data <- final.data |>
  ungroup() |>
  select(id:test, watt) |>
  filter(test == "max" | test == "per",
         period == 1) |>
  pivot_wider(names_from = test,
              values_from = watt) |>
  mutate(index = per / max * 100,
         id = as.factor(id)) |>
  select(-max, -per, -sex, -period) |>
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = index)


# change in endurance

data.A <- final.data |>
  ungroup() |>
  right_join(ids) |>
  filter(test %in% c("max", "per"),
         period == 1) |>
  select(id,
         timepoint,
         test,
         watt,
         vo2) |>
  pivot_wider(names_from = test,
              values_from = c(watt, vo2)) |>
  select(id,
         timepoint,
         per = watt_per,
         wmax = watt_max,
         vo2max = vo2_max) |>
  pivot_longer(names_to = "variable",
               values_to = "value",
               cols = per:vo2max) |>
  full_join(durability.data) |>
  mutate(variable = factor(variable,
                           levels = c("wmax",
                                      "per",
                                      "vo2max",
                                      "index")),
         timepoint = factor(timepoint, 
                            levels = c("pre", 
                                       "post"),
                            labels = c("Baseline",
                                       "Post test")))


max_vo2 <- readRDS("./data/derivedData/fitness/max_vo2.RDS")
max_watt <- readRDS("./data/derivedData/fitness/max_watt.RDS")
per_watt <- readRDS("./data/derivedData/fitness/per_watt.RDS")
dur_index <- readRDS("./data/derivedData/SR-durability/durability_change.RDS")


estimates.A <- data.frame(emmeans(max_vo2, specs = ~ timepoint)) |>
  full_join(data.frame(emmeans(max_watt, specs = ~ timepoint))) |>
  full_join(data.frame(emmeans(per_watt, specs = ~ timepoint))) |>
  full_join(data.frame(emmeans(dur_index, specs = ~ timepoint))) |>
  mutate(variable = c(rep("vo2max", 2),
                      rep("wmax", 2),
                      rep("per", 2),
                      rep("index", 2)),
         variable = factor(variable,
                           levels = c("wmax",
                                      "per",
                                      "vo2max",
                                      "index")),
         timepoint = factor(timepoint, 
                            levels = c("pre", 
                                       "post"),
                            labels = c("Baseline",
                                       "Post test"))) 



my_labeller.A <- as_labeller(c(vo2max = "VO[2][max]~(mL/min)", 
                                 wmax = "W[max]~(w)", 
                                 per = "W[15][min]~(w)",
                               index = "Durability~index"),
                               default = label_parsed)


plot.A <- estimates.A |>
  ggplot(aes(timepoint, emmean)) +
  geom_errorbar(aes(ymin = lower.CL, 
                    ymax = upper.CL),
                size = line_size,
                position = position_nudge(x = c(-0.2, 0.2)),
                width = 0.1) +
  geom_point(size = 2.5,
             color = "grey20",
             shape = 18,
             position = position_nudge(x = c(-0.2, 0.2))) +
  geom_line(data = data.A,
            aes(timepoint, value, group = id),
            color = "grey20",
            alpha = 0.5) +
  
  facet_wrap(~variable,
             ncol = 2,
             scales = "free_y",
             strip.position = "left",
             labeller = my_labeller.A) +
  
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(90, 390),
                                breaks = c(100, 150, 200, 250, 300, 350),
                                expand = c(0, 0)),
             scale_y_continuous(limits = c(50, 275),
                                breaks = c(60, 100, 140, 180, 220, 260),
                                expand = c(0, 0)),
             scale_y_continuous(limits = c(1200, 4800),
                                breaks = c(1500, 2000, 2500, 3000, 3500, 4000, 4500),
                                expand = c(0, 0)), 
             scale_y_continuous(limits = c(49, 75),
                                breaks = c(50, 54, 58, 62, 66, 70, 74),
                                expand = c(0, 0)))) +
  
  plot_theme() +
  
  theme(strip.background = element_blank(), 
        strip.placement = "outside",
        strip.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title = element_blank(),
        panel.border = element_rect(fill = NA)) 







# group change endurance

estimates.B <- data.frame(summary(max_watt)$coef) |>
  full_join(data.frame(summary(per_watt)$coef)) |>
  full_join(data.frame(summary(max_vo2)$coef)) |> 
  full_join(data.frame(summary(dur_index)$coef)) |> 
  mutate(variable = c(rep("wmax", 2),
                      rep("per", 2),
                      rep("vo2max", 2),
                      rep("index", 2)),
         variable = factor(variable,
                           levels = c("wmax",
                                      "per",
                                      "vo2max",
                                      "index")),
         coef = rep(c("intercept",
                      "change"), 
                    4))


confint.B <- data.frame(confint(max_watt)) |>
  full_join(data.frame(confint(per_watt))) |>
  full_join(data.frame(confint(max_vo2))) |>
  full_join(data.frame(confint(dur_index))) |>
  mutate(variable = c(rep("wmax", 4),
                      rep("per", 4),
                      rep("vo2max", 4),
                      rep("index", 4)),
         coef = rep(c("",
                      "",
                      "intercept",
                      "change"),
                    4)) |>
  rename("lower.ci" = "X2.5..",
         "upper.ci" = "X97.5..")


plot.B <- estimates.B |>
  left_join(confint.B) |> 
  select(-Std..Error,
         -t.value) |> 
  pivot_wider(names_from = coef,
              values_from = c(Estimate, lower.ci, upper.ci)) |>
  mutate(Estimate = Estimate_change / Estimate_intercept * 100,
         lower.ci = lower.ci_change / lower.ci_intercept * 100,
         upper.ci = upper.ci_change / upper.ci_intercept * 100,
         variable = factor(variable,
                           levels = c("index",
                                      "vo2max",
                                      "per",
                                      "wmax"),
                           labels = c(expression("Durability~index"),
                                      expression("VO[2][max]"),
                                      expression("W[15][min]"),
                                      expression("W[max]"))),
         robust = case_when(upper.ci > 0 & lower.ci > 0 ~ "robust",
                            upper.ci < 0 & lower.ci < 0 ~ "robust",
                            .default = "notrobust")) |> 
  ggplot(aes(Estimate, variable, color = robust)) +
  geom_errorbar(aes(xmin = lower.ci, xmax = upper.ci),
                size = line_size,
                width = 0.05,
                color = "Black") +
  geom_point(size = 2.5,
             shape = 18) +
  geom_vline(xintercept = 0,
             linetype = 2,
             size = line_size,
             color = "grey20") +
  scale_x_continuous(limits = c(-4, 14),
                     breaks = c(-4, -2, 0, 2, 4 ,6, 8, 10, 12, 14),
                     expand = c(0, 0)) +
  scale_y_discrete(labels = ~parse(text = .x)) +
  geom_hline(yintercept = 0,
             linetype = 2,
             size = line_size,
             color = "gray20") +
  labs(y = "",
       x = "% change from baseline") +
  scale_color_manual(values = c("grey20", "#d95f02")) +
  plot_theme() +
  theme(axis.title.x = element_text(size = 8),
        panel.border = element_rect(fill = NA),
        legend.position = "none",
        axis.text.y = element_text(size = 8,
                                   angle = 90,
                                   vjus = 0.5,
                                   hjust = 0.5))




# combine plots ##

figure3 <- cowplot::plot_grid(
  plot.A,
  plot.B,
  ncol = 2,
  rel_widths = c(2, 1)) +
  draw_plot_label(label = c("A)", "B)", "C)", "D)", "E)"),
                  x = c(0.06,
                        0.38,
                        0.06,
                        0.38,
                        0.71), 
                  y = c(0.98,
                        0.98,
                        0.50,
                        0.50,
                        0.98),
                  hjust = 0.5, 
                  vjust = 0.5, 
                  size = label.size)




# save plot as an RDS object
saveRDS(figure3, "./figures/rds/figure3.RDS")


# Width of figure = 1x columns 8.9 cm
# height of figure = full page = 23 cm


# save plot as PDF
ggsave("figures/figure3.pdf", 
       plot = figure3, 
       width = 12, 
       height = 18 * 0.75, 
       dpi = 600,
       units = "cm", 
       device = cairo_pdf)



