


source("./R/figures-source.R")
source("./R/libs.R")
source("./R/filter-id.R")



type1_m1 <- readRDS("./data/derivedData/mhc_comp/type1_m1.RDS")
type2a_m1 <- readRDS("./data/derivedData/mhc_comp/type2a_m1.RDS")
type2x_m1 <- readRDS("./data/derivedData/mhc_comp/type2x_m1.RDS")


type.data <- mhc.data |>
  right_join(ids) |>
  group_by(id,
           timepoint,
           type) |>
  summarise(value = mean(value, na.rm = T)) |>
  mutate(id = as.factor(id),
         timepoint = as.factor(if_else(timepoint == 1, "Baseline", "Post test")))




estimates.A <- data.frame(emmeans(type1_m1, specs = ~ timepoint)) |>
  full_join(data.frame(emmeans(type2a_m1, specs = ~ timepoint))) |>
  full_join(data.frame(emmeans(type2x_m1, specs = ~ timepoint))) |>
  mutate(type = c(rep("type1", 2),
                      rep("type2a", 2),
                      rep("type2x", 2)),
         type = factor(type,
                           levels = c("type1",
                                      "type2a",
                                      "type2x")),
         timepoint = factor(timepoint, 
                            levels = c("pre", 
                                       "post"),
                            labels = c("Baseline",
                                       "Post test"))) 



my_labeller.A <- as_labeller(c(type1 = "MHC-I", 
                               type2a = "MHC-IIa", 
                               type2x = "MHC-IIx"),
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
  geom_line(data = type.data,
            aes(timepoint, value, group = id),
            color = "grey20",
            alpha = 0.5) +
  
  facet_wrap(~type,
             ncol = 3,
             scales = "free_y",
             strip.position = "top",
             labeller = my_labeller.A) +
  
  ggh4x::facetted_pos_scales(
    y = list(scale_y_continuous(limits = c(29, 76),
                                breaks = c(30, 37.5, 45, 52.5, 60, 67.5, 75),
                                expand = c(0, 0)), 
             scale_y_continuous(limits = c(20, 52),
                                breaks = c(25, 30, 35, 40, 45, 50),
                                expand = c(0, 0)),
             scale_y_continuous(limits = c(-1, 31),
                                breaks = c(0, 5, 10, 15, 20, 25, 30),
                                expand = c(0, 0)))) +

  labs(y = "Relative distribution of MHC isoform (%)") +
  
  plot_theme() +
  
  theme(strip.background = element_blank(), 
        strip.placement = "outside",
        strip.text = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 8),
        panel.border = element_rect(fill = NA)) 




# group change endurance

estimates.B <- data.frame(summary(type1_m1)$coef) |>
  full_join(data.frame(summary(type2a_m1)$coef)) |>
  full_join(data.frame(summary(type2x_m1)$coef)) |> 
  mutate(type = c(rep("type1", 2),
                      rep("type2a", 2),
                      rep("type2x", 2)),
         type = factor(type,
                           levels = c("type1",
                                      "type2a",
                                      "type2x")),
         coef = rep(c("intercept",
                      "change"), 
                    3))


confint.B <- data.frame(confint(type1_m1)) |>
  full_join(data.frame(confint(type2a_m1))) |>
  full_join(data.frame(confint(type2x_m1))) |>
  mutate(type = c(rep("type1", 4),
                      rep("type2a", 4),
                      rep("type2x", 4)),
         coef = rep(c("",
                      "",
                      "intercept",
                      "change"),
                    3)) |>
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
         type = factor(type,
                           levels = c("type1",
                                      "type2a",
                                      "type2x"),
                           labels = c(expression("MHC-I"),
                                      expression("MHC-IIa"),
                                      expression("MHC-IIx"))),
         robust = case_when(upper.ci > 0 & lower.ci > 0 ~ "robust",
                            upper.ci < 0 & lower.ci < 0 ~ "robust",
                            .default = "notrobust")) |> 
  ggplot(aes(type, Estimate, color = robust)) +
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci),
                size = line_size,
                width = 0.05,
                color = "Black") +
  geom_point(size = 2.5,
             shape = 18) +
  geom_hline(yintercept = 0,
             linetype = 2,
             size = line_size,
             color = "grey20") +
  scale_y_continuous(limits = c(-23, 16),
                     breaks = c(-20, -15, -10, -5, 0, 5, 10, 15),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = ~parse(text = .x)) +
  geom_vline(xintercept = 0,
             linetype = 2,
             size = line_size,
             color = "gray20") +
  labs(x = "",
       y = "% change from baseline") +
  scale_color_manual(values = c("grey20", "#d95f02")) +
  plot_theme() +
  theme(axis.text.y = element_text(size = 8,
                                   angle = 0,
                                   hjust = 0.5),
        axis.title.y = element_text(size = 8),
        panel.border = element_rect(fill = NA),
        legend.position = "none",
        axis.text.x = element_text(size = 8))





# combine plots ##

figure4 <- cowplot::plot_grid(
  plot.A,
  plot.B,
  nrow = 2,
  rel_heights = c(1.75, 1)) +
  draw_plot_label(label = c("A)", "B)", "C)", "D)"),
                  x = c(0.04,
                        0.38,
                        0.70,
                        0.02), 
                  y = c(0.98,
                        0.98,
                        0.98,
                        0.38),
                  hjust = 0.5, 
                  vjust = 0.5, 
                  size = label.size)




# save plot as an RDS object
saveRDS(figure4, "./figures/rds/figure4.RDS")


# Width of figure = 1x columns 8.9 cm
# height of figure = full page = 23 cm


# save plot as PDF
ggsave("figures/figure4.pdf", 
       plot = figure4, 
       width = 12, 
       height = 18 * 0.75, 
       dpi = 600,
       units = "cm", 
       device = cairo_pdf)



