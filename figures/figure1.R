

source("./R/figures-source.R")
source("./R/libs.R")
source("./R/filter-id.R")



# design figures

arrow_glyph <- "\U2193"


#### Design graph ####

# vo2 session label
label <- "VO[2]~measurement~session"


design.A <- data.frame(weeks = seq(1:8),
                       sessions = c(2, 3, 4, 2, 3, 4, 3, 3)) |> 
  ggplot(aes(weeks, sessions)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.5),
           width = 0.65, 
           size = 0,
           color = "Black",
           fill = "gray90") +
  
  scale_x_continuous(breaks = seq(1:8), 
                     labels = c("1", "2", "3", "4", "5", "6", "7", "8"),
                     limits = c(-3.5, 10), 
                     expand = c(0, 0)) +
  
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
                     limits = c(0, 6), 
                     expand = c(0, 0), 
                     labels = c("1", "2", "3", "4", "5")) +

  annotate("text", x = c(-3.2, -3.2, -3.2), y = c(4.4, 3.6, 2.8), 
           label = c("4x5min session",
                     "Muscle biopsy",
                     "Endurance tesing"), 
           vjust = 0, 
           hjust = 0, 
           size = 2.65,
           color = c("Blue", 
                     "Purple", 
                     "Red")) +
  
  annotate("text",
           x = -3.2,
           y = 5.2,
           label = label,
           parse = TRUE,
           vjust = 0, 
           hjust = 0, 
           size = 2.65,
           color = "Black") +


  # Endurance tests
  annotate("text",
           x = c(-0.8, 0.1, 0.3, 8.9), 
           y = rep(2.8), 
           label = rep(arrow_glyph, 4), 
           vjust = 0, 
           size = 4.5,
           color = "Red") +

  
  # Biopsies
  annotate("text", 
           x = c(-0.2, 8.6), 
           y = rep(3.6), 
           label = rep(arrow_glyph, 2), 
           vjust = 0, 
           size = 4.5,
           color = "Purple") +
  
  # Standardized Session
  annotate("text", 
           x = c(1, 2, 4, 6, 8), 
           y = rep(4.4), 
           label = arrow_glyph, 
           vjust = 0, 
           size = 4.5,
           color = "Blue") +
  
  # Vo2 sessions 
  annotate("text", 
           x = c(3, 6), 
           y = rep(5.2), 
           label = rep(arrow_glyph, 2), 
           vjust = 0, 
           size = 4.5,
           color = "Black") +
  
  
  xlab("Time (weeks)") +
  ylab("Number of sessions") +
  plot_theme() +
  theme(legend.position = "none", 
        legend.direction = "vertical", 
        legend.title = element_blank(), 
        legend.background = element_blank(),
        legend.spacing.x = unit(0.25, "lines"),
        axis.title = element_text(size = 8))







# make plot B

# make session graphs
data.B <- data.frame(time = rep(seq(0, 45.25, by = 0.25), times = 3),
                       session = c(rep("4x5min maximal effort", times = 182),
                                   rep("4x8min 30/15's at 108% and 54%, respectively", times = 182),
                                   rep("6x6min at 65%", times = 182)),
                       part = c(rep("warmup", times = 70),
                                rep("training", times = 112),
                                rep("warmup", times = 28),
                                rep("training", times = 154),
                                rep("warmup", times = 16),
                                rep("training", times = 166)),
                       load = c(
                         # 4 x 5min session
                         rep(0, times = 1),
                         rep(50, times = 12), 
                         rep(70, times = 8), 
                         rep(90, times = 4), 
                         rep(30, times = 10), 
                         rep(50, times = 12), 
                         rep(70, times = 8), 
                         rep(90, times = 4),
                         rep(30, times = 12),
                         rep(100, times = 20),
                         rep(30, times = 10),
                         rep(100, times = 20),
                         rep(30, times = 10),
                         rep(100, times = 20),
                         rep(30, times = 10),
                         rep(100, times = 20),
                         rep(0, times = 1),
                         
                         # 4 x 8min session
                         rep(0, times = 1),
                         rep(50, times = 8), 
                         rep(70, times = 8), 
                         rep(90, times = 4), 
                         rep(30, times = 8), 
                         rep(108, times = 2), 
                         rep(54, times = 1), 
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2),
                         rep(30, times = 8),
                         rep(108, times = 2), 
                         rep(54, times = 1), 
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2),
                         rep(30, times = 8),
                         rep(108, times = 2), 
                         rep(54, times = 1), 
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2),
                         rep(30, times = 8),
                         rep(108, times = 2), 
                         rep(54, times = 1), 
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2), 
                         rep(54, times = 1),
                         rep(108, times = 2),
                         rep(0, times = 1),
                         
                         # 6 x 6min session
                         rep(0, times = 1),
                         rep(50, times = 6), 
                         rep(70, times = 6), 
                         rep(50, times = 4), 
                         rep(65, times = 24), 
                         rep(30, times = 4), 
                         rep(65, times = 24), 
                         rep(30, times = 4), 
                         rep(65, times = 24), 
                         rep(30, times = 4), 
                         rep(65, times = 24), 
                         rep(30, times = 4), 
                         rep(65, times = 24), 
                         rep(30, times = 4), 
                         rep(65, times = 24),
                         rep(0, times = 1)
                       )) 



design.B <- data.B |>
  ggplot(aes(time, load)) +
  geom_step(data = data.B |> filter(part == "training"),
            size = line_size) +
  geom_step(data = data.B |> filter(part == "warmup"),
            size = line_size,
            linetype = 4) +
  scale_y_continuous(limits = c(0, 125),
                     breaks = c(0, 25, 50, 75, 100, 125),
                     expand = c(0.05, 0)) +
  scale_x_continuous(limits = c(0, 45.25),
                     breaks = c(0, 10, 20, 30, 40),
                     expand = c(0.05, 0)) +
  labs(y = expression("%"~of~W[4][x][5][min]),
       x = "Time (minutes)") +
  facet_wrap(~session,
             nrow = 3) +
  plot_theme() +
  theme(strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size = 7),
        axis.title = element_text(size = 8))





# make plot C


data.C <- session.data |>
  group_by(id) |>
  filter(!is.na(ftp)) |>
  mutate(timepoint = str_c(seq_len(length(ftp)),
                           "."),
         id = as.factor(id)) |> 
  right_join(ids) |>
  select(id, ftp, timepoint) |>
  filter(timepoint != "1.") |>
  mutate(timepoint = case_when(timepoint == "2." ~ "1.",
                               timepoint == "3." ~ "2.",
                               timepoint == "4." ~ "3.",
                               timepoint == "5." ~ "4.",
                               timepoint == "6." ~ "5."))


ftp_model <- readRDS("./data/derivedData/training/ftp_model.RDS")


estimates.C <- emmeans(ftp_model, specs = ~ timepoint) |>
  data.frame() 

design.C <- data.C |>
  ggplot(aes(timepoint, ftp, group = id)) +
  geom_line(size = line_size,
            color = "Blue",
            alpha = 0.3) +
  geom_line(data = estimates.C,
            aes(timepoint, emmean, group = 1),
            size = line_size,
            color = "Black",
            linetype = 3) +
  geom_errorbar(data = estimates.C,
                aes(timepoint, emmean, group = 1, ymin = lower.CL, ymax = upper.CL),
                size = line_size,
                width = 0.2,
                color = "Black") +
  geom_point(data = estimates.C,
             aes(timepoint, emmean, group = 1),
             size = 2.5,
             shape = 18,
             color = "grey20") +
  labs(y = expression(W[4][x][5][min]~(watt)),
       x = "Number of 4x5min session") +
  plot_theme() +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))




# combine plots ##

figure1 <- cowplot::plot_grid(
  design.A,
  cowplot::plot_grid(design.B, design.C,
                     ncol = 2,
                     align = "h",
                     rel_widths = c(2, 1)),
  nrow = 2,
  rel_heights = c(1, 1.5)) +
  draw_plot_label(label = c("A)",  
                            "B)",
                            "C)"),
                  x = c(0.02, 
                        0.02,
                        0.69), 
                  y = c(0.99, 
                        0.59,
                        0.59),
                  hjust = 0.5, 
                  vjust = 0.5, 
                  size = label.size)




# save plot as an RDS object
saveRDS(figure1, "./figures/rds/figure1.RDS")


# Width of figure = 1x columns 8.9 cm
# height of figure = full page = 23 cm


# save plot as PDF
ggsave("figures/figure1.pdf", 
       plot = figure1, 
       width = 12, 
       height = 23 * 0.75, 
       dpi = 600,
       units = "cm", 
       device = cairo_pdf)




