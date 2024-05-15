## figure source file
##
## this file contains "meta-code" for the creation of graphs



# Libraries for figures

library(tidyverse)
library(cowplot)
library(ggtext)
library(ggh4x)
library(magick)


male.color <- "#d95f02"
female.color   <- "#7570b3"



# line sizes
line_size <- 0.3
error.size <- 0.15

# text sizes
label.size <- 10
text.size <- 8



# customized theme for plotting 

plot_theme <- function() {
  
  theme_bw() +
    theme(panel.grid = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(size = line_size), 
          axis.ticks = element_line(size = line_size), 
          axis.text = element_text(color = "black", size = 7), 
          axis.title = element_text(color = "black", size = 7),
          legend.title = element_blank(), 
          legend.background = element_rect(fill = "white"),
          legend.margin = margin(t = 0, r = 1, b = 1, l = 1, unit = "pt"),
          legend.key = element_rect(fill = "white", color = "white"),
          legend.position = c(0.85, 0.9)) 
  
  
}
