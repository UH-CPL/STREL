
library(tidyverse) # includes ggplot2 dplyr tidyr readr purrr tibble stringr forcats
library(latex2exp) # parses and converts LaTeX math formulas to R's plotmath expressions
library(ggpubr) # facilitates the creation of beautiful ggplot2-based graphs
library(arm)  # convenience functions for regression in R
library(jtools) # efficient presentation of regression analysis, e.g., summ() function
library(ggcorrplot) # visualize easily a correlation matrix using ggplot2
require(ggpmisc) # extension to ggplot2
library(cowplot) # extension to ggplot - arranging plots in a grid
library(hms) # pretty time of day



# Without x axis label first rows
my_theme <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 10),
    legend.position = "none"
  )

# With  x-y axis label
my_theme1 <- my_theme +
  theme(
    axis.text.x = element_text(size = 8, face = "bold")
  )

mycolors_stress = c(NS = "green", S ="red")

n_fun <- function(x) {
  return(data.frame(y = median(x) * 1.3, label = paste0("~italic(n)", " == ", length(x))))
}

activity_names <- c(
  `sleeping` = "Sleeping",
  `office_home` = "Office Work",
  `walking` = "Walking",
  `running` = "Running",
  `biking` = "Biking",
  `driving` = "Driving"
)

my_theme3 =   theme_bw() +theme(
  panel.grid = element_blank(),
  plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(size = 10, face = "bold"),
  axis.text.y = element_text(size = 10, face = "bold"),
  legend.position = "none"
)



# Create custum significance legend for model plots
levels <- c("A", "S1", "B", "C")
num <- c(5, 10, 15, 20)
ymin <- c(0, 0, 0, 0)
ymax <- c(1, 2, 3, 4)

Legend_DF <- data.frame(levels, num, ymin, ymax)

Legend_Plot <- ggplot(Legend_DF, aes(x = levels, y = num, colour = levels)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), size = 1.1) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.key.width = unit(1.7, "cm"),
    legend.text = element_text(size = 20)
  ) +
  theme(
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_color_manual(
    values = c("black", "cyan", "#ee9a00", "red"),
    breaks = c("A", "S1", "B", "C"),
    labels = c("NS     ", "*     ", "**     ", "***")
  )

mylegend <- ggpubr::get_legend(Legend_Plot)
