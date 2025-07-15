# -----------------------------------------------------------------------------
# Title:        Activity Classification and Stress Analysis
# Author:       Fettah Kiran
# Date:         2025-02-26
# Description:  This script visualize activity classification and stress levels 
#               using data visualization techniques in R.
# -----------------------------------------------------------------------------


library(tidyverse) # includes ggplot2 dplyr tidyr readr purrr tibble stringr forcats
library(latex2exp) # parses and converts LaTeX math formulas to R's plotmath expressions
library(ggpubr) # facilitates the creation of beautiful ggplot2-based graphs
library(ggcorrplot) # visualize easily a correlation matrix using ggplot2
require(ggpmisc) # extension to ggplot2
library(cowplot) # extension to ggplot - arranging plots in a grid
library(hms) # pretty time of day

library(grid) # used for adding annotations, such as debriefing text, to the plot

theme_set(theme_classic()) # set the theme to classic
theme_update(plot.title = element_text(hjust = 0.5)) # center the title

rm(list = ls())
dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)
getwd()

# Without x axis label first rows
my_theme <- theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold"),
    strip.text = element_text(size = 10),
    legend.position = "none"
  )

# With  x-y axis label
my_theme1 <- my_theme +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  )


# With  x-y axis label
my_theme2 <- my_theme1 +
  theme(
    axis.text.x = element_text(size = 8, face = "bold"),
    axis.text.y = element_text(size = 8, face = "bold")
  )

mycolors_stress <- c(NS = "green", S = "red")

activity_names <- c(
  `Unknown` = "Unknown",
  `Sleeping` = "Sleeping",
  `Walking` = "Walking",
  `NonWork` = "NonWork",
  `Running` = "Running",
  `Work` = "Work",
  `Biking` = "Biking",
  `Driving` = "Driving"
)

activity_colors <- c(
  Sleeping = "yellow", Unknown = "white",
  NonWork = "#FFC067", Work = "darkgray",
  Walking = "darkolivegreen",
  Running = "deepskyblue", Biking = "orangered",
  Driving = "deeppink3"
)

# Create a dummy data frame for the legend
activity_legend_df <- data.frame(Activity = names(activity_names))

# Generate the legend-only plot
activity_legend_plot <- ggplot(activity_legend_df, aes(x = Activity, fill = Activity)) +
  geom_bar() + # Bar chart to generate legend
  scale_fill_manual(
    # values = activity_colors,  # Remove alpha() from here
    values = alpha(activity_colors, 0.7),
    labels = activity_names,
    breaks = names(activity_names),
    drop = FALSE
  ) +
  theme_void() + # Remove axes
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    legend.key = element_rect(color = "black"), # Add black outline to legend keys
    legend.direction  = "horizontal",
    legend.key.width  = unit(0.9, "cm") 
    # legend.key.width  = unit(0.8, "cm"),         # wider keys
    # legend.spacing.x     = unit(3.2, "cm"),       # space between entries
    # legend.margin        = margin(t = 5, r = 5, b = 5, l = 5)  # outer margin
  ) +
  guides(
    fill = guide_legend(
      nrow = 1, # Single row for legend
      byrow = TRUE, # Fill by row
      label.theme = element_text(margin = margin(l= 8, r = 30, unit = "pt")),  # space after each label
      override.aes = list(alpha = 0.7, color = "black") # Apply transparency in legend
    )
  )

# Print the legend plot
# print(activity_legend_plot)


activity_legend <- ggpubr::get_legend(activity_legend_plot)

# Add black outlines to the legends of all plots
add_legend_outline <- function(plot) {
  plot + guides(
    fill = guide_legend(override.aes = list(color = "black")),
    color = guide_legend(nrow = 1)
  )
}


# Read the data -----------------------------------------------------------

data_dir <- "../data/"
plot_dir <- "../figures/"


file <- "Activity_Stress_data_N24.csv"
df_multimodel <- read.csv(paste0(data_dir, file), stringsAsFactors = FALSE)

## Plot function

### recode activity labels ####
df_multimodel %>%
  mutate(Activity4 = recode_factor(Activity4,
    `Unknown` = "Unknown",
    `Sleeping` = "Sleeping",
    `Walking` = "Walking",
    `Work` = "Work",
    `Biking` = "Biking",
    `NonWork` = "NonWork",
    `Running` = "Running",
    `Driving` = "Driving"
  )) %>%
  droplevels() -> df_multimodel


activity_classification_plots_4CH <- function(P_ID = "T001", isSave = F, file_suffix = "5_min") {
  file_suffix <- "5_min"

  all.df_extented_3_2 <- df_multimodel %>%
    filter(Participant == P_ID & !is.na(Day)) %>%
    filter(!is.na(Activity4))

  all.df_extented_3_2$Date <- as.POSIXct(all.df_extented_3_2$Date, format = "%Y-%m-%d")
  all.df_extented_3_2$Time <- as.POSIXct(all.df_extented_3_2$Time, format = "%Y-%m-%d %H:%M:%S")

  all.df_extented_3_2$Time2 <- format(all.df_extented_3_2$Time, "%H:%M:%S")
  all.df_extented_3_2$Time2 <- as.POSIXct(all.df_extented_3_2$Time2, format = "%H:%M:%S")

  unique(all.df_extented_3_2$Day)

  all.df_extented_3_2$Day <- factor(all.df_extented_3_2$Day, levels = c("WD1", "CD", "WD2", "ND"))

  unique(all.df_extented_3_2$Activity4)

  my.alpha <- 0.7 # transparency level of activities
  day_nrow <- length(unique(all.df_extented_3_2$Day))

  levels(all.df_extented_3_2$Activity4)

  xmax_add <- 5 * 60

  # HR Plot
  ylim.min <- min(all.df_extented_3_2$HR, na.rm = TRUE)
  ylim.max <- max(all.df_extented_3_2$HR, na.rm = TRUE)

  P1_HR <- all.df_extented_3_2 %>%
    ggplot(aes(x = Time2, y = HR)) +
    facet_wrap(Day ~ .,
      scales = "fixed",
      strip.position = "left", nrow = day_nrow, ncol = 1
    ) +
    geom_rect(aes(
      xmin = Time2,
      xmax = Time2 + xmax_add,
      ymin = -Inf, ymax = Inf,
      fill = Activity4
    ), alpha = my.alpha) +
    geom_line(aes(group = subDC), color = "red", linewidth = 0.5, na.rm = T) +
    scale_x_datetime(date_breaks = "4 hour", date_labels = "%H") +
    labs(title = "HR [BPM]", x = "Time [h]", y = "") +
    scale_fill_manual(values = alpha(activity_colors, 1), name = "", 
                      guide = guide_legend(nrow = 1), drop = FALSE) + # legend title = ""
    my_theme1 +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 14) # Bold and larger size
    ) +
    stat_correlation(mapping = use_label(c("n")), label.x = "left")

  # Speed Plot
  ylim.min <- min(all.df_extented_3_2$Speed, na.rm = TRUE)
  ylim.max <- max(all.df_extented_3_2$Speed, na.rm = TRUE)
  ylim.min <- round(ylim.min, 1)

  P2_Speed <-
    all.df_extented_3_2 %>%
    ggplot(aes(x = Time2, y = Speed)) +
    facet_wrap(Day ~ .,
      scales = "free_y",
      nrow = day_nrow, ncol = 1,
      labeller = as_labeller(activity_names)
    ) +
    geom_rect(aes(
      xmin = Time2,
      xmax = Time2 + xmax_add,
      ymin = -Inf, ymax = Inf,
      fill = Activity4
    ), alpha = my.alpha) +
    geom_line(aes(group = subDC), color = "blue", linewidth = 0.5) +
    ylim(ylim.min, ylim.max) +
    scale_x_datetime(date_breaks = "4 hour", date_labels = "%H") +
    labs(title = "Speed [km/h]", y = "", x = "Time [h]") +
    scale_fill_manual(values = alpha(activity_colors, 1), name = "", 
                      guide = guide_legend(nrow = 1), drop = FALSE) + # legend title = ""
    my_theme1 +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.placement = "outside"
    ) +
    stat_correlation(mapping = use_label(c("n")), label.x = "left")

  # Cadence Plot
  ylim.min <- min(all.df_extented_3_2$Cadence, na.rm = TRUE)
  ylim.max <- max(all.df_extented_3_2$Cadence, na.rm = TRUE)

  P3_Cadence <- all.df_extented_3_2 %>%
    ggplot(aes(x = Time2, y = Cadence)) +
    facet_wrap(Day ~ .,
      scales = "free_y",
      nrow = day_nrow, ncol = 1,
      labeller = as_labeller(activity_names)
    ) +
    geom_rect(aes(
      xmin = Time2,
      xmax = Time2 + xmax_add,
      ymin = 0, ymax = Inf,
      fill = Activity4
    ), alpha = my.alpha) +
    geom_line(aes(group = subDC), color = "black", linewidth = 0.5) +
    ylim(ylim.min, ylim.max) +
    scale_fill_manual(values = alpha(activity_colors, 1), name = "", 
                      guide = guide_legend(nrow = 1), drop = FALSE) + # legend title = ""
    scale_color_manual(labels = activity_names) +
    scale_x_datetime(date_breaks = "4 hour", date_labels = "%H") +
    labs(title = "Cadence [CPM]", y = "", x = "Time [h]") +
    my_theme1 +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.placement = "outside"
    ) +
    stat_correlation(mapping = use_label(c("n")), label.x = "left")



  # SNSindex / HRV Plot
  ylim.min <- min(all.df_extented_3_2$SNSindex, na.rm = TRUE)
  ylim.max <- max(all.df_extented_3_2$SNSindex, na.rm = TRUE)

  P4_HRV <- all.df_extented_3_2 %>%
    ggplot(aes(x = Time2, y = SNSindex)) +
    facet_wrap(Day ~ .,
      scales = "free_y",
      nrow = day_nrow, ncol = 1,
      labeller = as_labeller(activity_names)
    ) +
    geom_rect(aes(
      xmin = Time2,
      xmax = Time2 + xmax_add,
      ymin = -Inf, ymax = Inf,
      fill = Activity4
    ), alpha = my.alpha) +
    geom_line(aes(group = subDC), color = "red", linewidth = 0.5) +
    ylim(ylim.min, ylim.max) +
    scale_fill_manual(values = alpha(activity_colors, 1), name = "", 
                      guide = guide_legend(nrow = 1), drop = FALSE) + # legend title = ""
    scale_color_manual(labels = activity_names) +
    scale_x_datetime(date_breaks = "4 hour", date_labels = "%H") +
    labs(title = "SNS Index", y = "", x = "Time [h]") +
    my_theme1 +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.placement = "outside"
    ) +
    stat_correlation(mapping = use_label(c("n")), label.x = "left")

  ## Add black outlines to the legends of all plots
  P1_HR <- add_legend_outline(P1_HR)
  P2_Speed <- add_legend_outline(P2_Speed)
  P3_Cadence <- add_legend_outline(P3_Cadence)
  P4_HRV <- add_legend_outline(P4_HRV)


  figure <- ggarrange(P1_HR, P2_Speed, P3_Cadence, P4_HRV,
    ncol = 4, nrow = 1, common.legend = TRUE, legend = "none"
  )

  figure <- ggarrange(activity_legend, figure,
    ncol = 1, nrow = 2, common.legend = FALSE,
    heights = c(0.1, 1)
  )

  # title of the plot
  title.text <- paste0("Participant ", P_ID)

  final_ann_figure <- annotate_figure(figure,
    top = text_grob(title.text, color = "black", face = "bold", size = 14),
    bottom = guide_legend(nrow = 1)
  )


  # if full path not exist, create the directory
  if (!dir.exists("../figures/Activity4_Visualization_5min")) {
    dir.create("../figures/Activity4_Visualization_5min")
  }

  if (isSave) {
    full_path <- paste0("../figures/Activity4_Visualization_5min/", P_ID, "_4CH_Activity_Classification.pdf")
    # full_path
    ggsave(full_path, final_ann_figure, width = 20, height = 9, units = "in", dpi = 300)
    print(sprintf("%s saved to: %s", P_ID, full_path))
  } else {
    # print(final_ann_figure)
    return(final_ann_figure)
  }
}


#### Save the plots for Activity4 Labeling - activity_classification_plots_4CH

## Selected Subject for the paper
P_ID <- "T005"
activity_classification_plots_4CH(P_ID = P_ID, isSave = T)
P_ID <- "T025"
activity_classification_plots_4CH(P_ID = P_ID, isSave = T)

# All plots in one pdf

# pdf("Activity4_Visualization_5min/Activity_Visualization_5min.pdf", width = 20, height = 9)
# all_ID_list = sprintf("T%03d", c(1:12,14:25)) # T013 dropped
# ## Save plots for given subjects
# for (P_ID in all_ID_list) {
#   print(P_ID)
#   activity_classification_plots_4CH(P_ID = P_ID, file_suffix= "5_min", isSave = F)
# }
#
# dev.off()



### Stress Labeling Plots ---------------------------------------------------

df_sedentMultimodal <- df_multimodel %>%
  filter(Activity4 %in% c("Work", "NonWork", "Driving")) %>%
  droplevels()

df_sedentMultimodal$Activity4 <- factor(df_sedentMultimodal$Activity4, levels = c("Work", "NonWork", "Driving"))


df_sedentMultimodal$Day <- recode_factor(df_sedentMultimodal$Day,
  "WD1" = "WD", "WD2" = "WD",
  "CD" = "CD", "ND" = "ND"
)

df_sedentMultimodal$Day <- factor(df_sedentMultimodal$Day, levels = c("CD", "WD", "ND"))


## Read debriefing data
file <- "Debriefings.csv"
df_debriefing <- read.csv(paste0(data_dir, file), stringsAsFactors = FALSE)

# Function to visualize stress labels
viz_Stress_Label <- function(P_ID = "T001", isSave = FALSE) {
  # Create a complete grid of Participant and Day combinations
  complete_grid <- expand_grid(
    Participant = sprintf("T%03d", 1:25),
    Day = c("CD", "WD", "ND")
  )

  complete_grid$Day <- factor(complete_grid$Day, levels = c("CD", "WD", "ND"))

  # Ensure all combinations are present in the data
  df_sedentMultimodalForm <- complete_grid %>%
    left_join(df_sedentMultimodal, by = c("Participant", "Day"))


  df_sedentMultimodalForm_PID <- df_sedentMultimodalForm %>%
    filter(Participant == P_ID)


  # plot a1
  plot_stress_distr <- df_sedentMultimodalForm_PID %>%
    drop_na(NHR_Stress) %>%
    ggplot(aes(x = HR_Normalized, fill = NHR_Stress)) +
    scale_fill_manual(values = alpha(mycolors_stress, 0.9)) +
    geom_histogram(position = "dodge", binwidth = 1) +
    facet_wrap(~ Participant + Day, ncol = 3) +
    my_theme2 +
    scale_x_continuous(breaks = seq(-10, 40, by = 10)) +
    # ylim(0,20)+ # only for T005
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white", color = "black"), # Set background color
      strip.text = element_text(color = "black") # Set facet text color if needed
    ) +
    labs(x = TeX(r'(\textit{$NHR_{D}$} \[BPM\])'), y = "Frequency") +
    geom_vline(aes(xintercept = NHR_0_2SD), color = "blue", linetype = 3, linewidth = 1)

  # plot a2 - qq plots for HR_Normalized
  plot_stress_qq <- df_sedentMultimodalForm_PID %>%
    drop_na(HR_Normalized) %>%
    ggplot(aes(sample = HR_Normalized)) +
    geom_qq() +
    stat_qq_line(color = "red") +
    my_theme2 +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    facet_wrap(~ Participant + Day, ncol = 3) +
    theme(legend.position = "none") +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white", color = "black"), # Set background color
      strip.text = element_text(color = "black") # Set facet text color if needed
    )

  plot_stress_label <- cowplot::plot_grid(plot_stress_distr, plot_stress_qq,
    label_size = 12,
    labels = c("a1", "a2"), ncol = 1, nrow = 2
  )

  # plot b1
  plot_sns_stress_distr <- df_sedentMultimodalForm_PID %>%
    # if SNS_Stress is NA , then SNSindexThreshold = NA
    mutate(SNSindexThreshold = ifelse(is.na(SNS_Stress), NA, SNSindexThreshold)) %>%
    ggplot(aes(x = SNSindex, fill = SNS_Stress)) +
    scale_fill_manual(values = alpha(mycolors_stress, 0.9)) +
    geom_histogram(position = "dodge", binwidth = 0.1) +
    facet_wrap(~ Participant + Day, ncol = 3) +
    my_theme2 +
    scale_x_continuous(breaks = seq(-10, 20, by = 5)) +
    theme(
      legend.position = "none",
      # strip.background = element_rect(fill = "orange", color = "black"),  # Set background color
      strip.text = element_text(color = "black") # Set facet text color if needed
    ) +
    labs(x = TeX(r'(SNS Index)'), y = "Frequency") +
    geom_vline(aes(xintercept = SNSindexThreshold), color = "blue", linetype = 3, linewidth = 1)

  # plot b2 - qq plots for SNSindex
  plot_sns_stress_qq <- df_sedentMultimodalForm_PID %>%
    ggplot(aes(sample = SNSindex)) +
    geom_qq() +
    stat_qq_line(color = "red") +
    my_theme2 +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
    facet_wrap(~ Participant + Day, ncol = 3) +
    theme(
      legend.position = "none",
      strip.text = element_text(color = "black") # Set facet text color if needed
    )


  plot_sns_stress_label <- cowplot::plot_grid(plot_sns_stress_distr, plot_sns_stress_qq,
    label_size = 12,
    labels = c("b1", "b2"), ncol = 1, nrow = 2
  )


  # Combine a1 and b1
  plot_stress_label <- cowplot::plot_grid(plot_stress_label, plot_sns_stress_label, ncol = 2, nrow = 1)

  # Filter debriefing for the given ID
  df_debriefing_PID <- df_debriefing %>% filter(ID == P_ID)


  if (nrow(df_debriefing_PID) > 0) {
    debriefing_text <- df_debriefing_PID$Debriefing

    # Ensure input is character
    debriefing_text <- as.character(debriefing_text)

    # Wrap the debriefing text
    wrapped_lines <- strwrap(debriefing_text, width = 200)

    # Join lines with LaTeX line breaks
    wrapped_text <- paste(wrapped_lines, collapse = "\n")

    # Create two separate grobs - one for "Debriefing:" and one for the debriefing
    debriefing_grob <- grid::textGrob(
      label = "Debriefing:",
      x = unit(0, "npc"),
      y = unit(1, "npc"),
      just = c("left", "top"),
      gp = gpar(fontsize = 11, fontface = "bold")
    )

    # Create the text grob with proper formatting
    description_grob <- grid::textGrob(
      label = paste("                   ", wrapped_text),
      x = unit(0, "npc"),
      y = unit(1, "npc"),
      just = c("left", "top"),
      gp = gpar(fontsize = 11)
    )

    # Combine the bold "Debriefing:" and the text
    final_grob <- grid::grobTree(debriefing_grob, description_grob)

    plot_stress_label_final <- plot_grid(
      plot_stress_label, # Main plot
      NULL, # Spacer
      ggdraw() + draw_grob(final_grob), # Debriefing text
      ncol = 1, nrow = 3,
      rel_heights = c(3, 0.05, 1) # Adjust heights for spacing
    )
  }




  if (isSave) {
    plot_file <- paste0(plot_dir, "Stress-Label-Plots-", P_ID, ".pdf")
    ggsave(plot_file, plot_stress_label, width = 8.0, height = 4.0, dpi = 300)
  } else {
    return(plot_stress_label_final)
  }
}


# Save for paper
viz_Stress_Label("T005", isSave = TRUE)
viz_Stress_Label("T025", isSave = TRUE)

# All plots in one pdf
# pdf_file = paste0(plot_dir, "Stress-Label-Plots.pdf")
# pdf(pdf_file, width = 8, height = 4)
# P_list = sprintf("T%03d", c(1:12,14:25)) # T013 dropped
# for (Participant in P_list ) {
#   print(Participant)
#   print(viz_Stress_Label(Participant, isSave = FALSE)  )
#
# }
# dev.off()

my_theme2 = my_theme1 # make viz_Stress_Label labels bigger in combined plots

### Combine Activity and Stress label plots in a page per subject
pdf_file <- paste0(plot_dir, "Activity-Stress-Label-Plots.pdf")
pdf(pdf_file, width = 15, height = 13)
P_list <- sprintf("T%03d", c(1:12, 14:25)) # T013 dropped
# P_list = sprintf("T%03d", c(1))
for (Participant in P_list) {
  print(Participant)
  combine_plots <- cowplot::plot_grid(activity_classification_plots_4CH(P_ID = Participant, isSave = FALSE),
    NULL,
    viz_Stress_Label(Participant, isSave = FALSE),
    ncol = 1, nrow = 3,
    scale = c(0.97, .1, .97),
    rel_heights = c(3.5, 0.1, 3.0)
  )

  print(combine_plots)
}
dev.off()

