library(tidyverse)
library(latex2exp)
library(ggpubr)
library(arm)  # convenience functions for regression in R display()
library(jtools) # summ() function
library(ggcorrplot)
library(dplyr)
require(ggpmisc) # stat_correlation

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

my_theme3 =   theme_bw() +theme(
  panel.grid = element_blank(),
  plot.title = element_text(hjust = 0.5),
  axis.text.x = element_text(size = 10, face = "bold"),
  axis.text.y = element_text(size = 10, face = "bold"),
  legend.position = "none"
)

# Add black outlines to the legends of all plots

add_legend_outline <- function(plot) {
  plot + guides(fill = guide_legend(override.aes = list(color = "black")))
}


options(digits=4)

### Read the data

file = "../data/All_Curated_Demog_Activity_Sleep5_min_N24.csv"
all.df2 = read.csv(file, stringsAsFactors = T)
names(all.df2)

all.df2$Time <- as.POSIXct(all.df2$Time, format = "%Y-%m-%d %H:%M:%S")


unique(all.df2$Day_label)


file_IP_label = "../data/All_Curated_Demog_Activity_Sleep5_min_N24-Manually-Labeled-FINAL_Activity4.csv"
tmp = read.csv(file_IP_label, stringsAsFactors = T)
names(tmp)

# In excel yyyy-m-d h:mm:ss

tmp2 = tmp %>%
  dplyr::select(ID, Time, Day_label, Activity4, RR) 
head(tmp2)

unique(tmp2$Activity4)


# Format Time column in  the excel as 
# yyyy-m-d h:mm:ss
# # Convert to date-time object 10/26/23 18:20
#tmp2$Time  <- strptime(tmp2$Time , format = "%m/%d/%y %H:%M")
# # Format to the desired output
#tmp2$Time <- format(tmp2$Time, "%Y-%m-%d %H:%M:%S")

tmp2$Time <- as.POSIXct(tmp2$Time, format = "%Y-%m-%d %H:%M:%S")

#rm(all.df2_tmp)

all.df2 = merge(all.df2, tmp2, 
                by.x = c("ID", "Time","Day_label"), 
                by.y = c("ID", "Time","Day_label"), 
                all.x = T)


all.df2 <- all.df2 %>%
  filter(!is.na(Day_label)) %>%
  filter(!is.na(Time))


#Remove T0017 Day_label = PR and Date = 2024-03-29

all.df2 <- all.df2 %>%
  filter(!(ID == "T017" & subDC == "DC1.3")) 


unique(all.df2$Activity4)

unique(all.df2$Day_label)

## Activity4  Visualization of the data with selected days

#### Activity4 function

activity_names <- c(
  `Sleeping` = "Sleeping",
  `NonWork` = "NonWork",
  `Work` = "Work",
  `Walking` = "Walking",
  `Running` = "Running",
  `Biking` = "Biking",
  `Driving` = "Driving"
)

mycolors <- c(
  Sleeping = "yellow", Unknown = "white", 
  NonWork = "#FFC067", Work = "darkgray",
  Walking = "darkolivegreen",
  Running = "deepskyblue", Biking = "orangered",
  Driving = "deeppink3"
)

## Plot function
### Visualization of the data with selected days ####

all.df_5min_new = all.df2

activity4_classification_plots_4CH <- function(P_ID, isSave = F, file_suffix = "") {
  
  file_suffix <- "5_min"
  all.df_extented_3_2 <- all.df_5min_new %>%
    filter(ID == P_ID & !is.na(Day_label)) %>%
    filter(!is.na(Activity4))
  
  all.df_extented_3_2$Date <- as.POSIXct(all.df_extented_3_2$Date, format = "%Y-%m-%d")
  all.df_extented_3_2$Time <- as.POSIXct(all.df_extented_3_2$Time, format =  "%Y-%m-%d %H:%M:%S")
  
  all.df_extented_3_2$Time2 <- format(all.df_extented_3_2$Time, "%H:%M:%S")
  all.df_extented_3_2$Time2 <- as.POSIXct(all.df_extented_3_2$Time2, format = "%H:%M:%S")
  
  unique(all.df_extented_3_2$Day_label)
  # remove the last digit
  all.df_extented_3_2$Day_label <- gsub("\\d", "", all.df_extented_3_2$Day_label)
  unique(all.df_extented_3_2$Day_label)
  
  all.df_extented_3_2$Day_label <- factor(all.df_extented_3_2$Day_label, levels = c("PR", "MD", "PS", "RD"))
  #all.df_extented_3_2$Day_label <- factor(all.df_extented_3_2$Day_label, levels = c("WD", "CD", "WD", "OD"))
  
  #recode Day_labels from PR, MD, PS, RD to WD, CD, WD, OD
  all.df_extented_3_2$Day_label <- recode_factor(all.df_extented_3_2$Day_label,
                                                 `PR` = "WD1",
                                                 `MD` = "CD",
                                                 `PS` = "WD2",
                                                 `RD` = "OD"
  )
  levels(all.df_extented_3_2$Day_label)
  
  unique(all.df_extented_3_2$Day_label)
  
  unique(all.df_extented_3_2$Activity4)
  
  # remove unknown activity and refactor
  all.df_extented_3_2 %>%
    mutate(Activity4 = recode_factor(Activity4,
                                     `unknown` = "Unknown",
                                     `sleeping` = "Sleeping", 
                                     `NonWork` = "NonWork",
                                     `Work` = "Work",
                                     `walking` = "Walking",
                                     `running` = "Running",
                                     `biking` = "Biking",
                                     `driving` = "Driving"
    )) %>% droplevels() -> all.df_extented_3_2
  
  unique(all.df_extented_3_2$Activity4)
  
  levels(all.df_extented_3_2$Activity4)
  
  my.alpha <- 0.7
  day_nrow <- length(unique(all.df_extented_3_2$Day_label))
  
  # Create a data frame with 2-hour intervals for 24 hours for each day
  levels(all.df_extented_3_2$Activity4)
  
  if (file_suffix == "1_min") {
    xmax_add= 60 
  } else if (file_suffix == "5_min") {
    xmax_add = 5*60
  } else {
    xmax_add = 1
  }
  
  # HR Plot
  ylim.min <- min(all.df_extented_3_2$HR, na.rm = TRUE)
  ylim.max <- max(all.df_extented_3_2$HR, na.rm = TRUE)
  
  P1_HR <- all.df_extented_3_2 %>%
    ggplot(aes(x = Time2, y = HR)) +
    facet_wrap(Day_label ~ .,
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
    scale_x_datetime(date_breaks = "2 hour", date_labels = "%H") +
    labs(title = "HR [BPM]", x = "Time [h]", y = "") +
    scale_fill_manual(values = alpha(mycolors, 1), "", drop = FALSE) + # legend title = ""
    my_theme1 +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 14) # Bold and larger size
    ) +
    stat_correlation(mapping = use_label(c("n")), label.x = "left")
  
  # Speed_NR Plot
  ylim.min <- min(all.df_extented_3_2$Speed_NR, na.rm = TRUE)
  ylim.max <- max(all.df_extented_3_2$Speed_NR, na.rm = TRUE)
  
  ylim.min <- round(ylim.min, 1)
  
  P2_Speed <-
    all.df_extented_3_2 %>%
    ggplot(aes(x = Time2, y = Speed_NR)) +
    facet_wrap(Day_label ~ .,
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
    scale_x_datetime(date_breaks = "2 hour", date_labels = "%H") +
    labs(title = "Speed [km/h]", y = "", x = "Time [h]") +
    scale_fill_manual(values = alpha(mycolors, 1)) +
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
    facet_wrap(Day_label ~ .,
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
    scale_fill_manual(values = alpha(mycolors, 1)) +
    scale_color_manual(labels = activity_names) +
    scale_x_datetime(date_breaks = "2 hour", date_labels = "%H") +
    labs(title = "Cadence [RPM]", y = "", x = "Time [h]") +
    my_theme1 +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      strip.placement = "outside"
    ) +
    stat_correlation(mapping = use_label(c("n")), label.x = "left")
  
  
  
  # HRV Plot
  ylim.min <- min(all.df_extented_3_2$RR, na.rm = TRUE)
  ylim.max <- max(all.df_extented_3_2$RR, na.rm = TRUE)
  
  P4_HRV <- all.df_extented_3_2 %>%
    ggplot(aes(x = Time2, y = RR)) +
    facet_wrap(Day_label ~ .,
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
    geom_line(aes(group = subDC), color = "black", linewidth = 0.5) +
    #ylim(ylim.min, ylim.max) +
    scale_fill_manual(values = alpha(mycolors, 1)) +
    scale_color_manual(labels = activity_names) +
    scale_x_datetime(date_breaks = "2 hour", date_labels = "%H") +
    labs(title = "HRV [ms]", y = "", x = "Time [h]") +
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
                      ncol = 4, nrow = 1, common.legend = TRUE, legend = "right"
  )
  
  
  
  
  title.text <- paste0("Activity Classification of ", P_ID, " | ",file_suffix," \n")
  
  final_ann_figure <- annotate_figure(figure,
                                      top = text_grob(title.text, color = "red", face = "bold", size = 14)
  )
  
  getwd()
  full_path <- paste0("Activity4_Visualization_5min/4CH_Activity4_Classification_", P_ID, "_sleep_",file_suffix,".jpeg")
  full_path
  
  #if full path not exist, create the directory
  if (!dir.exists("Activity4_Visualization_5min")) {
    dir.create("Activity4_Visualization_5min")
  }
  
  if (isSave) {
    ggsave(full_path,
           final_ann_figure,
           width = 20, height = 9, units = "in", dpi = 300
    )
    cat("Figure saved as: ", full_path)
  } else {
    print(final_ann_figure)
  }
}




#### Save the plots for Activity4 Labeling - activity4_classification_plots_4CH
# Save the plots - CHOOSE
file_suffix = "5_min"
#P_ID = "T022"

#activity4_classification_plots_4CH(P_ID = P_ID, file_suffix, isSave = T)


all_ID_list = sprintf("T%03d", c(1:12,14:25)) # T013 dropped

## Save plots for given subjects
for (P_ID in all_ID_list) {
  print(P_ID)
  activity4_classification_plots_4CH(P_ID = P_ID, file_suffix= "5_min", isSave = T)
}






