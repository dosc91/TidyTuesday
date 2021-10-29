current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(current_path)

# clear environment 
rm(list=ls())
gc()

# load libraries
library(tidytuesdayR)
library(dplyr)
library(lubridate)
library(extrafont)
library(ggplot2)
library(hrbrthemes)
library(ggtext)

# import data
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# join data sets
data <- ultra_rankings %>% left_join(race, by="race_year_id")

# some data wrangling I
data$year <- year(data$date)

data$gender <- as.factor(data$gender)
data$year <- as.factor(data$year)

df <- as.data.frame(table(data$year, data$gender))

df <- df %>% 
  rename(
    year = Var1,
    gender = Var2,
    number = Freq
  )

df_m <- subset(df, gender == "M")

df_m <- df_m %>% 
  rename(
    number_m = number
  )


df_w <- subset(df, gender == "W")

df_w <- df_w %>% 
  rename(
    number_w = number
  )

data_vis <- df_m %>% left_join(df_w, by="year")

# visualisaton

loadfonts()

p <- ggplot(data_vis) +
  geom_segment(aes(x=year, xend=year, y=number_m, yend=number_w), color="#716c61", size=1.5) +
  geom_point(aes(x=year, y=number_m), color="#af8dc3", size=6) +
  geom_point(aes(x=year, y=number_w), color="#7fbf7b", size=6) +
  geom_text(aes(x=year, y=number_m, label=number_m),
            hjust=0.4,
            vjust=-1.5,
            color="#716c61",
            family="Gisha",
            size=3.5) +
  geom_text(aes(x=year, y=number_w, label=number_w),
            hjust=0.5,
            vjust=2.2,
            color="#716c61",
            family="Gisha",
            size=3.5) +
  scale_y_continuous(limits=c(0, 21000)) +
  coord_flip() +
  theme_ipsum() +
  theme(
    text = element_text(family="Gisha"), 
    legend.position = "none",
    plot.background = element_rect(fill = "#fff3db"),
    panel.background = element_rect(fill = "#fff3db", colour = "#fff3db", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#fff3db"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#fff3db"),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(family="Gisha", size = 14),
    plot.subtitle = element_markdown(family="Gisha", size=10),
    plot.caption = element_text(family="Gisha")) +
  labs(title = "Ultra Trail Runners from 2012 to 2021",
       subtitle = "the number of ultra trail runners steadily increased until the onset of covid-19  
                  overall, more <span style='color:#af8dc3;'>**men**</span> than 
                  <span style='color:#7fbf7b;'>**women**</span>
                  engage in ultra trail running ",
       caption = "Visualisation: Dominic Schmitz @dmncschmtz \nData Source: Benjamin Nowak, International Trail Running Association (ITRA) @tidytuesdayR")


ggsave(p, filename = "ultratrailrunning.png", dpi = 100, type = 'cairo', width = 20, height = 17, units = 'cm')

### Please note that depending on the size (e.g. width & height), adjustments to alignments of labels have to be made
### The current alignments work for the above specified dimensions (32x17 cm)
