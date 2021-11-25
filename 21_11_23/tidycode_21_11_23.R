current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(current_path)

# clear environment 
rm(list=ls())
gc()

# load libraries
library(tidyverse)
library(rvest)
library(ggplot2)
library(ggtext)

# fonts
library(showtext)

font_add(family = "doctor who", regular = "Doctor-Who.ttf") # https://www.dafont.com/doctor-who.font
font_add(family = "DJB Sunflowers for Vincent", regular = "DJB Sunflowers for Vincent.ttf") # https://www.wfonts.com/font/djb-sunflowers-for-vincent
showtext_auto()

# import/get data
tuesdata <- tidytuesdayR::tt_load('2021-11-23')

season <- 1

get_imdb <- function(season){
  url <- glue::glue("https://www.imdb.com/title/tt0436992/episodes?season={season}")
  
  raw_html <- read_html(url)
  
  raw_div <- raw_html %>% 
    html_elements("div.list.detail.eplist") %>% 
    html_elements("div.info")
  
  ep_num <- raw_div %>% 
    html_elements("meta") %>% 
    html_attr("content")
  
  air_date <- raw_div %>% 
    html_elements("div.airdate") %>% 
    html_text() %>% 
    str_squish()
  
  ratings <- raw_div %>% 
    html_elements("div.ipl-rating-star.small > span.ipl-rating-star__rating") %>% 
    html_text()
  rate_ct <- raw_div %>% 
    html_elements("div.ipl-rating-star.small > span.ipl-rating-star__total-votes")%>% 
    html_text() %>% 
    str_remove_all("\\(|\\)|,")
  
  descrip <- raw_div %>% 
    html_elements("div.item_description") %>% 
    html_text() %>% 
    str_squish()
  
  tibble(
    season = season,
    ep_num = ep_num,
    air_date = air_date,
    rating = ratings, rating_n = rate_ct, desc = descrip)
  
}

all_season <- 1:12 %>% 
  map_dfr(get_imdb)

clean_season <- all_season %>% 
  type_convert()

## wrangle data

# first episodes
first <- clean_season %>% group_by(season) %>% slice_min(ep_num)
first$type <- c(rep("first", length(rownames(first))))

# last episodes
last <- clean_season %>% group_by(season) %>% slice_max(ep_num)
last$type <- c(rep("last", length(rownames(last))))

# combine
data <- as.data.frame(rbind(first, last))

# convert
data$season <- as.factor(data$season)

## visualisation

# define some colours
stripes <- c("#D33681", "#D33681", "#D33681", "#D33681", "#D33681", "#b5cc9b", 
             "#D33681", "#b5cc9b", "#D33681", "#D33681", "#b5cc9b", "#b5cc9b")


# visualise
p <- ggplot(data = data) +
  geom_line(aes(x=rating, y=season, group = season), size = 3, color = "#A6B8C7") +
  geom_point(aes(x=rating, y=season, color = type), size = 9) +
  geom_text(aes(x=rating, y=season, label=rating),
            hjust=0.5,
            vjust=0.5,
            color="#003B6F",
            family="DJB Sunflowers for Vincent",
            size=3.5) +
  scale_x_continuous(limits=c(5.5, 9.5)) +
  scale_color_manual(values=c("#b5cc9b", "#D33681")) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text = element_text(family="DJB Sunflowers for Vincent", size = 14, color = "#d3dce3"),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#003B6F"),
        panel.background = element_rect(fill = "#003B6F", colour = "#003B6F", size = 0.5),
        panel.grid.major.x = element_line(size = 0.5, linetype = 'dotted', colour = stripes), 
        panel.grid.major.y = element_line(size = 0.5, linetype = 'solid', colour = "#003B6F"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#003B6F"),
        plot.title = element_text(family="doctor who", size = 45, color = "#d3dce3"),
        plot.subtitle = element_markdown(family="DJB Sunflowers for Vincent", size = 19, color = "#A6B8C7"),
        plot.caption = element_text(family="DJB Sunflowers for Vincent", color = "#466885", size = 10, hjust = 0.5, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.margin= unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(title = "Doctor Who - Mystery of the First and Final Episodes",
       subtitle = "In 8 out of 12 seasons, the <b style='color:#D33681;'>final</b> episode is rated higher than the <b style='color:#b5cc9b;'>first</b> episode.",
       caption = "Visualisation: Dominic Schmitz @dmncschmtz l Data Source: Jonathan Kitt, datardis package @tidytuesdayR")

ggsave(p, filename = "doctorwho.png", dpi = 100, type = 'cairo', width = 34, height = 20, units = 'cm')


