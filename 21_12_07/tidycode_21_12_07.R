current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(current_path)

# clear environment 
rm(list=ls())
gc()

# load libraries
library(afrilearndata)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
#devtools::install_github("ricardo-bion/ggradar")
library(ggradar)
library(ggtext)


# fonts
library(showtext)
font_add(family = "Blackwidow", regular = "Blackwidow-o6ga.ttf") # https://www.fontspace.com/blackwidow-font-f23155
font_add(family = "Montserrat", regular = "Montserrat-Regular.ttf") # https://fonts.google.com/specimen/Montserrat?category=Sans+Serif
showtext_auto()


# import/get data
tuesdata <- tidytuesdayR::tt_load('2021-12-07')

spiders <- as.data.frame(tuesdata$spiders)
rm(tuesdata)

spiders[ , 1:ncol(spiders)] <- lapply(spiders[ , 1:ncol(spiders)], as.factor)


# south_american countries
south_america <- c("Argentina",
                   "Bolivia",
                   "Brazil",
                   "Chile",
                   "Colombia",
                   "Ecuador",
                   "Guyana",
                   "Paraguay",
                   "Peru",
                   "Suriname",
                   "Uruguay",
                   "Venezuela")


# african countries
data(africountries)

africa <- africountries$name_long

africa <- gsub("Central African Republic", "Central African Rep.", africa)
africa <- gsub("Côte d'Ivoire" , "Ivory Coast", africa)
africa <- gsub("Democratic Republic of the Congo" , "DR Congo", africa)
africa <- gsub("Republic of Congo" , "Congo", africa)
africa <- gsub("The Gambia" , "Gambia", africa)

rm(africountries)


# split data
spiders2  <-  separate(
  data = spiders, 
  col = "distribution", 
  into = c(paste("d", 1:15, sep = "")), 
  sep = ",", 
  extra = "warn", 
  fill = "warn")


# spiders per continent per country
data_list_africa <- vector(mode = "list")

for(i in 1:length(africa)) {
  
  current <- africa[i]
  
  data <- subset(spiders2, d1 == current | d2 == current | d3 == current | d4 == current | d5 == current | 
                   d6 == current | d7 == current | d8 == current | d9 == current | d10 == current | 
                   d11 == current | d12 == current | d13 == current | d14 == current | d15 == current)
  
  data <- droplevels(data)
  
  data_list_africa[[i]] <- data
  
}


data_list_south_america <- vector(mode = "list")

for(i in 1:length(south_america)) {
  
  current <- south_america[i]
  
  data <- subset(spiders2, d1 == current | d2 == current | d3 == current | d4 == current | d5 == current | 
                   d6 == current | d7 == current | d8 == current | d9 == current | d10 == current | 
                   d11 == current | d12 == current | d13 == current | d14 == current | d15 == current)
  
  data <- droplevels(data)
  
  data_list_south_america[[i]] <- data
  
}


## combine lists of countries per continent
spiders_africa <- ldply(data_list_africa, rbind)
spiders_africa = spiders_africa[!duplicated(spiders_africa$speciesId),]


spiders_south_america <- ldply(data_list_south_america, rbind)
spiders_south_america = spiders_south_america[!duplicated(spiders_south_america$speciesId),]


## get family size per continent
data_africa <- spiders_africa %>% 
               group_by(family) %>%
               summarise(no_rows = length(family))

data_south_america <- spiders_south_america %>% 
                      group_by(family) %>%
                      summarise(no_rows = length(family))


## shorten data
data_africa_s <- data_africa[order(-data_africa$no_rows),]
data_africa_s <- data_africa_s[1:10,]
data_africa_s <- data_africa_s[order(data_africa_s$family),]

data_south_america_s <- data_south_america[order(-data_south_america$no_rows),]
data_south_america_s <- data_south_america_s[1:10,]
data_south_america_s <- data_south_america_s[order(data_south_america_s$family),]


## combine data
data_africa_st <- data.frame(t(data_africa_s))
colnames(data_africa_st) <- c(t(data_africa_s[,1]))
data_africa_st <- data_africa_st[2,]

data_south_america_st <- data.frame(t(data_south_america_s))
colnames(data_south_america_st) <- c(t(data_south_america_s[,1]))
data_south_america_st <- data_south_america_st[2,]


data_all <- rbind.fill(data_africa_st, data_south_america_st)

data_all[is.na(data_all)] <- 0

data_all <- data_all[ , order(names(data_all))]

data_all$continent <- c("Africa", "South America")

data_all[ , 1:14] <- lapply(data_all[ , 1:ncol(data_all)], as.numeric)

data_all <- data_all %>%
  select(continent, everything())

# visualisation
ggradar(
  data_all, 
  values.radar = c("0", "525", "1050"),
  grid.min = 0, grid.mid = 525, grid.max = 1100,
  group.line.width = 2, 
  group.point.size = 4,
  grid.line.width = 2.5,
  gridline.min.colour = "#24292A",
  gridline.mid.colour = "#24292A",
  gridline.max.colour = "#24292A",
  axis.line.colour = "#24292A",
  label.gridline.min = FALSE,
  label.gridline.mid = FALSE,
  label.gridline.max = FALSE,
  gridline.min.linetype = "solid",
  gridline.mid.linetype = "solid",
  gridline.max.linetype = "solid",
  plot.legend = FALSE,
  font.radar = "Montserrat",
  group.colours = c("#8A423B", "#7CA16D")
)+
  labs(title = "we are family",
       subtitle = "Most common spider families across <span style='color:#8A423B;'>Africa</span> and <span style='color:#7CA16D;'>South America</span>",
       caption = "Visualisation: Dominic Schmitz @dmncschmtz\nData: World Spider Database via tidytuesdayR") +
  theme(
    plot.background = element_rect(fill = "#f5f5ea", color = "#f5f5ea"),
    panel.background = element_rect(fill = "#f5f5ea", color = "#f5f5ea"),
    plot.title = element_text(
      family = "Blackwidow", 
      size = 140,
      face = "bold", 
      color = "#24292A",
      hjust = 0.5,
      margin = margin(t = 20, r = 0, b = 15, l = 0)
    ),
    plot.subtitle = element_markdown(size = 26, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5),
    plot.margin= unit(c(0.5,0.5,0.5,0.5),"cm")
  )

ggsave(filename = "spiders.png", dpi = 100, width = 34, height = 26, units = 'cm')


