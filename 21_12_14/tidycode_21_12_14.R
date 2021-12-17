current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(current_path)


# clear environment 
rm(list=ls())
gc()


# load libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(cowplot)


# fonts
library(showtext)
font_add(family = "BakbakOne", regular = "BakbakOne-Regular.ttf") # https://fonts.google.com/specimen/Bakbak+One?category=Display
font_add(family = "Inconsolata", regular = "Inconsolata-Regular.ttf") # https://fonts.google.com/specimen/Inconsolata?category=Monospace
showtext_auto()


# import/get data
tuesdata <- tidytuesdayR::tt_load('2021-12-14')


# work on feature data
data_raw <- tuesdata$studio_album_tracks

data_raw$album_id <- as.factor(data_raw$album_id)

energy <- mean(data_raw$energy)
speechiness <- mean(data_raw$speechiness)
acousticness <- mean(data_raw$acousticness)
instrumentalness <- mean(data_raw$instrumentalness)
valence <- mean(data_raw$valence)

energy2 <- sd(data_raw$energy)
speechiness2 <- sd(data_raw$speechiness)
acousticness2 <- sd(data_raw$acousticness)
instrumentalness2 <- sd(data_raw$instrumentalness)
valence2 <- sd(data_raw$valence)

data_vis <- as.data.frame(rbind(energy, speechiness, acousticness, instrumentalness, valence))
data_vis$category <- as.factor(rownames(data_vis))
data_vis$sd <- rbind(energy2, speechiness2, acousticness2, instrumentalness2, valence2)

data_vis <- data_vis %>% 
  rename(
    value = `V1`
  )

# work on lyrics data
data2 <- tuesdata$lyrics

Scary <- sum(str_count(data2$section_artist, "Scary"))
Posh <- sum(str_count(data2$section_artist, "Posh"))
Ginger <- sum(str_count(data2$section_artist, "Ginger"))
Baby <- sum(str_count(data2$section_artist, "Baby"))
Sporty <- sum(str_count(data2$section_artist, "Sporty"))

data_vis2 <- data.frame(rbind(Scary, Posh, Ginger, Baby, Sporty))
data_vis2$who <- rownames(data_vis2)

data_vis2 <- data_vis2 %>% 
  rename(
    value = `rbind.Scary..Posh..Ginger..Baby..Sporty.`
  )

data_vis2$fraction = data_vis2$value / sum(data_vis2$value)

data_vis2$ymax = cumsum(data_vis2$fraction)

data_vis2$ymin = c(0, head(data_vis2$ymax, n=-1))

# visualisation

title <- ggplot()+
  labs(title = "So tell me what you want, what you really, really want",
       subtitle = "All the ingredients you need for a great Spice Girls song") +
  theme_bw(base_family = "BakbakOne") +
  theme(plot.title = element_text(size = 36, color = "#c6cacb", margin = margin(t = 5, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(size = 26, color = "#c6cacb", margin = margin(t = 0, r = 0, b = 10, l = 0)),
        plot.background = element_rect(fill = "#5e5f5f", color = "#5e5f5f"),
        panel.background = element_rect(fill = "#5e5f5f", colour = "#5e5f5f", size = 0.5),
        panel.border = element_rect(colour = "#5e5f5f", fill=NA),
        plot.margin= unit(c(1,0.5,1,0.5),"cm"))


part1 <- ggplot(data_vis2) +
  geom_rect(aes(ymax=ymax, ymin=ymin, xmax=5, xmin=3), fill="#0d9aa2") +
  geom_point(aes(x=1, y=1, group=who), size = 30, color = "#262626") +
  geom_rect(aes(ymax=ymax, ymin=ymax+0.03, xmax=4, xmin=1), fill="#cccccc") +
  coord_polar(theta="y") +
  xlim(c(1, 5)) +
  facet_grid(. ~ who) +
  theme_void(base_family = "Inconsolata") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#5e5f5f", color = "#5e5f5f"),
        panel.grid.major.x = element_line(size = 1.5, colour = "#5e5f5f"),
        plot.margin= unit(c(0.5,0.5,0.5,0.5), "cm"),
        panel.spacing = unit(1.75, "cm"),
        strip.text.x = element_text(size = 20)
  )


part2 <- ggplot() +
  geom_bar(data = data_vis, aes(x=category, y=value+sd), stat = "identity", position = position_dodge(), fill = "#fefb01", width = 0.1) +
  geom_bar(data = data_vis, aes(x=category, y=value), stat = "identity", position = position_dodge(), fill = "#1eff07", width = 0.1) +
  geom_point(data = data_vis, aes(x=category, y=value), size = 16, shape = 15, color = "#5a5a5b") +
  geom_point(data = data_vis, aes(x=category, y=value), size = 14, shape = 15, color = "#727374") +
  geom_errorbar(data = data_vis, aes(x = category, ymin = value, ymax = value), width = 0.24, size = 4, color = "#5a5a5b") +
  geom_errorbar(data = data_vis, aes(x = category, ymin = value, ymax = value), width = 0.2, size = 2, color = "#acacac") +
  scale_y_continuous(limit = c(-0.05, 1.025), expand = c(0,0), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  facet_grid(. ~ category, scales = "free") +
  xlab("") +
  ylab("") +
  theme_bw(base_family = "Inconsolata") +
  theme(axis.text = element_text(size = 16, color = "#c6cacb"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#5e5f5f", color = "#5e5f5f"),
        panel.background = element_rect(fill = "#030203", colour = "#030203", size = 0.5),
        panel.grid.major.x = element_line(size = 1.5, colour = "#585858"),
        panel.grid.major.y = element_line(size = 1, colour = "#2b2b2b"),
        panel.grid.minor.y = element_line(size = 1, colour = "#2b2b2b"),
        panel.border = element_blank(),
        plot.caption = element_text(color = "#c6cacb", size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.margin= unit(c(0.5,0.5,0.5,0.5), "cm"),
        strip.background =element_rect(fill = "#898989"),
        panel.spacing = unit(0.5, "cm"),
        strip.text.x = element_text(size = 20)
  ) +
  labs(caption = "Visualisation: Dominic Schmitz @dmncschmtz
       Data: Spotify & Genius @tidytuesdayR")


spice <- plot_grid(title, part1, part2, nrow=3, rel_heights = c(0.75, 1.5, 3)) +
  theme(plot.background = element_rect(fill="#5e5f5f", color = NA))

ggsave(filename = "spice.png", spice, dpi = 100, width = 34, height = 20, units = 'cm')






