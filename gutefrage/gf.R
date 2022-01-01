current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(current_path)


# clear environment 
rm(list=ls())
gc()


# load libraries
library(ggplot2)
library(elementalist)
library(ggrepel)

# fonts
library(sysfonts)
library(showtext)
font_add_google("Roboto")
font_add_google("Roboto Slab")
showtext_auto()
library(extrafont)
loadfonts(device = "win")


# import/get data
gf2 <- read.csv("gutefrage2.csv")


# wrangle data
gf2$month <- ordered(gf2$month, levels = c("Januar", "Februar", "März", "April", "Mai", "Juni", 
                                           "Juli", "August", "September", "Oktober", "November", "Dezember"))

gf2$category <- ordered(gf2$category, levels = c("pos_Bew", "Beitraege", "gelesen"))

levels(gf2$category) <- c("positive Bewertungen erhalten", "Beiträge verfasst", "Views meiner Beiträge")


# create highlight df
highlights <- data.frame(
  a <- c("August", "April", "Juni"),
  b <- c(1043, 549, 27454),
  b <- c("positive Bewertungen erhalten", "Beiträge verfasst", "Views meiner Beiträge")
)

names(highlights)[1] <- "month"
names(highlights)[2] <- "value"
names(highlights)[3] <- "category"

rm(a, b)

# visualise
ggplot(gf2) +
  geom_line(aes(x = month, y = value, group = 1), color = "#222223") +
  geom_segment(mapping = aes(x = month, xend = month, y = 0, yend = value), color = "#656768") +
  geom_point(mapping = aes(x = month, y = value), size = 3, color = "#222223") +
  geom_point(data = highlights, mapping = aes(x = month, y = value), size = 5, color = "#4aa0cc") +
  geom_label_repel(data = highlights,
                   aes(x = month, y = value, label = value),
                   nudge_x = -0.55,
                   nudge_y = 0,
                   na.rm = TRUE,
                   color = "#222223",
                   fill = "#eeeeef",
                   size=4,
                   family="Roboto Slab") +
  coord_flip() +
  facet_grid(. ~ category, scales = "free") +
  theme_bw(base_family = "Roboto") +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 16, color = "#222223"),
        plot.background = element_rect(fill = "#f3f3f3"),
        panel.background = element_rect_round(fill = "#ffffff", colour = "#ffffff", size = 0.5, radius = unit(4, "pt")),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.5, colour = "#eeeeef"),
        panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid', colour = "#eeeeef"),
        panel.border = element_rect_round(color = "#cccccc", radius = unit(4, "pt")),
        strip.background = element_rect_round(fill="#4aa0cc", color = "#4aa0cc", radius = unit(4, "pt")),
        strip.text = element_text(size = 18, family = "Roboto Slab", color = "#fffeff"),
        plot.title = element_text(size = 32, color = "#222223", family = "Roboto Slab", margin = margin(t = 4, r = 0, b = 8, l = 0), hjust = -0.2),
        plot.subtitle = element_text(size = 18, color = "#222223", family = "Roboto Slab", margin = margin(t = 0, r = 0, b = 16, l = 0), hjust = 1.55),
        plot.caption = element_text(color = "#222223", size = 12, family = "Roboto Slab", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        plot.margin= unit(c(0.5,0.5,0.5,0.5),"cm")) +
  labs(title = "Mein Jahr 2021 auf gutefrage.net",
       subtitle = "Daten aus meinem monatlichen Report als »Community-Experte« für Sprache, Gendern, Gender, LGBT & LGBTQ",
       caption = "Visualisation: Dominic Schmitz @dmncschmtz \nData Source: gutefrage.net @gutefrage_net")

ggsave(filename = "gf.png", dpi = 100, width = 34, height = 20, units = 'cm')
