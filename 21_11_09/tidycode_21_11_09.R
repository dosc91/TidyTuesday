current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(current_path)

# clear environment 
rm(list=ls())
gc()

# afrilearndata
# remotes::install_github("afrimapr/afrilearndata")

# load libraries
library(extrafont)
library(ggplot2)
library(sf)
library(afrilearndata)
library(Cairo)

# fonts
library(sysfonts)
font_add_google("Noto Serif")


# import data
data(africountries)

# language data
languages <- c(5, 3, 39, 4, 2, 2,
               1, 8, 5, 3, 2, 2,
               1, 8, 7, 1, 11, 1,
               22, 1, 3, 2, 1, 3,
               2, 4, 2, 4, 5, 5,
               9, 12, 12, 4, 4, 0,
               2, 60, 12, 2, 3, 4,
               2, 2, 3, 1, 2, 2,
               11, 1, 16)

# join data
africountries$languages <- languages

# visualisaton

# background colour
background <- "#123F69"

ggplot(data = africountries, aes(fill = log(languages))) +
  geom_sf(color = background, size = 0.5) +
  geom_sf_text(aes(label = languages), 
               color = background,
               size = 3.5,
               family="Noto Serif") +
  theme_void(base_family = "Noto Serif") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = background),
        panel.background = element_rect(fill = background, colour = background, size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = background), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = background),
        plot.margin= unit(c(0.5,0.5,0,0.5),"cm"),
        plot.title = element_text(colour = "#ffffcc", size = 16),
        plot.subtitle = element_text(colour = "#ffffcc", size = 14),
        plot.caption = element_text(colour = "#ffffcc", size = 7, vjust = 3, hjust = 1.05)) +
  scale_fill_gradient(
    low = "#ffffcc",
    high = "#006837",
    space = "Lab",
    na.value = background,
    guide = "colourbar",
    aesthetics = "fill"
  ) +
  labs(title = "Languages in Africa",
       subtitle = "Sum of Official & National Languages per Country",
       caption = "Visualisation: Dominic Schmitz @dmncschmtz | Data Source: @afrimapr via @tidytuesdayR")

ggsave(filename = "afrimapr.png", dpi = 100, type = 'cairo', width = 17.8, height = 20, units = 'cm')

### Please note that depending on the size (e.g. width & height), adjustments to alignments of labels have to be made
### The current alignments work for the above specified dimensions (17.8x20 cm)
