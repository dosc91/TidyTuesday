# clear environment 
rm(list=ls())
gc()

# load libraries
library(tidytuesdayR)
library(ggplot2)
library(ggimage)
library(png)
library(Cairo)

# import data
tuesdata <- tidytuesdayR::tt_load('2021-10-12')

# choose data set
data <- tuesdata$`capture-fisheries-vs-aquaculture`

# Europe only
data_eu <- subset(data, Entity == "Austria" | Entity == "Belgium" | Entity == "Bulgaria" | Entity == "Croatia" | Entity == "Cyprus" | 
                    Entity == "Czechia" | Entity == "Denmark" | Entity == "Estonia" | Entity == "Finland" | Entity == "France" | 
                    Entity == "Germany" | Entity == "Greece" | Entity == "Hungary" | Entity == "Ireland" | Entity == "Italy" | 
                    Entity == "Latvia" | Entity == "Lithuania" | Entity == "Luxembourg" | Entity == "Malta" | Entity == "Netherlands" | 
                    Entity == "Poland" | Entity == "Portugal" | Entity == "Romania" | Entity == "Slovakia" | Entity == "Slovenia" | 
                    Entity == "Spain" | Entity == "Sweden")

# some cleaning
data_eu$Aquaculture <- data_eu$`Aquaculture production (metric tons)`
data_eu$Capture <- data_eu$`Capture fisheries production (metric tons)`

# find top 3 producers
data_eu$Combined <- data_eu$Aquaculture + data_eu$Capture
data_com <- subset(data_eu, Year == 2018)
data_com <- data_com[order(data_com$Combined, decreasing = T),]

head(data_com, 3)
# Spain
# Denmark
# France

# create data set for visualisation

data_vis <- subset(data_eu, Entity == "Spain" | Entity == "France" | Entity == "Denmark")
data_vis <- data.frame(cbind(data_vis$Entity, data_vis$Year, data_vis$Aquaculture, data_vis$Capture, data_vis$Combined))

names(data_vis)[names(data_vis) == "X1"] <- "Entity"
names(data_vis)[names(data_vis) == "X2"] <- "Year"
names(data_vis)[names(data_vis) == "X3"] <- "Aquaculture"
names(data_vis)[names(data_vis) == "X4"] <- "Capture"
names(data_vis)[names(data_vis) == "X5"] <- "Combined"

denmark <- data_vis[data_vis$Entity=="Denmark",]
catch_denmark <- data.frame(c(denmark$Aquaculture, denmark$Capture))
catch_denmark$type <- c(rep("Aquaculture", 59), rep("Capture", 59))
catch_denmark$year <- c(1960:2018, 1960:2018)
names(catch_denmark)[names(catch_denmark) == "c.denmark.Aquaculture..denmark.Capture."] <- "amount"

france <- data_vis[data_vis$Entity=="France",]
catch_france <- data.frame(c(france$Aquaculture, france$Capture))
catch_france$type <- c(rep("Aquaculture", 59), rep("Capture", 59))
catch_france$year <- c(1960:2018, 1960:2018)
names(catch_france)[names(catch_france) == "c.france.Aquaculture..france.Capture."] <- "amount"

spain <- data_vis[data_vis$Entity=="Spain",]
catch_spain <- data.frame(c(spain$Aquaculture, spain$Capture))
catch_spain$type <- c(rep("Aquaculture", 59), rep("Capture", 59))
catch_spain$year <- c(1960:2018, 1960:2018)
names(catch_spain)[names(catch_spain) == "c.spain.Aquaculture..spain.Capture."] <- "amount"

data_vis4 <- data.frame(rbind(catch_denmark, catch_france, catch_spain))

data_vis4$country <- c(rep("Denmark", 118),rep("France", 118),rep("Spain", 118))

data_vis4$country <- as.factor(data_vis4$country)
data_vis4$type <- as.factor(data_vis4$type)
data_vis4$amount <- as.numeric(data_vis4$amount)

# background image path
path <- "bg.png" # this is available on github

# create the ggplot object
plot <- ggplot()+
  theme_minimal()+
  geom_smooth(data = data_vis4, aes(x=year, y=amount, color=type, linetype=country), se=F, size=1.5)+
  scale_colour_manual(values=c(Aquaculture="#b4c43c", Capture="#b74532"))+
  scale_x_continuous(breaks=c(1960, 1970, 1980, 1990, 2000, 2010, 2018), expand = c(0, 1))+
  scale_y_continuous(expand = c(0.01, 0.01))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#043c46"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#043c46"),
    axis.text = element_text(color="white", size=15),
    axis.title = element_text(color="white", size=17),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 12, l = 0)),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    #legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "vertical",
    #legend.justification = "right",
    legend.position = c(0.835, 1.15),
    legend.margin = margin(unit(c(6,6,-50,6),"cm")),
    plot.margin= unit(c(5.5,1,0,0.8),"cm"),
    plot.title = element_text(size=17, color ="#043c46", vjust = 50, hjust = -1.75),
    plot.subtitle = element_text(size=12, color ="#043c46", vjust = 68, hjust = -11),
    plot.caption = element_text(size=10, color ="#436d74", vjust = 8),
    legend.spacing.x = unit(0.5, 'cm')
  )+
  xlab("Year")+
  ylab("Production in Metric Tons")+
  labs(linetype = "Country", color="Production Type",
       title = "Aquaculture vs. Fishery Production in Top 3 European Countries Ranked by Overall Production",
       subtitle = "While the world now produces more seafood from aquaculture (fish farming) than from wild catch, this is not true for the top 3 European countries",
       caption = "Data Source: OurWorldinData.org @ tidytuesdayR | Image Source: BSGStudio @ all-free-download.com")+
  scale_linetype_manual(values=c("twodash", "dotted", "dashed"))

# set background image to plot
finplot <- ggbackground(plot, path)

## view plot
# CairoWin()
# finplot

# save the plot
ggsave(finplot, filename = 'tidytuesday_seafood.png', dpi = 100, type = 'cairo', width = 32, height = 17, units = 'cm')

### Please note that depending on the size (e.g. width & height), adjustments to alignments of legend, title, subtitle, etc., have to be made
### The current alignments work for the above specified dimensions (32x17 cm)

