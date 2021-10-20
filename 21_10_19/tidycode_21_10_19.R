# clear environment 
rm(list=ls())
gc()

# load libraries
library(tidytuesdayR)
library(ggplot2)
library(Cairo)
library(ggrepel)

# import data
tuesdata <- tidytuesdayR::tt_load('2021-10-19')

# choose data set
data <- tuesdata$pumpkins

# some data wrangling I
split <- data.frame(do.call('rbind', strsplit(as.character(data$id),'-',fixed=TRUE)))

data$year <- split$X1
data$type <- split$X2

data$year <- as.numeric(data$year)
data$type <- as.factor(data$type)

# data cleaning
data$weight_lbs <- gsub(",", "", as.character(data$weight_lbs))
data$weight_lbs <- as.numeric(data$weight_lbs)

data <- data[complete.cases(data[ , 3]),]

# some data wrangling II
data_vis <- aggregate(data[, 3], list(data$year, data$type), mean)

data_vis <- data_vis %>%
  rename(
    year = Group.1,
    type = Group.2,
    weight = weight_lbs
  )

data_vis$type <- factor(data_vis$type, levels=c("T", "F", "L", "W", "S", "P"))

### create vis data

data_T <- subset(data, type == "T")
data_T <- aggregate(data_T[, 3], list(data_T$year), mean)

T_2013 <- 0
T_2014 <- 100 / data_T[1,2] * data_T[2,2] - 100
T_2015 <- 100 / data_T[2,2] * data_T[3,2] - 100
T_2016 <- 100 / data_T[3,2] * data_T[4,2] - 100
T_2017 <- 100 / data_T[4,2] * data_T[5,2] - 100
T_2018 <- 100 / data_T[5,2] * data_T[6,2] - 100
T_2019 <- 100 / data_T[6,2] * data_T[7,2] - 100
T_2020 <- 100 / data_T[7,2] * data_T[8,2] - 100
T_2013 <- 100 / data_T[8,2] * data_T[9,2] - 100

T_values <- data.frame(rbind(T_2013,T_2014,T_2015,T_2016,T_2017,T_2018,T_2019,T_2020,T_2013))
T_values$year <- data_T$Group.1
T_values$mean_weight <- data_T$weight_lbs * 0.453592
names(T_values)[1] <- "percent"

##

data_F <- subset(data, type == "F")
data_F <- aggregate(data_F[, 3], list(data_F$year), mean)

F_2013 <- 0
F_2014 <- 100 / data_F[1,2] * data_F[2,2] - 100
F_2015 <- 100 / data_F[2,2] * data_F[3,2] - 100
F_2016 <- 100 / data_F[3,2] * data_F[4,2] - 100
F_2017 <- 100 / data_F[4,2] * data_F[5,2] - 100
F_2018 <- 100 / data_F[5,2] * data_F[6,2] - 100
F_2019 <- 100 / data_F[6,2] * data_F[7,2] - 100
F_2020 <- 100 / data_F[7,2] * data_F[8,2] - 100
F_2013 <- 100 / data_F[8,2] * data_F[9,2] - 100

F_values <- data.frame(rbind(F_2013,F_2014,F_2015,F_2016,F_2017,F_2018,F_2019,F_2020,F_2013))
F_values$year <- data_F$Group.1
F_values$mean_weight <- data_F$weight_lbs * 0.453592
names(F_values)[1] <- "percent"

##

data_L <- subset(data, type == "L")
data_L <- aggregate(data_L[, 3], list(data_L$year), mean)

L_2013 <- 0
L_2014 <- 100 / data_L[1,2] * data_L[2,2] - 100
L_2015 <- 100 / data_L[2,2] * data_L[3,2] - 100
L_2016 <- 100 / data_L[3,2] * data_L[4,2] - 100
L_2017 <- 100 / data_L[4,2] * data_L[5,2] - 100
L_2018 <- 100 / data_L[5,2] * data_L[6,2] - 100
L_2019 <- 100 / data_L[6,2] * data_L[7,2] - 100
L_2020 <- 100 / data_L[7,2] * data_L[8,2] - 100
L_2013 <- 100 / data_L[8,2] * data_L[9,2] - 100

L_values <- data.frame(rbind(L_2013,L_2014,L_2015,L_2016,L_2017,L_2018,L_2019,L_2020,L_2013))
L_values$year <- data_L$Group.1
L_values$mean_weight <- data_L$weight_lbs * 2.54
names(L_values)[1] <- "percent"

##

data_W <- subset(data, type == "W")
data_W <- aggregate(data_W[, 3], list(data_W$year), mean)

W_2013 <- 0
W_2014 <- 100 / data_W[1,2] * data_W[2,2] - 100
W_2015 <- 100 / data_W[2,2] * data_W[3,2] - 100
W_2016 <- 100 / data_W[3,2] * data_W[4,2] - 100
W_2017 <- 100 / data_W[4,2] * data_W[5,2] - 100
W_2018 <- 100 / data_W[5,2] * data_W[6,2] - 100
W_2019 <- 100 / data_W[6,2] * data_W[7,2] - 100
W_2020 <- 100 / data_W[7,2] * data_W[8,2] - 100
W_2013 <- 100 / data_W[8,2] * data_W[9,2] - 100

W_values <- data.frame(rbind(W_2013,W_2014,W_2015,W_2016,W_2017,W_2018,W_2019,W_2020,W_2013))
W_values$year <- data_W$Group.1
W_values$mean_weight <- data_W$weight_lbs * 0.453592
names(W_values)[1] <- "percent"

##

data_S <- subset(data, type == "S")
data_S <- aggregate(data_S[, 3], list(data_S$year), mean)

S_2013 <- 0
S_2014 <- 100 / data_S[1,2] * data_S[2,2] - 100
S_2015 <- 100 / data_S[2,2] * data_S[3,2] - 100
S_2016 <- 100 / data_S[3,2] * data_S[4,2] - 100
S_2017 <- 100 / data_S[4,2] * data_S[5,2] - 100
S_2018 <- 100 / data_S[5,2] * data_S[6,2] - 100
S_2019 <- 100 / data_S[6,2] * data_S[7,2] - 100
S_2020 <- 100 / data_S[7,2] * data_S[8,2] - 100
S_2013 <- 100 / data_S[8,2] * data_S[9,2] - 100

S_values <- data.frame(rbind(S_2013,S_2014,S_2015,S_2016,S_2017,S_2018,S_2019,S_2020,S_2013))
S_values$year <- data_S$Group.1
S_values$mean_weight <- data_S$weight_lbs * 0.453592
names(S_values)[1] <- "percent"

##

data_P <- subset(data, type == "P")
data_P <- aggregate(data_P[, 3], list(data_P$year), mean)

P_2013 <- 0
P_2014 <- 100 / data_P[1,2] * data_P[2,2] - 100
P_2015 <- 100 / data_P[2,2] * data_P[3,2] - 100
P_2016 <- 100 / data_P[3,2] * data_P[4,2] - 100
P_2017 <- 100 / data_P[4,2] * data_P[5,2] - 100
P_2018 <- 100 / data_P[5,2] * data_P[6,2] - 100
P_2019 <- 100 / data_P[6,2] * data_P[7,2] - 100
P_2020 <- 100 / data_P[7,2] * data_P[8,2] - 100
P_2013 <- 100 / data_P[8,2] * data_P[9,2] - 100

P_values <- data.frame(rbind(P_2013,P_2014,P_2015,P_2016,P_2017,P_2018,P_2019,P_2020,P_2013))
P_values$year <- data_P$Group.1
P_values$mean_weight <- data_P$weight_lbs * 0.453592
names(P_values)[1] <- "percent"

data_vis <- rbind(T_values, F_values, L_values, W_values, S_values, P_values)

data_vis$type <- c(rep("T", 9),rep("F", 9),rep("L", 9),rep("W", 9),rep("S", 9),rep("P", 9))

data_vis$year <- as.factor(data_vis$year)

## create label data

data_vis$label2 <- NA
data_vis$label2[data_vis$mean_weight == max(data_vis$mean_weight[data_vis$type=="T"])] <- paste(round(max(data_vis$mean_weight[data_vis$type=="T"]), 2), "kg")
data_vis$label2[data_vis$mean_weight == max(data_vis$mean_weight[data_vis$type=="F"])] <- paste(round(max(data_vis$mean_weight[data_vis$type=="F"]), 2), "kg")
data_vis$label2[data_vis$mean_weight == max(data_vis$mean_weight[data_vis$type=="L"])] <- paste(round(max(data_vis$mean_weight[data_vis$type=="L"]), 2), "cm")
data_vis$label2[data_vis$mean_weight == max(data_vis$mean_weight[data_vis$type=="W"])] <- paste(round(max(data_vis$mean_weight[data_vis$type=="W"]), 2), "kg")
data_vis$label2[data_vis$mean_weight == max(data_vis$mean_weight[data_vis$type=="S"])] <- paste(round(max(data_vis$mean_weight[data_vis$type=="S"]), 2), "kg")
data_vis$label2[data_vis$mean_weight == max(data_vis$mean_weight[data_vis$type=="P"])] <- paste(round(max(data_vis$mean_weight[data_vis$type=="P"]), 2), "kg")

# re-level data
data_vis$type <- ordered(data_vis$type, levels = c("T", "P", "F", "L", "W", "S"))

levels(data_vis$type) <- c("Tomato", "Field Pumpkin", "Giant Pumpkin", "Long Gourd", "Giant Watermelon", "Giant Squash")  

# load font Book Antiqua
loadfonts(device = "win")

finplot <- ggplot()+
  theme_bw() +
  geom_hline(yintercept = 0, size = 1, linetype = "dashed", color = "#8a7070")+
  geom_label_repel(data = data_vis,
                   aes(x = year, y = percent, label = label2),
                   nudge_x = -0.25,
                   nudge_y = 15,
                   na.rm = TRUE,
                   color = "#d2aaaa",
                   fill = "#272121",
                   size=4,
                   family="Book Antiqua") +
  geom_line(data = data_vis, aes(x = year, y = percent, color = type, group = type), size=3) +
  scale_color_manual(values = c("#bd493a", "#eb7f25", "#c38367", "#576254", "#638759", "#bacd67")) +
  facet_wrap(. ~ type) +
  scale_x_discrete(breaks = c("2013", "2015", "2017", "2019", "2021")) +
  scale_y_continuous(limits=c(-20, 50), labels = function(x) paste0(x, "%")) +
  
  theme(
    text = element_text(family="Book Antiqua"),
    plot.background = element_rect(fill = "#272121"),
    panel.background = element_rect(fill = "#272121", colour = "#272121", size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#272121"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#272121"),
    axis.text = element_text(color="#f4c5c5", size=12),
    axis.title = element_text(color="#f4c5c5", size=14),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 12)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 12, l = 0)),
    legend.position = "none",
    strip.background = element_rect(colour="#d2aaaa",
                                    fill="#bb9797"),
    strip.text = element_text(color="#272121", face = "bold", size=14),
    plot.title = element_text(size=19, color ="#f4c5c5"),
    plot.subtitle = element_text(size=14, color ="#f4c5c5", margin=margin(0,0,20,0)),
    plot.caption = element_text(size=10, color ="#8a7070"),
    plot.margin= unit(c(1,1,0.8,0.8),"cm")) +
  xlab("Year") +
  ylab("Mean Weight/Length Increase in %") +
  labs(title = "Increase in Pumpkin Average Weight/Length from 2013 - 2021",
       subtitle = "Years of highest relative increase in weight/length are not necessarily years of highest overall mean weight/length",
       caption = "Dominic Schmitz @dmncschmtz | Data Source: Great Pumpkin Commonwealth @ tidytuesdayR")

ggsave(finplot, filename = 'tidytuesday_pumpkins.png', dpi = 100, type = 'cairo', width = 32, height = 17, units = 'cm')

### Please note that depending on the size (e.g. width & height), adjustments to alignments of labels have to be made
### The current alignments work for the above specified dimensions (32x17 cm)
