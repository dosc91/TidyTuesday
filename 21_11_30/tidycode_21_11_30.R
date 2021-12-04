current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
rm(current_path)


# clear environment 
rm(list=ls())
gc()


# load libraries
library(dplyr)
library(ggplot2)
library(ggtext)


# fonts
library(showtext)
font_add(family = "dosis", regular = "Dosis-Regular.ttf", bold = "Dosis-Bold.ttf") # https://fonts.google.com/specimen/Dosis?query=dosis
showtext_auto()


# import/get data
tuesdata <- tidytuesdayR::tt_load('2021-11-30')

matches <- tuesdata$matches


# wrangle data --- sorry for the mess! can't be bothered right now to clean it up
matches$team1 <- as.factor(matches$team1)
matches$team2 <- as.factor(matches$team2)
matches$winner <- as.factor(matches$winner)
matches$ground_country <- as.factor(matches$ground_country)

levels(matches$ground_country)[levels(matches$ground_country)=="Midlothian"] <- "Scotland"

countries <- c(
  "Australia",
  "Bangladesh",
  "Canada",
  "England",
  "India",
  "Kenya",
  "Netherlands",
  "New Zealand",
  "Pakistan",
  "Scotland",
  "South Africa",
  "Sri Lanka",
  "United Arabic Emirates",
  "West Indies",
  "Zimbabwe"
)

data <- data.frame(table(matches$toss, matches$toss_decision, matches$winner))

data <- subset(data, Freq != 0)

data$win_or_not <- ifelse(as.character(data$Var1) == as.character(data$Var3), "win", "no win")

data <- data %>% 
  dplyr::rename(
    toss = Var1,
    toss_decision = Var2,
    winner = Var3
  )

data <- aggregate(data[, 4], list(data$toss_decision, data$winner), sum)

data <- data %>%
  dplyr::group_by(Group.2) %>%
  dplyr::filter(dplyr::n()>1)

data <- data %>% 
  dplyr::rename(
    toss_decision = Group.1,
    winner = Group.2,
    frequency = x
  )

data$toss_decision <- ordered(data$toss_decision, levels = c("field first", "bat first"))

bat <- subset(data, toss_decision == "bat first")

bat <- bat %>% 
  dplyr::rename(
    freq_bat = frequency
  )


field <- subset(data, toss_decision == "field first")

field <- field %>% 
  dplyr::rename(
    freq_field = frequency
  )

df <- bat

df$freq_field <- field$freq_field

df$freq_dif <- (df$freq_field + df$freq_bat) / df$freq_bat

df <- df[order(df$freq_dif),]

order <- c(as.character(df$winner))

data$winner <- ordered(data$winner, levels = order)

n <- aggregate(data[, 3], list(data$winner), sum)
n <- rbind(n, n)
n <- n %>%
  dplyr::rename(
    winner = Group.1,
    n = frequency
  )

data_vis <- merge(data, n, by="winner")

data_vis <- data_vis[!duplicated(data_vis), ]

data_vis$props <- data_vis$frequency / data_vis$n

data_vis <- subset(data_vis, winner != "Match tied")

all_bats_freq <- sum(data_vis$frequency[data_vis$toss_decision == "bat first"])

all_fields_freq <- sum(data_vis$frequency[data_vis$toss_decision == "field first"])

all_n <- all_bats_freq + all_fields_freq

o_dat <- data.frame(cbind(c(rep("Overall", 2)), c("bat first", "field first"), c(all_bats_freq, all_fields_freq), c(rep(all_n, 2))))

o_dat$X3 <- as.numeric(o_dat$X3)

o_dat$X4 <- as.numeric(o_dat$X4)

o_dat <- o_dat %>%
  dplyr::rename(
    winner = X1,
    toss_decision = X2,
    frequency = X3,
    n = X4
  )

o_dat$props <- o_dat$frequency / o_dat$n

data_vis <- rbind(data_vis, o_dat)


# visualisation

ggplot(data = data_vis) +
  geom_bar(data = data_vis, aes(x=winner, y=frequency, fill=toss_decision), stat="identity", position="fill", width=0.75) +
  geom_hline(yintercept = 0.5, linetype="solid", color = "#AED7D2", size=1.5, alpha=0.35) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0), labels = scales::percent, breaks = c(0.25, 0.5, 0.75)) +
  scale_fill_manual(values = c("#94C16D", "#E0A060")) +
  theme_bw(base_family = "dosis") +
  theme(axis.text = element_text(size = 16, color = "#30383d"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#efefda"),
        panel.background = element_rect(fill = "#efefda", colour = "#efefda", size = 0.5),
        panel.grid.major.x = element_line(size = 1.5, colour = "#AED7D2"),
        panel.grid.minor.x = element_line(size = 0.5, linetype = 'solid', colour = "#efefda"),
        panel.border = element_blank(),
        plot.title = element_markdown(size = 32, color = "#30383d", margin = margin(t = 5, r = 0, b = 5, l = -70)),
        plot.subtitle = element_markdown(size = 24, color = "#30383d", margin = margin(t = 0, r = 0, b = 10, l = -70)),
        plot.caption = element_text(color = "#30383d", size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.margin= unit(c(0.5,0.5,0.5,0.5),"cm")
  ) +
  labs(title = "<b style='color:#E0A060;'>Bat</b> or <b style='color:#94C16D;'>Field</b> first?",
       subtitle = "all teams but New Zealand win proportionally more often if the toss winners choose <b style='color:#E0A060;'>bat</b> first, not <b style='color:#94C16D;'>field</b> first",
       caption = "Visualisation: Dominic Schmitz @dmncschmtz
       Data: ESPN Cricinfo by way of Hassanasir @tidytuesdayR")


ggsave(filename = "cricket.png", dpi = 100, width = 34, height = 20, units = 'cm')


