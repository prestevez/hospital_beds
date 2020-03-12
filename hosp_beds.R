library(tidyverse)
library(magrittr)
library(ggthemes)

wb <- read_csv("HFA_478_EN.csv")

wb %>%
  group_by(COUNTRY) %>%
  filter(YEAR == max(YEAR)) %>%
  select(-COUNTRY_GRP, -SEX) %>% summary

who <- read_csv("data_who.csv")

who %>%
  group_by(Country) %>%
  filter(Year == max(Year)) %>%
  ggplot(aes(Country, `Hospital beds (per 10 000 population)`)) +
  geom_bar(stat = "identity")

oecd <- read_csv("DP_LIVE_12032020141002678.csv")

oecd %>%
  group_by(LOCATION) %>%
  filter(TIME == max(TIME)) %>%
  select(-`Flag Codes`, - INDICATOR, -SUBJECT, - MEASURE, - FREQUENCY) -> oecd_latest

oecd_latest %>%
  summary

iso_codes <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

iso_codes %>%
  select(Country = name, LOCATION = `alpha-3`, region, sub_r = `sub-region`) %>%
  right_join(oecd_latest) -> oecd_latest_names

oecd_latest_names %>%
  summarise(Value = mean(Value)) %>%
  transmute(Country = "OECD average", 
            LOCATION = "OECD", 
            region = "OECD",
            sub_r = "OECD",
            TIME = 2019,
            Value = Value) %>%
  bind_rows(oecd_latest_names) -> oecd_latest_avg

oecd_latest_names$Country[c(16,29,30)] <- c("Korea", "UK", "USA")

oecd_latest_names %>%
  mutate(grp = ifelse(Country %in% c("Mexico", "Italy"), Country, "OECD")) %>%
  mutate(Country = fct_reorder(Country, Value)) %>%
  ggplot(aes(Country, Value, fill = grp)) + 
  geom_bar(stat = "identity") -> oecd_p

oecd_p +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.35), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  scale_y_continuous(breaks = c(1,3,5,7,9,11,13,15)) +
  geom_hline(yintercept = mean(oecd_latest_names$Value), linetype = 2) +
  ggtitle("Camas hospitaliaras por 1,000 habitantes, OECD") +
  labs(caption = "Cifras más recientes disponibles en https://data.oecd.org/healtheqt/hospital-beds.htm
       CC-BY @prestevez") +
  scale_fill_colorblind() +
  annotate("text", x = 5, y = 5.2, label = "Promedio OECD")
  
  oecd_latest_names %>%
    filter(Country %in% c("Mexico", "Italy"))

3.18/1.38
