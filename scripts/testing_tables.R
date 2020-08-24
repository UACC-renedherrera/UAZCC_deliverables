library(here)
library(data.table)
library(tidyverse)
library(formattable)


str(df)

str(table_race)

table_race_formatted <- table_race %>%
  ungroup() %>%
  filter(race == "All") %>%
  select(Area = area,
         attribute,
         value) %>%
  spread(key = attribute,
         value = value) %>%
  arrange(Area) 


formattable(table_race_formatted,
    align = c("l", "r", "r", "r", "r"),
    list(`Area` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
         area(col = 2:5) ~ function(x) percent(x, digits = 2),
         area(col = 2:5) ~ color_tile("#efedf5", "#bcbddc")))
    `Hispanic` = color_tile("#efedf5", "#bcbddc"),
    `American Indian` = color_tile("#efedf5", "#bcbddc"),
    `Non-Hispanic Black`= color_tile("#efedf5", "#bcbddc")
  ))
