# set up
# packages 

library(here)
library(tidyverse)

# read data 

uazcc_attribute <- read_csv("data/raw/uazcc_characterization.csv",
                            col_names = TRUE,
                            col_types = cols(
                              attribute = col_character(),
                              area = col_factor(levels = c("USA", 
                                                           "AZ", 
                                                           "Catchment", 
                                                           "Cochise", 
                                                           "Pima", 
                                                           "Pinal", 
                                                           "Santa Cruz", 
                                                           "Yuma"), 
                                                ordered = TRUE),
                              race = col_factor(levels = c("All",
                                                           "Non-Hispanic White",
                                                           "Hispanic or Latino",
                                                           "American Indian",
                                                           "Non-Hispanic Black"),
                                                ordered = TRUE),
                              value = col_number(),
                              year = col_character(),
                              source = col_character(),
                              note = col_character()
                            ),
                            na = c("", "NA")
                            )

# prepare for UAZCC COE Characteristics, no race 

uazcc_attribute_table <- uazcc_attribute %>% 
  filter(race == "All") %>%
  select(attribute, area, value) %>%
  spread(key = area, value = value)


# prepare for UAZCC COE Characteristics, race only

uazcc_attribute_table_race <- uazcc_attribute %>% 
  filter(race != "All") %>%
  select(attribute, race, value) %>%
  spread(key = race, value = value)

# combine 
uazcc_attribute_table <- full_join(uazcc_attribute_table, uazcc_attribute_table_race)

# write to csv
write_csv(uazcc_attribute_table, "data/tidy/uazcc_characterization_attribute_table.csv")
