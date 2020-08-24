#### set up ####
# packages
library(here)
library(tidyverse)
library(ggthemes)

# color palette
blues_8 <- c("#f7fbff",
             "#deebf7",
             "#c6dbef",
             "#9ecae1",
             "#6baed6",
             "#4292c6",
             "#2171b5",
             "#084594")

blues_3 <- c("#deebf7",
             "#9ecae1",
             "#3182bd")

mixed_8 <- c("#deebf7",
             "#9ecae1",
             "#3182bd",
             "#edf8e9",
             "#bae4b3",
             "#74c476",
             "#31a354",
             "#006d2c")

#### read data ####
df <- read_rds("data/tidy/data_for_visualizations.rds")

distinct(df, category)
str(df)

#### race ####
table_race <- df %>%
  filter(category == "Race") %>%
  group_by(area)

table_race$attribute <- as.factor(table_race$attribute)

table_race$attribute <- ordered(table_race$attribute, levels = c("Non-Hispanic White", "Hispanic", "American Indian", "Non-Hispanic Black"))

table_race %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value, fill = attribute)) +
  geom_bar(stat = "identity", position = "stack", color = "black", alpha = 0.666) +
  labs(
    title = "Proportion of each Race and Ethnicity Category in Catchment Geographies",
    subtitle = "",
    y = "",
    x = "",
    caption = "Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("demographics_race.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

#### demographics ####
# age ----
table_demographics <- df %>%
  filter(category == "Demographics",
         area != "Catchment")

unique(table_demographics$area)
table_demographics$area <- ordered(table_demographics$area, levels = c("USA", "AZ", "Cochise", "Pima", "Pinal", "Santa Cruz", "Yuma"))

unique(table_demographics$attribute)

table_demographics_age <- table_demographics %>% 
  filter(attribute == "Median Age")

unique(table_demographics_age$attribute)

table_demographics_age$attribute <- as.factor(table_demographics_age$attribute)

table_demographics_age %>%
  filter(race == "All") %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  geom_label(aes(label = value)) +
  labs(
    title = "Median Age in Catchment Geographies",
    subtitle = "",
    y = "",
    x = "",
    caption = "Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("demographics_age.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

# sex ----
table_demographics <- df %>%
  filter(category == "Demographics")

unique(table_demographics$attribute)

table_demographics_sex <- table_demographics %>% 
  filter(attribute == "Female")

unique(table_demographics_sex$attribute)

table_demographics_sex$attribute <- as.factor(table_demographics_sex$attribute)

table_demographics_sex %>%
  filter(race == "All") %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value)) +
  geom_bar(stat = "identity", fill = "#0C234B") +
  geom_label(aes(label = round(value, digits = 2))) +
  labs(
    title = "Female Proportion in Catchment Geographies",
    subtitle = "",
    y = "",
    x = "",
    caption = "Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("demographics_sex.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

# education attainment ----
table_demographics <- df %>%
  filter(category == "Demographics")

unique(table_demographics$attribute)

table_demographics_edu <- table_demographics %>% 
  filter(attribute == "High School Graduation" | attribute == "Some College" | attribute == "College Graduate")

unique(table_demographics_edu$attribute)

table_demographics_edu$attribute <- as.factor(table_demographics_edu$attribute)

table_demographics_edu$attribute <- ordered(table_demographics_edu$attribute, levels = c("High School Graduation", "Some College", "College Graduate"))

table_demographics_edu %>%
  filter(race == "All") %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value, fill = attribute)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.666) +
  labs(
    title = "Proportion of Educational Attainment in Catchment Geographies",
    subtitle = "Proportion of population with high school diploma or college",
    y = "",
    x = "",
    caption = "Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("demographics_edu.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

# income ----
table_demographics <- df %>%
  filter(category == "Demographics")

table_demographics$area <- ordered(table_demographics$area, levels = c("USA", "AZ", "Catchment", "Cochise", "Pima", "Pinal", "Santa Cruz", "Yuma"))

unique(table_demographics$area)
unique(table_demographics$attribute)

table_demographics_income <- table_demographics %>% 
  filter(attribute == "Median Family income" | attribute == "Mean family income")

unique(table_demographics_income$attribute)

table_demographics_income$attribute <- as.factor(table_demographics_income$attribute)

table_demographics_income %>%
  filter(race == "All") %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "#0C234B") +
  geom_label(aes(label = value)) +
  facet_wrap("attribute") +
  labs(
    title = "Mean and Median Income in Catchment Geographies",
    subtitle = "in US dollars",
    y = "",
    x = "",
    caption = "Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("demographics_income.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

# poverty ----
table_demographics <- df %>%
  filter(category == "Demographics")

unique(table_demographics$attribute)

table_demographics_poverty <- table_demographics %>% 
  filter(attribute == "Food Insecurity" | attribute == "Households Below Poverty Level" | attribute == "Unemployment" | attribute == "Uninsured")

unique(table_demographics_poverty$attribute)

table_demographics_poverty$attribute <- as.factor(table_demographics_poverty$attribute)

table_demographics_poverty %>%
  filter(race == "All") %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value, fill = attribute)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = .666) +
  labs(
    title = "Indicators of Poverty in Catchment Geographies",
    subtitle = "",
    y = "",
    x = "",
    caption = "Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates;
    U.S. Bureau of Labor Statistics	May 2020;
    Map the Meal Gap 2020"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("demographics_poverty.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

#### Health Behavior Risk Factors ####
# physical activity ----
table_risk <- df %>%
  filter(category == "Health Behavior Risk Factor",
         area != "Catchment")

unique(table_risk$area)
table_risk$area <- ordered(table_risk$area, levels = c("USA", "AZ", "Cochise", "Pima", "Pinal", "Santa Cruz", "Yuma"))

unique(table_risk$attribute)

table_risk_pa <- table_risk %>% 
  filter(attribute == "No Leisure-Time Physical Activity" | attribute == "Adult Obesity" | attribute == "Diabetes")

unique(table_risk_pa$attribute)

table_risk_pa$attribute <- as.factor(table_risk_pa$attribute)
table_risk_pa$attribute <- ordered(table_risk_pa$attribute, 
                                           levels = c("No Leisure-Time Physical Activity", "Adult Obesity", "Diabetes"))

table_risk_pa %>%
  filter(race == "All") %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value, fill = attribute)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = .666) +
  geom_label(aes(label = value)) +
  labs(
    title = "Health Behavior and Risk Factors in Catchment Geographies",
    subtitle = "Rates of Physical Activity, Obesity, and Diabetes",
    y = "",
    x = "",
    caption = "Source: 2018 BRFSS Survey Data;
    US Diabetes Surveillance System"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("risk_pa.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

# alcohol and smoking ----
table_risk <- df %>%
  filter(category == "Health Behavior Risk Factor",
         area != "Catchment")

unique(table_risk$area)
table_risk$area <- ordered(table_risk$area, levels = c("USA", "AZ", "Cochise", "Pima", "Pinal", "Santa Cruz", "Yuma"))

unique(table_risk$attribute)

table_risk_behavior <- table_risk %>% 
  filter(attribute == "Excessive drinking (BRFSS)" | attribute == "Adult Smoking")

unique(table_risk_behavior$attribute)

table_risk_behavior$attribute <- as.factor(table_risk_behavior$attribute)

table_risk_behavior %>%
  filter(race == "All") %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value, fill = attribute)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = .666) +
  labs(
    title = "Health Behavior and Risk Factors in Catchment Geographies",
    subtitle = "Rates of Alcohol and Smoking Use",
    y = "",
    x = "",
    caption = "Source: 2017 & 2018 Behavioral Risk Factor Surveillance System"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("risk_smoking.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

# vaccination ----
table_risk <- df %>%
  filter(category == "Health Behavior Risk Factor",
         area != "Catchment")

unique(table_risk$area)
table_risk$area <- ordered(table_risk$area, levels = c("USA", "AZ", "Cochise", "Pima", "Pinal", "Santa Cruz", "Yuma"))

unique(table_risk$attribute)

table_risk_vac <- table_risk %>% 
  filter(attribute == "HPV vaccination (age range 13-17) 3+ doses")

unique(table_risk_vac$attribute)

table_risk_vac %>%
  filter(race == "All") %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value, fill = attribute)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = .666) +
  labs(
    title = "Health Behavior and Risk Factors in Catchment Geographies",
    subtitle = "HPV Vaccination",
    y = "",
    x = "",
    caption = "Source: 2018 National Immunization Survey;
    2017 Immunizations for Adolescents Completion Rates, AHCCCS
    "
  ) +
  theme_clean() +
  theme(legend.position = "") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("risk_vaccination.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

#### Screening ####
table_screening <- df %>%
  filter(category == "Screening",
         area != "Catchment",
         attribute != "Cervical Cancer Screening") %>%
  group_by(area)

unique(table_screening$area)
table_screening$area <- ordered(table_screening$area, levels = c("USA", "AZ", "Cochise", "Pima", "Pinal", "Santa Cruz", "Yuma"))

unique(table_screening$attribute)

table_screening$attribute <- as.factor(table_screening$attribute)

table_screening %>%
  arrange(desc(attribute)) %>%
  ggplot(mapping = aes(x = area, y = value, fill = attribute)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.666) +
  labs(
    title = "Cancer Screening Rates in Catchment Geographies",
    subtitle = "All races and sexes combined",
    y = "",
    x = "",
    caption = "Source: Directly Estimated 2018 BRFSS Data;
    2008-2010 County Level Modeled Estimate Combining BRFSS & NHIS"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Accent")

# save plot to file
ggsave("risk_screening.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

# demographic disparities 
unique(df$attribute)


# all 8 geographic areas
df %>%
  filter(attribute == "College Graduate" |
           attribute == "Rural" |
           attribute == "Hispanic" |
           attribute == "Households Below Poverty Level" |
           attribute == "Food Insecurity" |
           attribute == "Unemployment",
         race == "All") %>%
  ggplot(mapping = aes(x = attribute, y = value, fill = area)) +
  geom_bar(color = "black", stat = "identity", position = "dodge", alpha = .5) +
  labs(
    title = "",
    subtitle = "",
    y = "",
    x = "",
    caption = "Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates;
    Map the Meal Gap 2020"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = mixed_8)

# save plot to file
ggsave("demographic_disparities_01_complete.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)

# only USA, AZ, Catchment
df %>%
  filter(attribute == "College Graduate" |
           attribute == "Rural" |
           attribute == "Hispanic" |
           attribute == "Households Below Poverty Level" |
           attribute == "Food Insecurity" |
           attribute == "Unemployment",
         race == "All",
         area == "USA" |
           area == "AZ" |
           area == "Catchment") %>%
  ggplot(mapping = aes(x = attribute, y = value, fill = area)) +
  geom_bar(color = "black", stat = "identity", position = "dodge", alpha = .5) +
  labs(
    title = "",
    subtitle = "",
    y = "",
    x = "",
    caption = "Source: U.S. Census Bureau, 2014-2018 American Community Survey 5-Year Estimates;
    Map the Meal Gap 2020"
  ) +
  theme_clean() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = blues_3)

# save plot to file
ggsave("demographic_disparities_01_us_az_catchment.svg",
       width = 20,
       height = 11.25,
       device = svg,
       path = "figures/graphics/",
       scale = .5
)
