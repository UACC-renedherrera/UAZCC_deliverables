# Set up ----
library(here)
library(tidyverse)
library(foreign)

# Read data ----
BRFSS <- read.xport("../data/raw/BRFSS/LLCP2018.XPT")

# refer to previous work ----

BRFSS <- select(
  BRFSS_ADHS_2018,
  "State" = "_STATE", # State FIPS Code
  "Sex" = "SEX1", # Respondents Sex
  "Age_Group" = "_AGE_G", # imputed age in six groups
  "Race" = "_RACE", # Computed Race-Ethnicity grouping
  "Hispanic" = "_HISPANC", # Hispanic, Latino/a, or Spanish origin calculated variable
  "Smoke_Current" = "_RFSMOK3", # Current Smoking Calculated Variable
  "Smoke_Status" = "_SMOKER3", # Computed Smoking Status
  "_CRCREC" = "_CRCREC", # Respondents aged 50-75 who have fully met the USPSTF recommendation
  "Pap_Test" = "HADPAP2", # Ever Had a Pap Test
  "Pap_Last" = "LASTPAP2", # How Long Since Last Pap Test
  "Mammogram_Recent" = "_RFMAM21", # Women respondents aged 40+ who have had a mammogram in the past two years
  "X_HFOB3YR" = "_HFOB3YR", # Respondents aged 50-75 who have had a blood stool test within the past 3 years
  "HADSIGM3" = "HADSIGM3", # Ever Had Sigmoidoscopy/Colonoscopy
  "HPV_Vaccine" = "HPVADVC2", # Have you ever had the HPV vaccination?
  "HPV_Test" = "HPVTEST", # Have you ever had an HPV test?
  "Health_Plan" = "HLTHPLN1", # Have any health care coverage
  "BMI" = "_BMI5CAT", # Computed body mass index categories
  "Physical_Activity" = "_TOTINDA", # Leisure Time Physical Activity Calculated Variable
  "Income_Group" = "_INCOMG", # Computed income categories
  "EDU_Group" = "_EDUCAG", # Computed level of education completed categories
  "Age5" = "_AGEG5YR", # Reported age in five-year age categories calculated variable
  "_RFBLDS3" = "_RFBLDS3", # Respondents aged 50-75 who have had a blood stool test within the past year
  "_FOBTFS" = "_FOBTFS",
  "_COL10YR" = "_COL10YR",
  "LASTSIG3" = "LASTSIG3",
  "Diabetes" = "DIABETE3",
  "Alcohol" = "_DRNKWEK" # computed number of drinks of alcohol bev per week
) 
BRFSS_ADHS_2018 <- BRFSS_ADHS_2018 %>%
  mutate("Year" = "2018")