library(tidyverse)
library(broom)
library(googledrive)
library(googlesheets4)

# sheet only shared with authors
# https://docs.google.com/spreadsheets/d/1nMfwIPLBn77yalGT38tUDX27-MSJFU9wLRMdQlzm0E8/edit#gid=1475711680
ginfo <- drive_get("https://docs.google.com/spreadsheets/d/1nMfwIPLBn77yalGT38tUDX27-MSJFU9wLRMdQlzm0E8/edit#gid=1475711680")
# tab name "RER<1.0"

# fix column names, built byui predictions from linear model object above, kepts speed_group and grade_fixed but not needed, 

dat_validate <- read_sheet(ginfo, sheet = "RER<1.0") %>%
  select(id = `...1`, grade = `Grade(%)`, speed_actual = `Speed (mph)`, actual_vo2kgml =  `Actual vo2/kg/ml`, actual_kjmin = `Actual kj/min`,
         acsm_vo2kgml = `acsm(ml/kg/min)`, lcda_vo2kgml = `LCDA vo2(ml/kg/min)`, acsm_kjmin = `ACSM kj/min`, minetti_kjmin = `Minetti KJ/min`,
         lcda_kjmin = `LCDA kj/min`, pandolf_kjmin = `Pandolf (Kj/min)`, weight_kg = `Weight (kg)`, height = `Height (in)`, age = `Age (yr)`) %>%
  filter(!is.na(speed_actual), !is.na(grade), speed_actual <= 3.5) %>%
  mutate(byui_vo2kgml = predict(pr_model, newdata = .),
         byui_kjmin = byui_vo2kgml*weight_kg*0.0201,
         speed_groups = case_when(speed_actual <= 1.50 ~ 1,
                                  speed_actual > 1.50 & speed_actual <= 2.50 ~ 2,
                                  speed_actual > 2.50 & speed_actual <= 3.50 ~ 3,
                                  speed_actual > 3.50 ~ 4),
         grade_fixed = case_when(grade <= -15 ~ -18,
                                 grade > -15 & grade <= -7 ~ -10,
                                 grade > -7 & grade <= -3 ~ -5,
                                 grade > -3 & grade <= 3.5 ~ 0,
                                 grade > 3.5 & grade <= 7.5 ~ 5,
                                 grade > 7.5 & grade <= 12.5 ~ 10,
                                 grade > 12.5 & grade <= 17.5 ~ 15,
                                 grade > 17.5 & grade <= 22.5 ~ 20,
                                 grade > 22.5 & grade <= 27.5 ~ 25,
                                 grade > 27.5 & grade <= 32.5 ~ 30,
                                 grade > 32.5 & grade <= 37.5 ~ 35,
                                 grade > 37.5 ~ 40),
         combo = str_c(speed_groups %>% 
                         as.character() %>%
                         str_replace_all("1", "low") %>%
                         str_replace_all("2", "med") %>%
                         str_replace_all("3", "hi"), grade_fixed))


  

## validation data

write_csv(dat_validate, path = "data/dat_validation.csv")
