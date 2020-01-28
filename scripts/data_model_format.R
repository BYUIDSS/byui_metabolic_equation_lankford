library(tidyverse)
library(broom)

### Model Data Previously Collected

# Breanna Neuwirth created this file.  I don't have the provenance.  
# Her file was called '/derived_data/All_Data_cleaned1.csv'

dat_model <- read_csv("data/dat_cleaned.csv") %>%
  select(subject = SUBJECT, grade = GRADE, speed_actual = ACT.SPEED, rer = RER, actual_vo2kgml = VO2, age = AGE, weight_kg = WEIGHT_kg, height = HEIGHT) %>%
  filter(rer <= 1, !is.na(speed_actual), !is.na(grade), !is.na(actual_vo2kgml), speed_actual <= 3.50) %>%
  mutate(speed_groups = case_when(
    speed_actual <= 1.50 ~ 1,
    speed_actual > 1.50 & speed_actual <= 2.50 ~ 2,
    speed_actual > 2.50 & speed_actual <= 3.50 ~ 3,
    speed_actual > 3.50 ~ 4), 
    combo = str_c(speed_groups %>% 
                    as.character() %>%
                    str_replace_all("1", "low") %>%
                    str_replace_all("2", "med") %>%
                    str_replace_all("3", "hi"), grade),
    speed_actual_metersecond = speed_actual*0.44704) %>% 
  filter(!combo %in% c("hi25", "hi30", "hi40"))
# https://www.google.com/search?q=miles+per+hour+to+meters+per+second&oq=miles+per+hour+to+meters+per+second&aqs=chrome..69i57.4130j0j1&sourceid=chrome&ie=UTF-8


########## Model Code

## Miles per hour
pr_model <- lm(actual_vo2kgml ~ speed_actual + grade + grade:speed_actual + I(grade^2):speed_actual + I(grade^3):speed_actual, data = dat_model)

summary(pr_model) %>%
  tidy() %>%
  bind_cols(confint_tidy(pr_model)) %>%
  knitr::kable()

model_data <- augment(pr_model) %>%
  mutate(speed_groups = case_when(
    speed_actual <= 1.50 ~ 1,
    speed_actual > 1.50 & speed_actual <= 2.50 ~ 2,
    speed_actual > 2.50 & speed_actual <= 3.50 ~ 3,
    speed_actual > 3.50 ~ 4))

# This is the data to show the three model based lines using three fixed speeds in the prediction.
model_fixedspeeds_plot <- model_data %>%
  select(actual_vo2kgml:I.grade.3.) %>%
  mutate(speed_actual = case_when(
    speed_actual <= 1.50 ~ 1.11,
    speed_actual > 1.50 & speed_actual <= 2.50 ~ 2.05,
    speed_actual > 2.50 & speed_actual <= 3.50 ~ 2.95,
    speed_actual > 3.50 ~ 4)) %>%
  augment(pr_model, newdata = ., type.predict = "response")

################ End Model Miles per hour  ##########

## Meters per second
pr_model_ms <- lm(actual_vo2kgml ~ speed_actual_metersecond + grade + grade:speed_actual_metersecond + I(grade^2):speed_actual_metersecond + I(grade^3):speed_actual_metersecond, data = dat_model)

summary(pr_model_ms) %>%
  tidy() %>%
  bind_cols(confint_tidy(pr_model_ms)) %>%
  knitr::kable()


    

###### Save Objects for report

## model objects

write_rds(pr_model, path = "data/polinomial_regression_model.rds")
write_rds(pr_model_ms, path = "data/polinomial_regression_model_metersecond.rds")
write_csv(model_data, path = "data/training_model_data.csv" )
write_csv(model_fixedspeeds_plot, path = "data/fixedspeeds_plot_data.csv" )

## training data

write_csv(dat_model, "data/dat_model.csv")  




