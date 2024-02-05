install.packages(c("arrow","gender", "wru", "lubridate", "gtsummary"))
# Load required libraries
library(gender)
library(wru)
library(lubridate)
library(dplyr)
library(gtsummary)
library(arrow)
library(tidyr)
data<- read_feather("C:/Users/zzhong13/Downloads/app_data_starter.feather")
# Assume you have a dataframe named 'data' with the given columns

# Task 1: Create individual-level variables
# ------------------------------------------

examiner_names <- data %>% distinct(examiner_name_first)

examiner_names


data$examiner_name_first

examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

examiner_names_gender




examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
data <- data %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()


examiner_surnames <- data %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_surnames


examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
examiner_race


examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race


examiner_race <- examiner_race %>% 
  select(surname,race)

data <- data %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()


examiner_dates <- data %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates


examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
  ) %>% 
  filter(year(latest_date)<2018)

examiner_dates

data <- data %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()





View(data)


as.Date(data$filing_date)



data$filing_year_quarter <- quarter(data$filing_date, with_year = TRUE)
data$abandon_year_quarter <- quarter(data$abandon_date, with_year = TRUE)





new_application <- data %>%
  group_by(filing_year_quarter) %>%
  summarise(
    num_new_applications = n_distinct(application_number),
  ) %>%
  ungroup()

abandon_application <- data %>%
  group_by(abandon_year_quarter) %>%
  summarise(
    num_new_applications = n_distinct(application_number),
  ) %>%
  ungroup()



# Task 2: Create a panel dataset
# ------------------------------

# Create a quarter variable
data$quarter <- quarter(data$observation_date, with_year = TRUE)


# Task 3: Estimate predictors for turnover and mobility
# ---------------------------------------------------

# Create a logistic regression model for turnover
turnover_model <- glm(separation_indicator ~ gender + race + tenure + num_in_process + num_new_applications +
                        num_abandoned_applications + num_allowed_applications + num_people_in_art_unit +
                        num_women_in_art_unit + num_examiners_by_race, data = panel_data, family = "binomial")

# Create a logistic regression model for AU move
au_move_model <- glm(au_move_indicator ~ gender + race + tenure + num_in_process + num_new_applications +
                       num_abandoned_applications + num_allowed_applications + num_people_in_art_unit +
                       num_women_in_art_unit + num_examiners_by_race, data = panel_data, family = "binomial")

# Task 4: Use gtsummary to create descriptive tables
# --------------------------------------------------

# Create a summary table for turnover model
turnover_summary <- tbl_regression(turnover_model)

# Create a summary table for AU move model
au_move_summary <- tbl_regression(au_move_model)

# Display the tables
print(turnover_summary)
print(au_move_summary)