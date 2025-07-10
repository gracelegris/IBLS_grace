# ==========================================================================================================================================
## Script Name: Data Cleaning: Longitudinal Data
## Purpose: Assigns variable names and does data cleaning of baseline and follow-up IBLS data 
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

#source("load_path.R")

# read in data
base_data_ib <- read.csv(file.path(LongDataDir, "Ibadan Longitudinal data/IBLS_baselineall.csv"))
base_data_kn <- read.csv(file.path(LongDataDir, "Re_ Kano longitudinal data/KNLS_baselineall.csv"))
fup_data_ib <- read.csv(file.path(LongDataDir, "Ibadan Longitudinal data/IBLS_ffwupall.csv"))
fup_data_kn <- read.csv(file.path(LongDataDir, "Re_ Kano longitudinal data/KNLS_ffwupall.csv"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Clean Baseline Data
## -----------------------------------------------------------------------------------------------------------------------------------------

clean_baseline_data <- function(base_data) {
  
  # rename important variables
  base_data <- base_data %>%
    rename(
      lga = bi1,
      ward = bi2,
      settlement_type = bi3,
      #community = bi4,
      ea_cluster = bi5,
      household_id = bi6,
      hh_longitude = bi7_long,
      hh_latitude = bi7_lat,
      interview_date = bi9,
      #children_0_10 = q001, # CORRECT - yes 1, no 2
      #parent_age_years = q201a, # CORRECT
      #parent_sex = q202, # CORRECT - male 1, female 2
      child_age_years = q300iii,
      child_age_months = q300iv,
      two_wk_fever = q501, # CORRECT - yes 1, no 2
      #two_wk_suspected_malaria = q502, # CORRECT - yes 1, no 2
      #two_wk_took_test = q510, # CORRECT yes 1, no 2
      #two_wk_test_result = q511b, # CORRECT - positive 1, negative 2
      #two_wk_dr_confirmed_malaria = q512, # CORRECT - yes 1, no 2
      #num_sleeping_rooms = q105,
      #housing_type = q110,
      #type_of_work = q209,
      #work_in_out = q215,
      #test_done = q704, # CORRECT - yes 1, no 2
      test_result = q705 # CORRECT - positive 1, negative 2, indeterminate 3
    )
  
  base_data <- base_data %>% 
    dplyr::select(X, sn, lga, ward, settlement_type, ea_cluster, household_id, hh_longitude, hh_latitude, interview_date,
                  child_age_years, child_age_months, two_wk_fever, test_result)
  
  # recode values
  base_data <- base_data %>%
    mutate(
      settlement_type = factor(
        case_when(
          settlement_type == 1 ~ "formal",
          settlement_type == 2 ~ "informal",
          settlement_type == 3 ~ "slum",
          TRUE ~ NA_character_
        )
      ),
      two_wk_fever = factor(
        case_when(
          two_wk_fever == 1 ~ "yes",
          two_wk_fever == 2 ~ "no",
          TRUE ~ NA_character_
        ),
        levels = c("no", "yes")
      ),
      # two_wk_suspected_malaria = factor(
      #   case_when(
      #     two_wk_suspected_malaria == 1 ~ "yes",
      #     two_wk_suspected_malaria == 2 ~ "no",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("no", "yes")
      # ),
      # two_wk_took_test = factor(
      #   case_when(
      #     two_wk_took_test == 1 ~ "yes",
      #     two_wk_took_test == 2 ~ "no",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("no", "yes")
      # ),
      # two_wk_test_result = factor(
      #   case_when(
      #     two_wk_test_result == 1 ~ "yes",
      #     two_wk_test_result == 2 ~ "no",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("no", "yes")
      # ),
      # two_wk_dr_confirmed_malaria = factor(
      #   case_when(
      #     two_wk_dr_confirmed_malaria == 1 ~ "yes",
      #     two_wk_dr_confirmed_malaria == 2 ~ "no",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("no", "yes")
      # ),
      # children_0_10 = factor(
      #   case_when(
      #     children_0_10 == 1 ~ "yes",
      #     children_0_10 == 2 ~ "no",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("no", "yes")
      # ),
      # parent_sex = factor(
      #   case_when(
      #     parent_sex == 1 ~ "male",
      #     parent_sex == 2 ~ "female",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("female", "male")
      # ),
      # housing_type = factor(
      #   case_when(
      #     housing_type == 1 ~ "face-to-face",
      #     housing_type == 2 ~ "one-bedroom",
      #     housing_type == 3 ~ "two-bedroom",
      #     housing_type == 4 ~ "three-bedroom",
      #     housing_type == 5 ~ "duplex",
      #     housing_type == 6 ~ "other",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("face-to-face", "one-bedroom", "two-bedroom", "three-bedroom", "duplex", "other")
      # ),
      # type_of_work = factor(
      #   case_when(
      #     type_of_work == 1 ~ "professional/technical/managerial",
      #     type_of_work == 2 ~ "clerical",
      #     type_of_work == 3 ~ "sales and services",
      #     type_of_work == 4 ~ "skilled manual",
      #     type_of_work == 5 ~ "unskilled manual",
      #     type_of_work == 6 ~ "agriculture",
      #     type_of_work == 7 ~ "domestic work",
      #     type_of_work == 8 ~ "other",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("professional/technical/managerial", "clerical", "sales and services", "skilled manual", "unskilled manual", "agriculture", "domestic work", "other")
      # ),
      # work_in_out = factor(
      #   case_when(
      #     work_in_out == 1 ~ "indoors",
      #     work_in_out == 2 ~ "outdoors",
      #     work_in_out == 3 ~ "both",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("indoors", "outdoors", "both")
      # ),
      # test_done = factor(
      #   case_when(
      #     test_done == 1 ~ "yes",
      #     test_done == 2 ~ "no",
      #     TRUE ~ NA_character_
      #   ),
      #   levels = c("no", "yes")
      # ),
      test_result = factor(
        case_when(
          test_result == 1 ~ "positive",
          test_result == 2 ~ "negative",
          test_result == 3 ~ "indeterminate",
          TRUE ~ NA_character_
        ),
        levels = c("negative", "positive", "indeterminate")
      )
    )
  
}

base_data_ib <- clean_baseline_data(base_data_ib) %>% mutate(state = "Ibadan")
base_data_kn <- clean_baseline_data(base_data_kn) %>% mutate(state = "Kano")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Clean Follow-Up Data
## -----------------------------------------------------------------------------------------------------------------------------------------

clean_fup_data <- function(fup_data) {
  
  fup_data <- fup_data %>%
    rename_with(
      ~ str_replace_all(., 
                        c(
                          "q501_follow_up_(\\d+)_arm_1" = "two_wk_fever_m\\1",
                          "q502_follow_up_(\\d+)_arm_1" = "two_wk_sus_malaria_m\\1"
                        )
      ),
      .cols = matches("q501_follow_up_|q502_follow_up_")
    ) %>% 
    rename_with(
      .fn = ~ paste0("tested_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
      .cols = starts_with("q510_follow_up_")
    ) %>%
    rename_with(
      .fn = ~ paste0("remember_result_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
      .cols = starts_with("q511a_follow_up_")
    ) %>%
    rename_with(
      .fn = ~ paste0("test_result_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
      .cols = starts_with("q511b_follow_up_")
    ) %>% 
    rename_with(
      .fn = ~ paste0("study_rdt_done_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
      .cols = starts_with("q704_follow_up_")
    ) %>%
    rename_with(
      .fn = ~ paste0("study_rdt_result_m", gsub(".*_(\\d+)_.*", "\\1", .x)),
      .cols = starts_with("q705_follow_up_")
    )
  
  # get the renamed fever and malaria variable names
  fever_vars <- paste0("two_wk_fever_m", 1:12)
  malaria_vars <- paste0("two_wk_sus_malaria_m", 1:12)
  tested_vars <- paste0("tested_m", 1:12)
  remember_vars <- paste0("remember_result_m", 1:12)
  result_var <- paste0("test_result_m", 1:12)
  study_test_var <- paste0("study_rdt_done_m", 1:12)
  study_test_result_var <- paste0("study_rdt_result_m", 1:12)
  
  # function to recode yes/no
  recode_yes_no <- function(x) {
    factor(
      case_when(
        x == 1 ~ "yes",
        x == 2 ~ "no",
        TRUE ~ NA_character_
      ),
      levels = c("no", "yes")
    )
  }
  recode_pos_neg <- function(x) {
    factor(
      case_when(
        x == 1 ~ "positive",
        x == 2 ~ "negative",
        x == 3 ~ "indeterminate",
        TRUE ~ NA_character_
      ),
      levels = c("negative", "positive", "indeterminate")
    )
  }
  
  # apply recoding to all monthly columns
  fup_data <- fup_data %>%
    mutate(across(all_of(c(fever_vars, malaria_vars, tested_vars, remember_vars, study_test_var)), recode_yes_no)) %>% 
    mutate(across(all_of(c(result_var, study_test_result_var)), recode_pos_neg))

}

fup_data_ib <- clean_fup_data(fup_data_ib)
fup_data_kn <- clean_fup_data(fup_data_kn)

# select only relevant variables
condense <- function(fup_data) {
  fup_data <- fup_data %>%
    dplyr::select(sn, starts_with("two_wk_fever_m"), starts_with("two_wk_sus_malaria_m"), starts_with("tested_m"), starts_with("remember_result_m"), starts_with("test_result_m"), starts_with("study_rdt_done_m"), starts_with("study_rdt_result_m"))
  fup_data_rdt <- fup_data %>% 
    dplyr::select(sn, starts_with("two_wk_fever_m"), starts_with("study_rdt_done_m"), starts_with("study_rdt_result_m"))
}

fup_data_ib <- condense(fup_data_ib) %>% mutate(state = "Ibadan")
fup_data_kn <- condense(fup_data_kn) %>% mutate(state = "Kano")
