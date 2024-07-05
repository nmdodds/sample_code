#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# NCAPSurveySummary_SurveyDetails Script
# Author: Nick Dodds 
# Version: 1
# Notes:
# This script updates the NCAPSurveySummary_SurveyDetails file by pulling in 
# the processed, non-identifying, surveys from the shared-drive. You will need 
# to provide two or three inputs prior to running this script: 
# `survey_type` | `response_dr_numbers` | `recovery_dr_numbers`
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 0: First Time Running Script? ------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run the following line of code if the required packages are not yet installed
# on your computer:
# install.packages(c("tidyverse", "readxl", "writexl", "naniar", "data.table"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Load Libraries ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(readxl)
library(writexl)
library(naniar)
library(data.table)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Input Dynamic Fields ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note that you can skip over deleting the survey numbers for survey types 
# you are not processing, the script will run as needed so long as you've 
# supplied the correct input for the `survey_type` variable.


# Note which survey types to be processed: 
## `recovery` if processing only recovery surveys 
## `response` if processing only resposne surveys 
## `both` if processing both survey types 

survey_type <- "both"

# Input Disaster Numbers and state code for Survey (ex. 1234-CA)

## Response Surveys 
response_dr_numbers <- c(
  "1234-CA"
) %>% 
  # Add Prefix
  paste("DR-", ., sep = "")

###  Input Validation
if (sum(str_detect(response_dr_numbers, "\\d{4}-[A-Z]{2}")) !=
    length(response_dr_numbers)) {
  stop(
    "Incorrect input for response_dr_numbers. Ensure your inputs for the vector
    follow the correct naming scheme. ex. 1234-CA"
  )
}

## Recovery Surveys 
recovery_dr_numbers  <- c(
  "1234-CA"
) %>% 
  # Add Prefix 
  paste("DR-", ., sep = "")

### Input Validation
if (sum(str_detect(recovery_dr_numbers, "\\d{4}-[A-Z]{2}")) !=
    length(recovery_dr_numbers)) {
  stop(
    "Incorrect input for recovery_dr_numbers. Ensure your inputs for the vector
    follow the correct naming scheme. ex. 1234-CA"
  )
}


# Create import strings for surveys 
if(survey_type == "response"){
  dr_import_strings <-
    paste(response_dr_numbers, "01-RESPONSE_NoPII.xlsx", sep = "_")
  # Named vector using Long Disaster ID & Survey Type
  dr_import_strings <- set_names(
    dr_import_strings, 
    str_remove_all(
      dr_import_strings, 
      pattern = "_No.*"
    )
  )
}else if(survey_type == "recovery"){
  dr_import_strings <- 
    paste(recovery_dr_numbers, "02-RECOVERY_NoPII.xlsx", sep = "_")
  # Named vector using Long Disaster ID & Survey Type
  dr_import_strings <- set_names(
    dr_import_strings, 
    str_remove_all(
      dr_import_strings, 
      pattern = "_No.*"
    )
  )
}else if(survey_type == "both"){
  dr_import_strings <- 
    c(
      paste(response_dr_numbers, "01-RESPONSE_NoPII.xlsx", sep = "_"), 
      paste(recovery_dr_numbers, "02-RECOVERY_NoPII.xlsx", sep = "_")
    )
  # Named vector using Long Disaster ID
  # Named vector using Long Disaster ID & Survey Type
  dr_import_strings <- set_names(
    dr_import_strings, 
    str_remove_all(
      dr_import_strings, 
      pattern = "_No.*"
    )
  )
}else{
  stop(
    "`survey_type` variable must be one of three values: response, recovery; both.
    Check the spelling of your input and ensure it's lower-case."
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Import Processed Data for Each Survey -------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step 3.1: Create Import Strings --------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create import strings for surveys 
if(survey_type == "response"){
  dr_import_strings <-
    paste(response_dr_numbers, "01-RESPONSE_NoPII.xlsx", sep = "_")
  # Named vector using Long Disaster ID & Survey Type
  dr_import_strings <- set_names(
    dr_import_strings, 
    str_remove_all(
      dr_import_strings, 
      pattern = "_No.*"
    )
  )
}else if(survey_type == "recovery"){
  dr_import_strings <- 
    paste(recovery_dr_numbers, "02-RECOVERY_NoPII.xlsx", sep = "_")
  # Named vector using Long Disaster ID & Survey Type
  dr_import_strings <- set_names(
    dr_import_strings, 
    str_remove_all(
      dr_import_strings, 
      pattern = "_No.*"
    )
  )
}else if(survey_type == "both"){
  dr_import_strings <- 
    c(
      paste(response_dr_numbers, "01-RESPONSE_NoPII.xlsx", sep = "_"), 
      paste(recovery_dr_numbers, "02-RECOVERY_NoPII.xlsx", sep = "_")
    )
  # Named vector using Long Disaster ID
  # Named vector using Long Disaster ID & Survey Type
  dr_import_strings <- set_names(
    dr_import_strings, 
    str_remove_all(
      dr_import_strings, 
      pattern = "_No.*"
    )
  )
}else{
  stop(
    "`survey_type` variable must be one of three values: response, recovery; both.
    Check the spelling of your input and ensure it's lower-case."
  )
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step 3.2: Import Function --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

surveys <- map(dr_import_strings, ~{
  # Write Lambda Function
  # Import Function 
  read_xlsx(.x)  %>% 
    # Extract Long Disaster ID from import string and add it as a variable
    mutate(
      Disaster_ID_Long = str_extract(
        string = .x, 
        pattern = "DR-\\d{4}-[A-Z]{2}"
      )
    ) %>% 
    # Subset columns of interest
    select(Disaster_ID, 
           Disaster_ID_Long, 
           NCAP_Survey_Type, 
           NCAP_Briefing_Quarter,
           starts_with("WR1.H"), 
           starts_with("E1.D"),
           starts_with("RM0"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Tally Question Type Responses for Surveys  --------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

question_tallys <- map(names(dr_import_strings), ~{
  # Write Lambda Function 
  surveys[[.x]] %>% 
    # Extract question prefix
    rename_with(
      ~ str_remove_all(., "\\..*$"), 
      cols = -c(1:4)
    ) %>% 
    mutate(
      # Dummy variable to note if ANY of the RM questions have a response
      rm = case_when(
        !is.na(RM0a) | !is.na(RM0b) | !is.na(RM0c) ~ 1,
        T ~ NA
      )
    ) %>% 
    # Remove RM questions 
    select(-starts_with("RM0")) %>% 
    # Tally Responses for each question type 
    group_by(Disaster_ID) %>% 
    summarise(
      across(
        .cols = -c(1:3), ~ sum(!is.na(.x))
      )
    ) %>% 
    # Fill 
    # Rename variables 
    rename(
      WR_responses = WR1, 
      Equity_responses = E1, 
      RM_responses = rm
    ) %>% 
    # Create New Variables
    mutate(
      # Survey Respondents
      Survey_Respondents = WR_responses, 
      # Long Disaster ID 
      Disaster_ID_Long = str_extract(
        .x, 
        pattern = "DR-\\d{4}-[A-Z]{2}"
      ), 
      # Survey Type 
      NCAP_Survey_Type = str_extract(
        .x, 
        pattern = "(RESPONSE|RECOVERY)"
      ) %>% 
        str_to_title()
    ) 
}) %>%  
  # Combine list dataframes into a single dataframe 
  rbindlist(use.names = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: Create Survey Details Dataframe -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step 5.1: Pull Factor Variables from Surveys -------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Combine Surveys into a single DF with factor variables only
surveys <- map(names(dr_import_strings), ~ {
  # Subset the data
  surveys[[.x]] %>%
    # Only need single row for ID variables
    head(1) %>%
    # Select columns of interest
    select(starts_with("Disaster_ID"), starts_with("NCAP"))
}) %>% 
  # Combine into single DF 
  rbindlist(use.names = T)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step 5.2: Join Question Tallys to OG Survey Dataframe ----------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create Survey Details Dataframe with a join 
survey_details <- full_join(
  surveys, question_tallys, 
  by = c("Disaster_ID", "Disaster_ID_Long", "NCAP_Survey_Type")
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Append New Data to Master Survey Details File -----------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step 6.1: Import Survey Details Master File --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

master_survey_details <- read_xlsx("NCAPSurveySummary_SurveyDetails.xlsx")

# Export backup 
master_survey_details %>% 
  write_xlsx("NCAPSurveySummary_SurveyDetails_backup.xlsx")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step 6.2: Append Data to Master file and Export ----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

bind_rows(master_survey_details, survey_details) %>% 
  write_xlsx("NCAPSurveySummary_SurveyDetails.xlsx")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End of Script ---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())

