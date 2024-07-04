#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# NCAPSurveySummary_DisasterInformation Script
# Author: Nick Dodds 
# Version: 1
# Notes:
# This script will connect to the FEMA Open Data Portal via its API and pull in 
# relavent data for the inputted disaster numbers. The new data will be appended
# to the current NCAPSurveySummary_DisasterInformation file that is connected 
# to the NCAP Survey Summary PowerBI dashboard. There is a single variable that 
# needs to be updated: `dr_numbers` prior to running the script. 
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 0: First Time Running Script? ------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Please run the following line of code to install the rfema package if this is 
# your first time running the script: 
# install.packages("rfema", repos = "https://ropensci.r-universe.dev")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Load Libraries ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(rfema)
library(janitor)
library(readxl)
library(writexl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Input Dynamic Fields  -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Input disaster Numbers (as characters)
## Adjust the size of the vector as needed
dr_numbers <- c(
  "4763",
  "4779"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Connect to FEMA Open Data API  --------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step 3.1: Disaster Declaration Summaries  ----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Variables to pull 
variables <- c(
  "femaDeclarationString",
  "disasterNumber", 
  "incidentBeginDate", 
  "incidentEndDate", 
  "declarationDate",
  "ihProgramDeclared", 
  "iaProgramDeclared", 
  "paProgramDeclared"
)

# Load Subset of Interest 
disaster_declarations_summaries <- open_fema(
  # Data set of interest
  data_set = "DisasterDeclarationsSummaries", 
  # Variables of Interest 
  select = variables, 
  # Filters 
  filters = list(
    disasterNumber = dr_numbers
  )
) %>%
  # Tidynames
  janitor::clean_names()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Step 3.1a: Individual & Public Assistance Flags ---------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create Flags
disaster_declarations_summaries <- disaster_declarations_summaries %>%
  # Group by disaster number
  group_by(disaster_number) %>%
  mutate(
    # Create Initial IA Flag
    ia_flag = case_when(
      ia_program_declared == "TRUE" |
        ih_program_declared == "TRUE" ~ 1,
      T ~ 0
    ),
    # Create Aggregated IA Flag
    ia_flag = case_when(
      any(ia_flag == 1) ~ "Yes", 
      T ~ "No"
    ), 
    # Create Aggregated PA Flag 
    pa_flag = case_when(
      any(pa_program_declared == "TRUE") ~ "Categories A-G", 
      T ~ "No"
    )
  ) %>% 
  # Subset Variables of Interest 
  select(-ends_with("program_declared")) %>% 
  # Aggregate now that county level variables are removed
  distinct()
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step 3.2: FEMA Web Disaster Declarations ------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Variables to pull 
variables <- c(
  "disasterNumber", 
  "disasterName",
  "incidentType",
  "stateCode",
  "stateName", 
  "region"
)

# Load Subset of Interest 
web_disaster_declarations <- open_fema(
  # Data set of interest
  data_set = "FemaWebDisasterDeclarations", 
  # Variables of Interest 
  select = variables, 
  # Filters 
  filters = list(
    disasterNumber = dr_numbers
  )
) %>%
  # Tidynames
  janitor::clean_names()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Combine Data Sources  -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

disaster_summaries <- full_join(
  disaster_declarations_summaries, 
  web_disaster_declarations, 
  by = "disaster_number"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: Data Wrangling  -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

disaster_summaries <- disaster_summaries %>% 
  # Rename Variables to match legacy names 
  rename(
    Disaster_ID = disaster_number, 
    Disaster_ID_Long = fema_declaration_string, 
    Incident_Name = disaster_name, 
    Incident_Type = incident_type, 
    Incident_Start = incident_begin_date, 
    Incident_End = incident_end_date, 
    Declaration_Date = declaration_date, 
    State_Name = state_name, 
    State_Code = state_code, 
    Region = region, 
    `Individual Assistance?` = ia_flag, 
    `Public Assistance?` = pa_flag 
  ) %>% 
  # Add in Link to Disaster Map
  mutate(
    Declaration_Map = paste0(
      "https://gis.fema.gov/maps/disaster/dec_", Disaster_ID, ".png"
    ), 
    Region = as.numeric(Region)
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Add to Master File --------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load File
master_file <- readxl::read_xlsx("NCAPSurveySummary_DisasterInformation.xlsx")

# Add rows 
disaster_summaries <-  bind_rows(master_file, disaster_summaries) 

# Export Data 

writexl::write_xlsx(
  disaster_summaries, 
  "NCAPSurveySummary_DisasterInformation.xlsx"
)

write_xlsx(disaster_summaries, "disaster_summaries.xlsx")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End of Script ---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())