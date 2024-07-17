#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# NCAP Survey Summary ETL Script & Data Pipeline
# Author: Nick Dodds 
# Version: 0
# This script encompasses the majority of the NCAP Survey Summary Pipeline. It 
# processes the survey monkey data for individual surveys prior to appending 
# the data to varying master files. It pulls in the relevant disaster information 
# by connecting to FEMA's Open Data Portal API as well. Please review the 
# required inputs carefully and ensure they are in the required format and 
# follow the correct naming schema. 
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 0: First Time Running Script? ------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Please run the following line of code to install the rfema package if this is 
# your first time running the script: 
# install.packages("rfema", repos = "https://ropensci.r-universe.dev")
# To install the other required packages run the following line of code: 
# install.packages(
#  c("tidyverse", "readxl", "writexl", "data.table",
#    "lubridate", "uuid", "janitor", "unheadr", "naniar")
#)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Load Required Libraries ---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(readxl)    
library(writexl)    
library(data.table) 
library(lubridate)  
library(uuid)      
library(janitor)    
library(rfema)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Update Dynamic Inputs -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.1: Regional Questions Added? ---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note if Regional Questions were added to ANY of the surveys you will process
## Must be `yes` or `no`
regional_questions_added <- ""

### Input validation
if(regional_questions_added %in% c("yes", "no")) {
  
}else {
  stop(
    "Input for `regional_questions_added` must either be `yes` or `no`."
  )
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.2: Regional Question Disaster ID's ---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Input Disaster Numbers & State code for those surveys with regional Questions
## ex. 1234-CA
regional_question_dr_numbers <- c(
  ""
)
##  Input Validation
if(regional_questions_added == "yes") {
  ###  Input Validation
  if (sum(str_detect(regional_question_dr_numbers, "\\d{4}-[A-Z]{2}")) !=
      length(regional_question_dr_numbers)) {
    stop(
      "Incorrect input for regional_question_dr_numbers.Ensure your inputs for the
    vector follow the correct naming scheme. ex. 1234-CA"
    )
  }
} else{
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.3: Survey Types ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note which survey types to be processed: 
## `recovery` if processing only recovery surveys 
## `response` if processing only response surveys 
## `both` if processing both survey types 

survey_type <- ""

### Input validation
if (survey_type %in% c("recovery", "response", "both")) {
  
} else {
  stop(
    "`survey_type' must be one of three values: response, recovery; both.
    Check the spelling of your input and ensure it's lower-case."
  )
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.4: Response Disaster ID's ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Input Disaster Numbers and state code for Response Surveys (ex. 1234-CA)
response_dr_numbers <- c(
  ""
) %>% 
  # Add Prefix
  paste("DR-", ., sep = "")

# Input Validation
if (survey_type %in% c("response", "both")) {
  if (sum(str_detect(response_dr_numbers, "\\d{4}-[A-Z]{2}")) !=
      length(response_dr_numbers)) {
    stop(
      "Incorrect input for response_dr_numbers. Ensure your inputs for the vector
    follow the correct naming scheme. ex. 1234-CA"
    )
  }
} else{
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.5: Recovery Disaster ID's ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Input Disaster Numbers and state code for Recovery Surveys (ex. 1234-CA)
recovery_dr_numbers  <- c(
  ""
) %>% 
  # Add Prefix 
  paste("DR-", ., sep = "")


## Input Validation
if (survey_type %in% c("recovery", "both")) {
  if (sum(str_detect(recovery_dr_numbers, "\\d{4}-[A-Z]{2}")) !=
      length(recovery_dr_numbers)) {
    stop(
      "Incorrect input for recovery_dr_numbers. Ensure your inputs for the vector
    follow the correct naming scheme. ex. 1234-CA"
    )
  }
} else{
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Static Inputs -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.1: UUID Namespace --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dns_namespace <- "1ea18abc-d669-11ee-8000-9d57bf0070f9"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.2: 2024 Base Question Banks ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3.2a: Recovery Question Bank ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(survey_type %in% c("recovery", "both")){
  ## Recovery Questions
  recovery_questions <-
    c(
      "Respondent_ID",
      "Collector_ID",
      "Survey_Start_Time",
      "Survey_End_Time",
      "IP_Address",
      "Email_Address",
      "First_Name",
      "Last_Name",
      "Disaster_ID_Long",
      "NCAP_Survey_Type",
      "Survey_Close_Date",
      "Is_Reservist",
      "WR1.How well was the overall staffing level matched to the needs of this incident?",
      "WR1a.In which program areas or positions did you notice overstaffing?",
      "WR1b.In which program areas or positions did you notice understaffing?",
      "WR2.Did your Cadre or Section have all the necessary staff to meet the needs of the response?",
      "WR3.Did your Cadre or Section have all the necessary skills to meet the needs of the response?",
      "WR2a.What positions were missing or understaffed in your Cadre or Section?",
      "WR3a.What skills were missing in your Cadre or Section?",
      "WR4a.Equipment (e.g. laptop, phone, tablet, etc)",
      "WR4b.Vehicles",
      "WR4c.Facilities",
      "WR4d.IT Systems and Support",
      "WR4e.Commodities",
      "WR5.If you answered \"no\" to any of the above, what resources were missing?",
      "WR6.Beyond the resources above, what else made you more ready for this deployment?",
      "WR7.Are you currently or regularly employed outside of FEMA?",
      "WR8.Have you utilized Civilian Reservist Emergency Workforce (CREW) Act/Uniformed Services Employment and Reemployment Rights Act (USERRA) protections to keep non-FEMA employment as a FEMA Reservist?",
      "WR8a.If you experienced any challenges utilizing CREW Act/USERRA protections, please explain below.",
      "WR9.Are there any special challenges that come with being a FEMA Reservist that you would like to share?",
      "WR10.Did you receive a signing bonus when you started as a Reservist?",
      "WR10a.Would you have joined FEMA without the signing bonus?",
      "WR11.Are there any special challenges that come with being a full-time FEMA employee that you would like to share?",
      "WR12a.I had all the training, skills, and knowledge needed to perform my role at this incident.",
      "WR12b.The staff I worked with seemed to have all the training, skills, and knowledge needed to perform their roles at this incident.",
      "WR13.Were you a supervisor on this deployment?",
      "WR12c.The staff I supervised had all the training, skills, and knowledge needed to perform their roles at this incident.",
      "WR14.What training, skills, or knowledge would have helped staff you supervised be ready to perform their roles at this incident?",
      "WR15a.I felt ready for my role at this incident.",
      "WR15b.My previous deployment experience enabled me to perform my role at this incident.",
      "WR15c.I had the policy guidance I needed (for example: policies, doctrine, plans, SOPs, and checklists) to perform my role at this incident.",
      "WR16a.Participation in exercises",
      "WR16b.FQS trainings",
      "WR16c.Just-in-time-training (JITT)",
      "WR16d.On-the-job-training",
      "WR16e.Refresher training",
      "WR16f.Job aid for position",
      "WR16g.Policy guide",
      "WR16h.Prior deployed experience",
      "WR16i.Rest or time off",
      "WR17.Beyond the above, what else helped you be more ready for this deployment?",
      "WR18.Did you deploy in-person or virtually to support this incident?",
      "WR18a.Were you given a virtual deployment option?",
      "WR18b.Would you have preferred to deploy virtually?",
      "WR18c.Please share why or why not:",
      "WR18d.I was able to perform my role at this incident in a virtual environment.",
      "WR19.Have you deployed before this incident?",
      "WR19a.How recently did you demobilize from your last deployment?",
      "WR20.How well matched was the length of this deployment to your preferred deployment length?",
      "WR21.How would you prefer to deploy in the future?",
      "WR22.How was the pace of work while deployed?",
      "WR22a.What made the pace of work unsustainable?",
      "WR23a.They were clear and easy to understand.",
      "WR23b.I knew what to expect from this deployment.",
      "WR23c.I understood why I was deployed.",
      "WR23d.They matched my expectations for this deployment.",
      "WR23e.I worked in a role other than the position in which I was originally requested to deploy.",
      "WR23f.You answered that your orders were not clear or didn’t help you understand expectations or deployment reasons. What could be changed to improve future deployment requests?",
      "WR24.Did you process through an in-person or virtual Personnel Mobilization Center (PMC)?",
      "WR24a.How much do you agree or disagree that the Personnel Mobilization Center (PMC) process helped you be more ready for this deployment?",
      "WR25.Did you pair with a Coach & Evaluator (C&E) during your deployment?",
      "WR25a.Did you receive mentorship from a deployed supervisor or Coach & Evaluator while deployed?",
      "WR26a.I understood how to make progress in my PTB.",
      "WR26b.I was able to receive endorsements in my PTB.",
      "WR26c.The PTB endorsements I received reflected all the work I did while deployed.",
      "WR27.Did you receive a Deployed Performance Evaluation(s) (DPE)?",
      "WR27a.How helpful was the feedback you received from the Deployed Performance Evaluation (DPE)?",
      "WR28.How many rest days did you take during your deployment, if any?",
      "WR29.Are you aware of the FEMA Employee Health and Wellness Hub on SharePoint?",
      "WR29a.Have you visited the Employee Health and Wellness Hub before?",
      "WR28a.Were your rest days while deployed helpful or not helpful in performing your role?",
      "WR28b.If there was a regular rest schedule or cycle (for example: 6 days on, 1 day off), what was the schedule?",
      "WR30.Are you aware of FEMA’s Enhanced Demobilization process which allows all FEMA responders demobilizing from an incident to stay in deployed status for up to 16 hours to complete demobilization activities?",
      "WR30a.Have you used the Enhanced Demobilization process before?",
      "WR30b.How many hours did you stay in deployed status the last time you used this process?",
      "WR30c.If you experienced any challenges with following the Enhanced Demobilization process, please share them below:",
      "WR30d.Please share why you haven't used the Enhanced Demobilization process:",
      "WR31.How satisfied or dissatisfied were you with your overall experience during this deployment?",
      "WR31a.Why did you choose that satisfaction level for this deployment?",
      "WR32.Are you considering taking another position in a different Cadre or Section?",
      "WR33.Are you considering leaving the Agency?",
      "WR32a.What could Cadre or Section Management change that might encourage you to stay?",
      "WR33a.What could FEMA change that might encourage you to stay?",
      "WR34.What makes you want to keep working at FEMA?",
      "E1.Did you work directly with survivors or work with staff who worked directly with survivors?",
      "E2.Did you use any equity-related data sources or dashboards to support equitable delivery of assistance at this incident?",
      "E2a.CDC Social Vulnerability Index (SVI) data",
      "E2b.Census data, such as the American Community Survey (ACS)",
      "E2c.IA applicant demographic data from the National Emergency Management Information System (NEMIS)",
      "E2d.ODIC’s Disability Integration Dashboard",
      "E2e.RRAD’s IHP Disaster Assistance Equity Dashboard",
      "E2f.RRAD’s IHP Eligibility Summary Dashboard",
      "E2g.RRAD’s IHP Equity Data Exploration Dashboard",
      "E2h.RRAD’s PA Equity Data Exploration Dashboard",
      "E2i.Other (please specify)",
      "E3a.I found equity-related data sources to be easy to access.",
      "E3b.I had the skills and training to analyze equity-related data.",
      "E3c.I had the tools to analyze equity-related data.",
      "E3d.Equity-related data sources helped me to understand and address barriers to equity for underserved populations.",
      "E4a.Survivors who didn’t speak English had trouble accessing interpreter services.",
      "E4b.Survivors who didn’t read English had trouble accessing or finding translated FEMA disaster assistance materials.",
      "E4c.Survivors who were deaf or hard of hearing had trouble accessing Sign language interpreter services.",
      "E4d.Survivors who were blind or have low vision had trouble accessing or finding Braille or large print FEMA disaster assistance materials.",
      "E4e.Survivors who lacked access to technology (for example: computers, internet, cell phone, etc.) struggled to register for FEMA disaster assistance.",
      "E4f.Survivors who lacked knowledge about how to use technology struggled to register for or understand FEMA disaster assistance.",
      "E4g.FEMA equity-specialized staff (for example: Disability Integration Advisor, Civil Rights Advisor) were available to address any questions or concerns related to equity.",
      "E5a.Survivors in underserved communities did not have a facility located close to them.",
      "E5b.The facility lacked ramps or other reasonable accommodations for survivors with mobility disabilities.",
      "E5c.The facility was not accessible to survivors with mobility disabilities due to a lack of accessible transportation.",
      "E6.Please describe any other barriers to equity you noticed at this incident and any related impact.",
      "E7.Did you support the Individual Assistance program during this incident?",
      "E7a.I know how important equity is to getting Individual Assistance to survivors.",
      "E7b.I know what documentation is needed for survivors to prove they own or occupy their home.",
      "E7c.Survivors seemed to know what documentation is needed for survivors to prove they own or occupy their home.",
      "E7d.Home inspectors seemed to know how to verify survivors own or occupy their home.",
      "E8.Did you notice any strengths or weaknesses in equitable delivery of Individual Assistance?",
      "E9a.Please share any other challenges related to equity.",
      "E9b.Please share any successes related to equity.",
      "RM0a.Individual Assistance (IA)",
      "RM0b.Public Assistance (PA)",
      "RM0c.Mitigation or Resilience-related work",
      "RM1a.Survivors understand the IA process.",
      "RM1b.Survivors understand IA eligibility requirements.",
      "RM1c.Letters sent to survivors are well-written and clear.",
      "RM1d.Letters sent to survivors are informative and instructive.",
      "RM1e.Document requirements for IA applications are reasonable.",
      "RM2.Is there anything else you would like to share on Individual Assistance (IA)?",
      "RM3a.PA leadership effectively set expectations during the disaster overview brief.",
      "RM3b.Applicants understand PA eligibility requirements.",
      "RM3c.Applicants understand PA processing requirements (e.g., timing, documentation).",
      "RM3d.The PA process is straightforward enough for the applicants to understand.",
      "RM3e.PA document requirements are reasonable.",
      "RM3f.Grants Portal is user-friendly and intuitive for the applicants.",
      "RM3g.FEMA answers applicants’ questions in a timely manner.",
      "RM3h.FEMA answers applicants’ questions clearly and accurately.",
      "RM3i.There is effective coordination between PA Field Staff, PA CRC Staff, Mitigation, EHP, IRC, and SLTT.",
      "RM4.Is there anything else you would like to share on PA?",
      "RM5a.Preliminary Damage Assessment",
      "RM5b.PA Hazard Mitigation (406)",
      "RM5c.Hazard Mitigation Grant Program (HMGP)",
      "RM5d.HMGP Post-Fire",
      "RM5e.IA IHP Hazard Mitigation",
      "RM5f.Community Education and Outreach",
      "RM5g.Floodplain Management and Insurance",
      "RM5h.Consensus-Based Codes, Specifications and Standards (DRRA 1235-B)",
      "RM5i.Other (please specify)",
      "RM6a.Preliminary Damage Assessment",
      "RM6b.Applicant Briefing",
      "RM6c.Exploratory Call",
      "RM6d.Recovery Scoping Meeting",
      "RM6e.Previous experience with FEMA",
      "RM6f.Advised by State, Tribe, or Territory",
      "RM6g.Advised by Consultant",
      "RM6h.Other (please specify)",
      "RM7a.Mitigation opportunities are explained to applicants soon after the disaster declaration",
      "RM7b.Applicants are encouraged by FEMA staff to include mitigation with their repairs and rebuilding.",
      "RM7c.Applicants know what mitigation is and how it reduces risk.",
      "RM7d.Applicants understand mitigation policy requirements for eligibility.",
      "RM7e.Applicants find the process for including mitigation easy enough to follow",
      "RM7f.Applicants can afford the cost of mitigation",
      "RM7g.Applicants are comfortable with the time and effort that including mitigation requires",
      "RM7h.Applicants know their locally adopted building codes and floodplain management codes",
      "RM7i.Other (please specify)",
      "RM8.How did you address the challenges you observed? Please share any solutions or potential best practices.",
      "CQ1.If you supported other disaster(s) at the same time as this one, please list them in the box below.",
      "CQ2a.Any other successes not already covered?",
      "CQ2b.Any other challenges not already covered?",
      "CQ2c.Any other recommendations not already covered?",
      "P0.Do you want to continue to answer this next set of questions, or do you want to exit the survey?",
      "P-EQ1.First, we would like to know what you think equity means in the delivery of FEMA disaster response. Feel free to write as much or as little as you’d like.",
      "P-EQ2a.This disaster response made sure underserved areas had a greater number of resources when they were needed.",
      "P-EQ2b.This disaster response addressed the pre-existing disadvantages of underserved communities to achieve equal outcomes for all communities.",
      "P-EQ2c.Disaster leaders signaled to staff that it was important to consider equity in service delivery.",
      "P-EQ3a.Each individual or group receives the same resources and opportunities.",
      "P-EQ3b.Each individual or group has different circumstances and receives different resources and opportunities according to their circumstances to reach the same outcomes as others.",
      "P-EQ3c.Recovery program information is given to everyone in an English language printed flyer available at the Disaster Recovery Center.",
      "P-EQ4a.A society is fair when income and wealth are equally distributed among all people.",
      "P-EQ4b.A society is fair when income and wealth are equally distributed among all people.",
      "P-EQ4c.A society is fair when hard-working people earn more than others.",
      "P-EQ4d.A society is fair when people from families with high social status enjoy privileges in their lives.",
      "P-EQ5a.Lack of equity in FEMA disaster response remains a problem today.",
      "P-EQ5b.Equity policies do little to benefit survivors who are not diverse.",
      "P-EQ5c.Equity policies should be included in all aspects of FEMA disaster response.",
      "P-EQ5d.One's commitment to equity should not be limited to their workplace.",
      "P-EQ5e.Equity policies could help foster a healthier FEMA workplace culture.",
      "P-EQ5f.Equity policies are necessary to improve disaster response outcomes for all survivors.",
      "P-EQ5g.Workplace issues related to equity should be openly discussed during staff or team meetings.",
      "P-EQ5h.FEMA leadership is responsible for teaching its staff how to promote equity in disaster service delivery.",
      "P-EQ5i.Every staff member in FEMA could benefit from more conversations about equity.",
      "P-EQ5j.It is important to hold ongoing discussions about the implications of lack of equity in disaster service delivery.",
      "P-EQ5k.More attention should be given to how disaster response overlooks the needs of underserved communities.",
      "P-EQ5l.If people perform professionally in their role, there is no need to require them to \"practice\" equity in service delivery.",
      "P-EQ6.Thank you for helping us develop these new survey questions. Do you have any comments to share with us about these new questions?"
    )
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3.2b: Response Question Bank ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(survey_type %in% c("response", "both")){
  response_questions <-
    c(
      "Respondent_ID",
      "Collector_ID",
      "Survey_Start_Time",
      "Survey_End_Time",
      "IP_Address",
      "Email_Address",
      "First_Name",
      "Last_Name",
      "Disaster_ID_Long",
      "NCAP_Survey_Type",
      "Survey_Close_Date",
      "Is_Reservist",
      "WR1.How well was the overall staffing level matched to the needs of this incident?",
      "WR1a.In which program areas or positions did you notice overstaffing?",
      "WR1b.In which program areas or positions did you notice understaffing?",
      "WR2.Did your Cadre or Section have all the necessary staff to meet the needs of the response?",
      "WR3.Did your Cadre or Section have all the necessary skills to meet the needs of the response?",
      "WR2a.What positions were missing or understaffed in your Cadre or Section?",
      "WR3a.What skills were missing in your Cadre or Section?",
      "WR4a.Equipment (e.g. laptop, phone, tablet, etc)",
      "WR4b.Vehicles",
      "WR4c.Facilities",
      "WR4d.IT Systems and Support",
      "WR4e.Commodities",
      "WR5.If you answered \"no\" to any of the above, what resources were missing?",
      "WR6.Beyond the resources above, what else made you more ready for this deployment?",
      "WR7.Are you currently or regularly employed outside of FEMA?",
      "WR8.Have you utilized Civilian Reservist Emergency Workforce (CREW) Act/Uniformed Services Employment and Reemployment Rights Act (USERRA) protections to keep non-FEMA employment as a FEMA Reservist?",
      "WR8a.If you experienced any challenges utilizing CREW Act/USERRA protections, please explain below.",
      "WR9.Are there any special challenges that come with being a FEMA Reservist that you would like to share?",
      "WR10.Did you receive a signing bonus when you started as a Reservist?",
      "WR10a.Would you have joined FEMA without the signing bonus?",
      "WR11.Are there any special challenges that come with being a full-time FEMA employee that you would like to share?",
      "WR12a.I had all the training, skills, and knowledge needed to perform my role at this incident.",
      "WR12b.The staff I worked with seemed to have all the training, skills, and knowledge needed to perform their roles at this incident.",
      "WR13.Were you a supervisor on this deployment?",
      "WR12c.The staff I supervised had all the training, skills, and knowledge needed to perform their roles at this incident.",
      "WR14.What training, skills, or knowledge would have helped staff you supervised be ready to perform their roles at this incident?",
      "WR15a.I felt ready for my role at this incident.",
      "WR15b.My previous deployment experience enabled me to perform my role at this incident.",
      "WR15c.I had the policy guidance I needed (for example: policies, doctrine, plans, SOPs, and checklists) to perform my role at this incident.",
      "WR16a.Participation in exercises",
      "WR16b.FQS trainings",
      "WR16c.Just-in-time-training (JITT)",
      "WR16d.On-the-job-training",
      "WR16e.Refresher training",
      "WR16f.Job aid for position",
      "WR16g.Policy guide",
      "WR16h.Prior deployed experience",
      "WR16i.Rest or time off",
      "WR17.Beyond the above, what else helped you be more ready for this deployment?",
      "WR18.Did you deploy in-person or virtually to support this incident?",
      "WR18a.Were you given a virtual deployment option?",
      "WR18b.Would you have preferred to deploy virtually?",
      "WR18c.Please share why or why not:",
      "WR18d.I was able to perform my role at this incident in a virtual environment.",
      "WR19.Have you deployed before this incident?",
      "WR19a.How recently did you demobilize from your last deployment?",
      "WR21.How would you prefer to deploy in the future?",
      "WR22.How was the pace of work while deployed?",
      "WR22a.What made the pace of work unsustainable?",
      "WR23a.They were clear and easy to understand.",
      "WR23b.I knew what to expect from this deployment.",
      "WR23c.I understood why I was deployed.",
      "WR23d.They matched my expectations for this deployment.",
      "WR23e.I worked in a role other than the position in which I was originally requested to deploy.",
      "WR23f.You answered that your orders were not clear or didn’t help you understand expectations or deployment reasons. What could be changed to improve future deployment requests?",
      "WR24.Did you process through an in-person or virtual Personnel Mobilization Center (PMC)?",
      "WR24a.How much do you agree or disagree that the Personnel Mobilization Center (PMC) process helped you be more ready for this deployment?",
      "WR25.Did you pair with a Coach & Evaluator (C&E) during your deployment?",
      "WR25a.Did you receive mentorship from a deployed supervisor or Coach & Evaluator while deployed?",
      "WR26a.I understood how to make progress in my PTB.",
      "WR26b.I was able to receive endorsements in my PTB.",
      "WR26c.The PTB endorsements I received reflected all the work I did while deployed.",
      "WR27.Did you receive a Deployed Performance Evaluation(s) (DPE)?",
      "WR27a.How helpful was the feedback you received from the Deployed Performance Evaluation (DPE)?",
      "WR28.How many rest days did you take during your deployment, if any?",
      "WR29.Are you aware of the FEMA Employee Health and Wellness Hub on SharePoint?",
      "WR29a.Have you visited the Employee Health and Wellness Hub before?",
      "WR28a.Were your rest days while deployed helpful or not helpful in performing your role?",
      "WR28b.If there was a regular rest schedule or cycle (for example: 6 days on, 1 day off), what was the schedule?",
      "WR30.Are you aware of FEMA’s Enhanced Demobilization process which allows all FEMA responders demobilizing from an incident to stay in deployed status for up to 16 hours to complete demobilization activities?",
      "WR30a.Have you used the Enhanced Demobilization process before?",
      "WR30b.How many hours did you stay in deployed status the last time you used this process?",
      "WR30c.If you experienced any challenges with following the Enhanced Demobilization process, please share them below:",
      "WR30d.Please share why you haven't used the Enhanced Demobilization process:",
      "WR31.How satisfied or dissatisfied were you with your overall experience during this deployment?",
      "WR31a.Why did you choose that satisfaction level for this deployment?",
      "WR32.Are you considering taking another position in a different Cadre or Section?",
      "WR33.Are you considering leaving the Agency?",
      "WR32a.What could Cadre or Section Management change that might encourage you to stay?",
      "WR33a.What could FEMA change that might encourage you to stay?",
      "WR34.What makes you want to keep working at FEMA?",
      "E1.Did you work directly with survivors or work with staff who worked directly with survivors?",
      "E2.Did you use any equity-related data sources or dashboards to support equitable delivery of assistance at this incident?",
      "E2a.CDC Social Vulnerability Index (SVI) data",
      "E2b.Census data, such as the American Community Survey (ACS)",
      "E2c.IA applicant demographic data from the National Emergency Management Information System (NEMIS)",
      "E2d.ODIC’s Disability Integration Dashboard",
      "E2e.RRAD’s IHP Disaster Assistance Equity Dashboard",
      "E2f.RRAD’s IHP Eligibility Summary Dashboard",
      "E2g.RRAD’s IHP Equity Data Exploration Dashboard",
      "E2h.RRAD’s PA Equity Data Exploration Dashboard",
      "E2i.Other (please specify)",
      "E3a.I found equity-related data sources to be easy to access.",
      "E3b.I had the skills and training to analyze equity-related data.",
      "E3c.I had the tools to analyze equity-related data.",
      "E3d.Equity-related data sources helped me to understand and address barriers to equity for underserved populations.",
      "E4a.Survivors who didn’t speak English had trouble accessing interpreter services.",
      "E4b.Survivors who didn’t read English had trouble accessing or finding translated FEMA disaster assistance materials.",
      "E4c.Survivors who were deaf or hard of hearing had trouble accessing Sign language interpreter services.",
      "E4d.Survivors who were blind or have low vision had trouble accessing or finding Braille or large print FEMA disaster assistance materials.",
      "E4e.Survivors who lacked access to technology (for example: computers, internet, cell phone, etc.) struggled to register for FEMA disaster assistance.",
      "E4f.Survivors who lacked knowledge about how to use technology struggled to register for or understand FEMA disaster assistance.",
      "E4g.FEMA equity-specialized staff (for example: Disability Integration Advisor, Civil Rights Advisor) were available to address any questions or concerns related to equity.",
      "E5a.Survivors in underserved communities did not have a facility located close to them.",
      "E5b.The facility lacked ramps or other reasonable accommodations for survivors with mobility disabilities.",
      "E5c.The facility was not accessible to survivors with mobility disabilities due to a lack of accessible transportation.",
      "E6.Please describe any other barriers to equity you noticed at this incident and any related impact.",
      "E7.Did you support the Individual Assistance program during this incident?",
      "E7a.I know how important equity is to getting Individual Assistance to survivors.",
      "E7b.I know what documentation is needed for survivors to prove they own or occupy their home.",
      "E7c.Survivors seemed to know what documentation is needed for survivors to prove they own or occupy their home.",
      "E7d.Home inspectors seemed to know how to verify survivors own or occupy their home.",
      "E8.Did you notice any strengths or weaknesses in equitable delivery of Individual Assistance?",
      "E9a.Please share any other challenges related to equity.",
      "E9b.Please share any successes related to equity.",
      "RM0a.Individual Assistance (IA)",
      "RM0b.Public Assistance (PA)",
      "RM0c.Mitigation or Resilience-related work",
      "RM1a.Survivors understand the IA process.",
      "RM1b.Survivors understand IA eligibility requirements.",
      "RM1c.Letters sent to survivors are well-written and clear.",
      "RM1d.Letters sent to survivors are informative and instructive.",
      "RM1e.Document requirements for IA applications are reasonable.",
      "RM2.Is there anything else you would like to share on Individual Assistance (IA)?",
      "RM3a.PA leadership effectively set expectations during the disaster overview brief.",
      "RM3b.Applicants understand PA eligibility requirements.",
      "RM3c.Applicants understand PA processing requirements (e.g., timing, documentation).",
      "RM3d.The PA process is straightforward enough for the applicants to understand.",
      "RM3e.PA document requirements are reasonable.",
      "RM3f.Grants Portal is user-friendly and intuitive for the applicants.",
      "RM3g.FEMA answers applicants’ questions in a timely manner.",
      "RM3h.FEMA answers applicants’ questions clearly and accurately.",
      "RM3i.There is effective coordination between PA Field Staff, PA CRC Staff, Mitigation, EHP, IRC, and SLTT.",
      "RM4.Is there anything else you would like to share on PA?",
      "RM5a.Preliminary Damage Assessment",
      "RM5b.PA Hazard Mitigation (406)",
      "RM5c.Hazard Mitigation Grant Program (HMGP)",
      "RM5d.HMGP Post-Fire",
      "RM5e.IA IHP Hazard Mitigation",
      "RM5f.Community Education and Outreach",
      "RM5g.Floodplain Management and Insurance",
      "RM5h.Consensus-Based Codes, Specifications and Standards (DRRA 1235-B)",
      "RM5i.Other (please specify)",
      "RM6a.Preliminary Damage Assessment",
      "RM6b.Applicant Briefing",
      "RM6c.Exploratory Call",
      "RM6d.Recovery Scoping Meeting",
      "RM6e.Previous experience with FEMA",
      "RM6f.Advised by State, Tribe, or Territory",
      "RM6g.Advised by Consultant",
      "RM6h.Other (please specify)",
      "RM7a.Mitigation opportunities are explained to applicants soon after the disaster declaration",
      "RM7b.Applicants are encouraged by FEMA staff to include mitigation with their repairs and rebuilding.",
      "RM7c.Applicants know what mitigation is and how it reduces risk.",
      "RM7d.Applicants understand mitigation policy requirements for eligibility.",
      "RM7e.Applicants find the process for including mitigation easy enough to follow",
      "RM7f.Applicants can afford the cost of mitigation",
      "RM7g.Applicants are comfortable with the time and effort that including mitigation requires",
      "RM7h.Applicants know their locally adopted building codes and floodplain management codes",
      "RM7i.Other (please specify)",
      "RM8.How did you address the challenges you observed? Please share any solutions or potential best practices.",
      "CQ1.If you supported other disaster(s) at the same time as this one, please list them in the box below.",
      "CQ2a.Any other successes not already covered?",
      "CQ2b.Any other challenges not already covered?",
      "CQ2c.Any other recommendations not already covered?",
      "P0.Do you want to continue to answer this next set of questions, or do you want to exit the survey?",
      "P-EQ1.First, we would like to know what you think equity means in the delivery of FEMA disaster response. Feel free to write as much or as little as you’d like.",
      "P-EQ2a.This disaster response made sure underserved areas had a greater number of resources when they were needed.",
      "P-EQ2b.This disaster response addressed the pre-existing disadvantages of underserved communities to achieve equal outcomes for all communities.",
      "P-EQ2c.Disaster leaders signaled to staff that it was important to consider equity in service delivery.",
      "P-EQ3a.Each individual or group receives the same resources and opportunities.",
      "P-EQ3b.Each individual or group has different circumstances and receives different resources and opportunities according to their circumstances to reach the same outcomes as others.",
      "P-EQ3c.Recovery program information is given to everyone in an English language printed flyer available at the Disaster Recovery Center.",
      "P-EQ4a.A society is fair when income and wealth are equally distributed among all people.",
      "P-EQ4b.A society is fair when income and wealth are equally distributed among all people.",
      "P-EQ4c.A society is fair when hard-working people earn more than others.",
      "P-EQ4d.A society is fair when people from families with high social status enjoy privileges in their lives.",
      "P-EQ5a.Lack of equity in FEMA disaster response remains a problem today.",
      "P-EQ5b.Equity policies do little to benefit survivors who are not diverse.",
      "P-EQ5c.Equity policies should be included in all aspects of FEMA disaster response.",
      "P-EQ5d.One's commitment to equity should not be limited to their workplace.",
      "P-EQ5e.Equity policies could help foster a healthier FEMA workplace culture.",
      "P-EQ5f.Equity policies are necessary to improve disaster response outcomes for all survivors.",
      "P-EQ5g.Workplace issues related to equity should be openly discussed during staff or team meetings.",
      "P-EQ5h.FEMA leadership is responsible for teaching its staff how to promote equity in disaster service delivery.",
      "P-EQ5i.Every staff member in FEMA could benefit from more conversations about equity.",
      "P-EQ5j.It is important to hold ongoing discussions about the implications of lack of equity in disaster service delivery.",
      "P-EQ5k.More attention should be given to how disaster response overlooks the needs of underserved communities.",
      "P-EQ5l.If people perform professionally in their role, there is no need to require them to \"practice\" equity in service delivery.",
      "P-EQ6.Thank you for helping us develop these new survey questions. Do you have any comments to share with us about these new questions?"
    )
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3.2c:Free Response Questions Bank -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
free_responses_questions <-
  c(
    "WR5.If you answered \"no\" to any of the above, what resources were missing?",
    "WR8a.If you experienced any challenges utilizing CREW Act/USERRA protections, please explain below.",
    "WR9.Are there any special challenges that come with being a FEMA Reservist that you would like to share?",
    "WR11.Are there any special challenges that come with being a full-time FEMA employee that you would like to share?",
    "WR14.What training, skills, or knowledge would have helped staff you supervised be ready to perform their roles at this incident?",
    "WR17.Beyond the above, what else helped you be more ready for this deployment?",
    "WR18c.Please share why or why not:",
    "WR22a.What made the pace of work unsustainable?",
    "WR23f.You answered that your orders were not clear or didn’t help you understand expectations or deployment reasons. What could be changed to improve future deployment requests?",
    "WR30c.If you experienced any challenges with following the Enhanced Demobilization process, please share them below:",
    "WR30d.Please share why you haven't used the Enhanced Demobilization process:",
    "WR31a.Why did you choose that satisfaction level for this deployment?",
    "WR32a.What could Cadre or Section Management change that might encourage you to stay?",
    "WR33a.What could FEMA change that might encourage you to stay?",
    "WR34.What makes you want to keep working at FEMA?",
    "E6.Please describe any other barriers to equity you noticed at this incident and any related impact.",
    "E8.Did you notice any strengths or weaknesses in equitable delivery of Individual Assistance?",
    "E9a.Please share any other challenges related to equity.",
    "E9b.Please share any successes related to equity.",
    "RM2.Is there anything else you would like to share on Individual Assistance (IA)?",
    "RM4.Is there anything else you would like to share on PA?",
    "RM8.How did you address the challenges you observed? Please share any solutions or potential best practices.",
    "CQ2a.Any other successes not already covered?",
    "CQ2b.Any other challenges not already covered?",
    "CQ2c.Any other recommendations not already covered?",
    "P-EQ1.First, we would like to know what you think equity means in the delivery of FEMA disaster response. Feel free to write as much or as little as you’d like.",
    "P-EQ6.Thank you for helping us develop these new survey questions. Do you have any comments to share with us about these new questions?"
  )
# Step 4: Process Raw Survey Monkey Data --------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.1: Import Raw Survey Data-------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.1a: Create Import Strings -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import String Logic 
if(survey_type == "response"){
  unprocessed_import_strings <-
    paste(response_dr_numbers, "01-RESPONSE.xlsx", sep = "_")
  # Named vector using Long Disaster ID & Survey Type
  unprocessed_import_strings <- set_names(
    unprocessed_import_strings, 
    str_remove_all(
      unprocessed_import_strings, 
      pattern = "_No.*"
    )
  )
}else if(survey_type == "recovery"){
  unprocessed_import_strings <- 
    paste(recovery_dr_numbers, "02-RECOVERY.xlsx", sep = "_")
  # Named vector using Long Disaster ID & Survey Type
  unprocessed_import_strings <- set_names(
    unprocessed_import_strings, 
    str_remove_all(
      unprocessed_import_strings, 
      pattern = "_No.*"
    )
  )
}else if(survey_type == "both"){
  unprocessed_import_strings <- 
    c(
      paste(response_dr_numbers, "01-RESPONSE.xlsx", sep = "_"), 
      paste(recovery_dr_numbers, "02-RECOVERY.xlsx", sep = "_")
    )
  # Named vector using Long Disaster ID & Survey Type
  unprocessed_import_strings <- set_names(
    unprocessed_import_strings, 
    str_remove_all(
      unprocessed_import_strings, 
      pattern = "_No.*"
    )
  )
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.1b: Import function -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Working Directory
setwd(
  "//fema.net/hqss/NPDSHARE/CIP_NCAP/SurveyData/Unprocessed_IncrementalSurveyData"
) 

# Import Function
unprocessed_surveys <- map(unprocessed_import_strings, ~{
  # Write Lambda Function
  # Import Function 
  read_xlsx(.x) %>% 
    # Remove the First row of data 
    slice(-1) 
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.2: Import DTS Records ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Working Directory 
setwd("//fema.net/hqss/NPDSHARE/CIP_NCAP/SurveyData/ResponderRecords")
# Declare Column Types for DTS File
dts_column_types <- c("text", "text", "text", "date", "text", "text", "text", 
                      "text", "text", "text", "text", "text", "text", "text", 
                      "text", "text", "text", "text", "text", "text", "text", 
                      "text", "text", "text", "text", "text", "text", "text",
                      "text", "text", "numeric", "date", "date", "text", "text",
                      "text", "text", "text", "numeric", "numeric", "numeric",
                      "date", "date", "date", "date", "date", "date", "text",
                      "text", "text", "text", "date", "text", "text", "text", 
                      "text", "text", "text", "text", "text", "text", "text", 
                      "text", "text", "text", "text", "text", "text", "text")

# Import Logic
if(survey_type == "response"){
  ## Response Survey DTS Subset
  dts_subset <- read_xlsx(
    path = "DTSMASTER_PII_RESPONSE.xlsx",
    col_types = dts_column_types,
    na = c("NA", "", NA, "#N/A")
  )
}else if(survey_type == "recovery"){
  ## Recovery Survey DTS Subset
  dts_subset <- read_xlsx(
    path = "DTSMASTER_PII_RECOVERY.xlsx",
    col_types = dts_column_types,
    na = c("NA", "", NA, "#N/A")
  )
}else {
  ## Response and Recovery DTS Subset
  dts_subset <- bind_rows(
    read_xlsx(
      path = "DTSMASTER_PII_RESPONSE.xlsx",
      col_types = dts_column_types,
      na = c("NA", "", NA, "#N/A")
    ),
    read_xlsx(
      path = "DTSMASTER_PII_RECOVERY.xlsx",
      col_types = dts_column_types,
      na = c("NA", "", NA, "#N/A")
    )
  ) 
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.3: DTS Data Wrangling ----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename Variables
dts_subset <- dts_subset %>% 
  # Declare Unique Variable Names
  rename(
    Home_Office = `Employee Region`, 
    Deployed_Program_Area = `Deployed Prog Area`, 
    On_Site_Date = `On Site`, 
    Release_Date = Release, 
    Assigned_Coach_Evaluator = `Assigned C&Es`, 
    Assigned_Trainee = `Assigned Trainees`
  ) %>% 
  # Adjust the Remaining Variables
  rename_with(
    ~ str_replace_all(.x, " ", "_")
  ) %>% 
  # Add DTS Prefix
  rename_with(
    ~ paste("DTS.", .x, sep = ""), 
    .cols = -c(1:5)
  )

# Add and Recode Variables 
dts_subset <- dts_subset %>% 
  mutate(
    # Calculate the Time deployed
    #DTS.Days_Deployed = as.numeric(
    # Convert Seconds to Days 
    # DTS.Release_Date - DTS.On_Site_Date, unit = "days"
    # Add one more day (why?)
    #) + 1,
    # Standardize 'Home Office' values
    DTS.Home_Office = case_when(
      DTS.Home_Office == "Region 1" | DTS.Home_Office == "Region I" ~ "R1",
      DTS.Home_Office == "Region 2" | DTS.Home_Office == "Region II" ~ "R2",
      DTS.Home_Office == "Region 3" | DTS.Home_Office == "Region III" ~ "R3",
      DTS.Home_Office == "Region 4" | DTS.Home_Office == "Region IV" ~ "R4",
      DTS.Home_Office == "Region 5" | DTS.Home_Office == "Region V" ~ "R5",
      DTS.Home_Office == "Region 6" | DTS.Home_Office == "Region VI" ~ "R6",
      DTS.Home_Office == "Region 7" | DTS.Home_Office == "Region VII" ~ "R7",
      DTS.Home_Office == "Region 8" | DTS.Home_Office == "Region VIII" ~ "R8",
      DTS.Home_Office == "Region 9" | DTS.Home_Office == "Region IX" ~ "R9",
      DTS.Home_Office == "Region 10" | DTS.Home_Office == "Region X" ~ "R10",
      DTS.Home_Office == "Non FEMA" ~ "NF", 
      T ~ DTS.Home_Office
    ), 
    # Standardize 'Event Region' variable
    DTS.Event_Region = case_when(
      DTS.Event_Region == "Region 1" ~ "R1",
      DTS.Event_Region == "Region 2" ~ "R2",
      DTS.Event_Region == "Region 3" ~ "R3",
      DTS.Event_Region == "Region 4" ~ "R4",
      DTS.Event_Region == "Region 5" ~ "R5",
      DTS.Event_Region == "Region 6" ~ "R6",
      DTS.Event_Region == "Region 7" ~ "R7",
      DTS.Event_Region == "Region 8" ~ "R8",
      DTS.Event_Region == "Region 9" ~ "R9",
      DTS.Event_Region == "Region 10" ~ "R10",
      T ~ DTS.Event_Region
    ), 
    # Dummy Code the 'Assigned Trainee' variable 
    DTS.Assigned_Trainee = case_when(
      !is.na(DTS.Assigned_Trainee) ~ "Yes", 
      T ~ "No"
    ), 
    # Dummy Code the 'Assigned C&E' variable 
    DTS.Assigned_Coach_Evaluator = case_when(
      !is.na(DTS.Assigned_Coach_Evaluator) ~ "Yes", 
      T ~ "No"
    )
  )

# Subset Variables of Interest
dts_subset <- dts_subset %>% 
  select(
    Disaster_ID, NCAP_Survey_Type, UUID, Unique_ID, DTS.Home_Office, 
    DTS.Event_Region, DTS.Employee_Type, DTS.FQS_Program_Area, DTS.FQS_Position, 
    DTS.FQS_Proficiency, DTS.Deployed_Program_Area, DTS.Deployed_Position, 
    DTS.Deployed_Proficiency, DTS.On_Site_Date, DTS.Release_Date, 
    DTS.Request_Type, DTS.Is_Coach_and_Evaluator, DTS.Assigned_Trainee,
    DTS.Assigned_Coach_Evaluator
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.4: Adjust Variable Names -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Base or Regional Survey Logic
## Base Question Banks 
if (regional_questions_added == "no") {
  # Lambda Function & Iteration
  surveys <-
    # Iterate over Raw Surveys and Apply Changes
    map(unprocessed_import_strings, ~ {
      # Logic for Question Bank
      if (str_detect(.x, "RECOVERY")) {
        question_bank <- recovery_questions
      } else if (str_detect(.x, "RESPONSE")) {
        question_bank <- response_questions
      }
      # Apply Renaming Function and Adjust Variables
      unprocessed_surveys[[.x]] %>%
        rename_with(~ question_bank, everything()) %>%
        mutate(
          # Create UUID's
          Email_Address = tolower(Email_Address),
          UUID = UUIDfromName(dns_namespace, Email_Address),
          # Pull Disaster ID from Import String
          Disaster_ID_Long = str_extract(.x, "DR-\\d{4}"),
          # Pull NCAP Surveyt Type from Import String
          NCAP_Survey_Type = str_to_title(str_extract(.x, "(RESPONSE|RECOVERY)"))
        ) %>%
        # Rename Disaster_ID_Long Variable
        rename(Disaster_ID = Disaster_ID_Long)
    })
} else {
  # Includes at least one Regional Question
  surveys <-
    map(unprocessed_import_strings, ~ {
      # Logic for Regional Question Bank
      if (str_extract(.x,
                      "\\d{4}-[A-Z]{2}") %in% regional_question_dr_numbers) {
        # Regional Question Bank Sheet Name
        regional_question_sheet <- paste("2024", str_extract(.x, "\\d{4}-[A-Z]{2}"), sep = "_")
        # Import Regional Questions
        question_bank <-
          read_xlsx(
            path =
              "//fema.net//hqss//NPDSHARE//CIP_NCAP//SurveyData//NCAP_QuestionColumnComparison.xlsx",
            sheet = regional_question_sheet,
            col_names = T,
            na = ""
          )
        # Logic for Non-Regional Surveys
      } else if (str_detect(.x, "RECOVERY")) {
        question_bank <- recovery_questions
      } else if (str_detect(.x, "RESPONSE")) {
        question_bank <- response_questions
      }
      
      # Apply Renaming Function and Adjust Variables
      unprocessed_surveys[[.x]] %>%
        rename_with(~ question_bank, everything()) %>%
        mutate(
          # Create UUID's
          Email_Address = tolower(Email_Address),
          UUID = UUIDfromName(dns_namespace, Email_Address),
          # Pull Disaster ID from Import String
          Disaster_ID_Long = str_extract(.x, "DR-\\d{4}"),
          # Pull NCAP Surveyt Type from Import String
          NCAP_Survey_Type = str_to_title(str_extract(.x, "(RESPONSE|RECOVERY)"))
        ) %>%
        # Rename Disaster_ID_Long Variable
        rename(Disaster_ID = Disaster_ID_Long)
    })
  
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.5: Join DTS Responder Data to Survey Data --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
surveys <- map(unprocessed_import_strings, ~{
  surveys[[.x]] %>%  
    left_join(
      dts_subset, 
      by = c("Disaster_ID", "NCAP_Survey_Type", "UUID")
    )
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.6: Incremental NCAP Survey Data Wrangling --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adjust Variables 
surveys <- map(unprocessed_import_strings, ~{
  surveys[[.x]] %>% 
    mutate(
      # Reformat Date Variable
      Survey_Close_Date = as_date(Survey_Close_Date, format = "%m/%d/%Y"), 
      # Add in Quarter  Briefing Variable 
      NCAP_Briefing_Quarter = paste(
        year(Survey_Close_Date), quarters(Survey_Close_Date), sep = "-"
      ), 
      # Add Demographic Info 
      D1.Supervisor_Status = `WR13.Were you a supervisor on this deployment?`, 
      D2.Virtual_Deployment = 
        `WR18.Did you deploy in-person or virtually to support this incident?`,
      D3.Concurrent_Deployments = case_when(
        is.na(
          `CQ1.If you supported other disaster(s) at the same time as this one, please list them in the box below.`
        ) ~ "Yes", 
        T ~ "No"
      )
    ) %>% 
    # Remove PII Variables 
    select(
      -c(
        Respondent_ID, Collector_ID, IP_Address, Email_Address, 
        First_Name, Last_Name, Is_Reservist
      )) %>%
    # Adjust Variable Locations
    select(
      Unique_ID, UUID, Survey_Close_Date, Disaster_ID, NCAP_Survey_Type, 
      NCAP_Briefing_Quarter, Survey_Start_Time, Survey_End_Time, 
      matches("^D[1-9]"), 
      starts_with("DTS."), 
      everything()  
    )
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.7: Export Processed Incremental Survey Files -----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.7a: Create Non-PII Surveys ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Base Surveys Only
if (regional_questions_added == "no") {
  non_pii_surveys <- map(unprocessed_import_strings, ~ {
    surveys[[.x]] %>%
      # Remove Free Response Questions
      select(-free_responses_questions)
  })
} else if (regional_questions_added == "yes") {
  # Regional Surveys Included
  non_pii_surveys <- map(unprocessed_import_strings, ~ {
    if (str_extract(.x,
                    "\\d{4}-[A-Z]{2}") %in% regional_question_dr_numbers) {
      surveys[[.x]] %>%
        # Remove Free Response Questions and Regional Questions
        select(-free_responses_questions,-starts_with("RQ"))
    } else{
      surveys[[.x]] %>%
        # Remove Free Response Questions
        select(-free_responses_questions)
    }
  })
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.7b: Create Core Survey Files --------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Full Core Survey
core_ncap_surveys <- surveys
# Non-PII Surveys   
non_pii_core_ncap_surveys <- non_pii_surveys
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.8: Export Functions ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update working Directory 
setwd("R:\\CIP_NCAP\\SurveyData\\Processed_IncrementalSurveyData") 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.8a: Full Core Surveys ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
map(unprocessed_import_strings, ~ {
  # Create Export File Path
  path_name <-
    str_replace(string = .x,
                pattern = ".xlsx",
                replacement = "_PII.xlsx")
  # Write Export Function
  core_ncap_surveys[[.x]] %>%
    write_xlsx(
      list("Responses_PII" = .),
      path = path_name,
      col_names = T,
      format_headers = T
    )
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.8b: Non-PII Surveys -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
map(unprocessed_import_strings, ~ {
  # Create Export File Path
  path_name <-
    str_replace(string = .x,
                pattern = ".xlsx",
                replacement = "_NoPII.xlsx")
  # Write Export Function
  non_pii_core_ncap_surveys[[.x]] %>%
    write_xlsx(
      list("Responses_NoPII" = .),
      path = path_name,
      col_names = T,
      format_headers = T
    )
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.9: Add Incremental Data to Master File -----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Working Directory 
setwd("R:\\CIP_NCAP\\SurveyData\\Processed_MasterSurveyData") 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.9a: Import Master Files ------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Full Survey Master DF
full_ncap_master <- read_xlsx(
  "FULL_NCAPSurvey_MasterDataset.xlsx", 
  col_names = T, 
  na = "",
  guess_max = 10000
)
# Current Survey Master DF
current_ncap_master <- read_xlsx(
  "CURRENT_NCAPSurvey_MasterDataset.xlsx", 
  col_names = T, 
  na = "", 
  guess_max = 10000
)
# Write backups 
## Full Master Survey 
full_ncap_master %>% 
  write_xlsx(
    "FULL_NCAPSurvey_MasterDataset_BACKUP.xlsx", 
    col_names = T, 
    format_headers = T
  )
## Current Master Survey 
current_ncap_master %>% 
  write_xlsx(
    "CURRENT_NCAPSurvey_MasterDataset_BACKUP.xlsx", 
    col_names = T, 
    format_headers = T
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.9b: Harmonize Variable Types Across Files  -----------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create index vectors declaring data types for columns 
## Numeric Columns 
numeric_cols <- c("DTS.Days_Deployed")
## Date Columns
date_cols <- c("Survey_Close_Date","DTS.On_Site_Date","DTS.Release_Date")
## Date and Time Columns
datetime_cols <- c("Survey_Start_Time","Survey_End_Time")

# Transform Columns 
## Full NCAP master Survey
full_ncap_master <- full_ncap_master %>% 
  mutate(
    # Numeric Columns
    across(numeric_cols, as.numeric), 
    # Date Columns
    across(date_cols, as.Date), 
    # Date and Time Columns
    across(datetime_cols, as.POSIXct),
    # Character Columns
    across(-c(numeric_cols, date_cols, datetime_cols))
  )

## Current NCAP master Survey
current_ncap_master <- current_ncap_master %>% 
  mutate(
    # Numeric Columns
    across(numeric_cols, as.numeric), 
    # Date Columns
    across(date_cols, as.Date), 
    # Date and Time Columns
    across(datetime_cols, as.POSIXct),
    # Character Columns
    across(-c(numeric_cols, date_cols, datetime_cols))
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 4.10: Append Data to Master Files & Export ---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bind all Core surveys into a single DF  
processed_core_surveys <- core_ncap_surveys %>% 
  data.table::rbindlist(use.names = T, fill = T)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.10a: Full NCAP Master File ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Core Surveys to Master File 
full_ncap_master <- bind_rows(full_ncap_master, processed_core_surveys)

# Export File 
full_ncap_master %>% write_xlsx(
  path = "FULL_NCAPSurvey_MasterDataset.xlsx", 
  col_names = TRUE, 
  format_headers = TRUE
)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 4.10b: Current NCAP Master File -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add Core Surveys to Master File 
current_ncap_master <- bind_rows(current_ncap_master, processed_core_surveys)

# Export File 
current_ncap_master %>% write_xlsx(
  path = "CURRENT_NCAPSurvey_MasterDataset.xlsx", 
  col_names = TRUE, 
  format_headers = TRUE
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: Disaster Summary Information API Pull -------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.1: Pull DR Numbers -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dr_numbers <- str_extract(unprocessed_import_strings, "\\d{4}")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.2: Connect to FEMA Open Data API -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5.2a: Disaster Declaration Summaries ---------------------------------------
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
### 5.2b: FEMA Web Disaster Declarations --------------------------------------
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
## 5.3: Data Wrangling --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Dummy Variables for IA and PA
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
      any(pa_program_declared == "TRUE") ~ "Yes", 
      T ~ "No"
    )
  ) %>% 
  # Subset Variables of Interest 
  select(-ends_with("program_declared")) %>% 
  # Aggregate now that county level variables are removed
  distinct()

# Create Disaster Summaries DF
disaster_summaries <- full_join(
  disaster_declarations_summaries, 
  web_disaster_declarations, 
  by = "disaster_number"
)

# Rename Variables to match legacy names 
disaster_summaries <- disaster_summaries %>% 
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
## 5.4: Append to Master File & Export  ---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Working Directory 
setwd("R:\\CIP_NCAP\\SurveyData\\Processed_SurveySummary_SourceData")

# Load File
master_disaster_summaries <- 
  read_xlsx("NCAPSurveySummary_DisasterInformation.xlsx")

# Write Back-up
master_disaster_summaries %>% 
  write_xlsx(
    path = "NCAPSurveySummary_DisasterInformation_Backup.xlsx"
  )

# Append Data 
disaster_summaries <-  bind_rows(master_disaster_summaries,
                                 disaster_summaries) 

# Export Data 
disaster_summaries %>% 
write_xlsx(
  path = "NCAPSurveySummary_DisasterInformation.xlsx"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Survey Details ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6.1: Subset Variables of Interest ------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
surveys_subset <- map(unprocessed_import_strings, ~{
  non_pii_core_ncap_surveys[[.x]] %>% 
    # Write Lambda Function
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
## 6.2: Tally Question Type Responses -----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
question_tallys <- map(names(unprocessed_import_strings), ~{
  # Write Lambda Function 
  surveys_subset[[.x]] %>% 
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
        str_to_title(), 
      # Number of available Questions 
      WR_Qs = 22, 
      Equity_Qs = 13, 
      RM_Qs = 13, 
      Closing_Qs = 3
    ) 
}) %>%  
  # Combine list dataframes into a single dataframe 
  rbindlist(use.names = T)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6.3: Gather Total Survey Recipients ----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
survey_recipients <- dts_subset %>% 
  # Filter by Disaster Numbers 
  filter(Disaster_ID %in% str_extract(
    unprocessed_import_strings, 
    pattern = "DR-\\d{4}"
  )) %>% 
  # Unique ID must also be populated 
  filter(!is.na(Unique_ID)) %>% 
  # Tally Total Number of Survey Recipients  
  group_by(Disaster_ID) %>% 
  summarise(Survey_Recipients = n())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6.4: Create Survey Details Dataframe ---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 6.4a: Subset Factor Variables ---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
surveys_subset <- map(unprocessed_import_strings, ~ {
  # Subset the data
  surveys_subset[[.x]] %>%
    # Only need single row for ID variables
    head(1) %>%
    # Select columns of interest
    select(starts_with("Disaster_ID"), starts_with("NCAP"))
}) %>% 
  # Combine into single DF 
  rbindlist(use.names = T)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 6.4b: Join Dataframes -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# First Join
survey_details <- full_join(
  survey_recipients, 
  question_tallys, 
  by = "Disaster_ID"
)
# Final Join
survey_details <- full_join(
  survey_details, surveys_subset, 
  by = c("Disaster_ID", "Disaster_ID_Long", "NCAP_Survey_Type")
)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6.5: Append to Master File & Export ----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import Master File 
master_survey_details <- read_xlsx("NCAPSurveySummary_SurveyDetails.xlsx")

# Export backup 
master_survey_details %>% 
  write_xlsx("NCAPSurveySummary_SurveyDetails_backup.xlsx")

# Append to Master File 
master_survey_details <- bind_rows(master_survey_details, survey_details)

# Export Data 
master_survey_details %>% 
  write_xlsx("NCAPSurveySummary_SurveyDetails.xlsx")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 7: NCAP Survey Summary -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.1: Data Wrangling --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pivot the data longer for the BI Dashboard 
ncap_survey_summary_data <- non_pii_core_ncap_surveys %>% 
  # Bind list DF into a single DF
  rbindlist(use.names = T, fill = T) %>% 
  pivot_longer(
    cols = -c(1:26), 
    names_to = "Question", 
    values_to = "Response"
  )

# Adjust likert scale values 
ncap_survey_summary_data <- ncap_survey_summary_data %>% 
  mutate(
    Response = case_when(
      # Agreement Scale
      Response == 'Strongly Disagree' ~ '[1] Strongly Disagree', 
      Response == 'Disagree' ~ '[2] Disagree',
      Response == 'Neutral' ~ '[3] Neutral',
      Response == 'Agree' ~ '[4] Agree', 
      Response == 'Strongly Agree' ~ '[5] Strongly Agree',
      # Helpfulness Scale
      Response == 'Not at all helpful' ~ '[1] Not at all helpful',
      Response == 'Slightly helpful' | Response == 'Somewhat helpful' ~ 
        '[2] Slightly helpful' ,
      Response == 'Moderately helpful' | Response == 'Helpful' ~ 
        '[3] Moderately helpful', 
      Response == 'Very helpful' ~ '[4] Very helpful',
      Response == 'Strongly Agree' ~ '[5] Strongly Agree',
      Response == 'Extremely helpful' ~ '[5] Extremely helpful',
      # Satisfaction Scale
      Response == 'Completely Dissatisfied' ~ '[1] Completely Dissatisfied',
      Response == 'Dissatisfied' ~ '[2] Dissatisfied',
      Response == 'Neutral' ~ '[3] Neutral',
      Response == 'Satisfied' ~ '[4] Satisfied', 
      Response == 'Completely Satisfied' ~ '[5] Completely Satisfied', 
      # Complete Agreement Scale
      Response == 'Completely Disagree' ~ '[1] Completely Disagree',
      Response == 'Disagree' ~ '[2] Disagree', 
      Response == 'Neutral' ~ '[3] Neutral', 
      Response == 'Agree' ~ '[4] Agree', 
      Response == 'Completely Agree' ~ '[5] Completely Agree', 
      # Staffing Levels 
      Response == 'Heavily Understaffed' ~ '[1] Heavily Understaffed',
      Response == 'Moderately Understaffed' ~ '[2] Moderately Understaffed', 
      Response == 'Slightly Understaffed' ~ '[3] Slightly Understaffed', 
      Response == 'Just Right' | Response == 'Just right' ~ '[4] Just Right', 
      Response == 'Slightly Overstaffed' ~ '[5] Slightly Overstaffed', 
      Response == 'Moderately Overstaffed' ~ '[6] Moderately Overstaffed', 
      Response == 'Heavily Overstaffed' ~ '[7] Heavily Overstaffed', 
      # Months Ago 
      Response == 'Less than 1 month ago' ~ '[1] Less than 1 month ago', 
      Response == '1-3 months ago' ~ '[2] 1-3 months ago', 
      Response == '4-6 months ago' ~ '[3] 4-6 months ago', 
      Response == '7-12 months ago' ~ '[4] 7-12 months ago', 
      Response == '13-24 months ago' ~ '[5] 13-24 months ago', 
      Response == 'More than 24 months ago' ~ '[6] More than 24 months ago', 
      T ~ Response
    )
  )
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.2: Create Frequency Tables -----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create Frequency Table of Respondents by Disaster ID and Question
aggregated_data <- ncap_survey_summary_data %>% 
  # Count only those questions which were answered
  filter(!is.na(Response)) %>% 
  ## Tally the number of responses by question and Disaster ID
  group_by(Disaster_ID, Question) %>% 
  summarise(Respondent_Count = n())

# Join frequency table to the Survey Summary DF
ncap_survey_summary_data <- left_join(
  ncap_survey_summary_data,
  aggregated_data, 
  by = c("Disaster_ID", "Question")
) %>% 
  # Remove unnecessary filter fields 
  select(-c(NCAP_Briefing_Quarter, Survey_Start_Time, Survey_End_Time))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 7.3: Append Data to Master File and Export ---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update Working Directory 
setwd("R:\\CIP_NCAP\\SurveyData\\Processed_SurveySummary_SourceData")

# Import Master File
ncap_survey_summary_master_old <- read_csv(
  file = "NCAPSurveySummary_SurveyResponsesDataset.csv", 
  col_names = T, 
  na = ""
)


# Write backup "just in case"
write_csv(
  ncap_survey_summary_master_old, 
  file = "NCAPSurveySummary_SurveyResponsesDataset_BACKUP.csv", 
  col_names = T, 
  na = ""
)

# Standardize data types across DF's 
## Create index vectors declaring data types for columns 
### Numeric Columns 
numeric_cols <- c("DTS.Days_Deployed","Respondent_Count")
### Date Columns
date_cols <- c("Survey_Close_Date","DTS.On_Site_Date","DTS.Release_Date") 


## Transform Columns 
### Previous NCAP Summary Master File
ncap_survey_summary_master_old <- ncap_survey_summary_master_old %>% 
  mutate(
    # Numeric Columns
    across(numeric_cols, as.numeric), 
    # Date Columns
    across(date_cols, as.Date), 
    # Character Columns
    across(-c(numeric_cols, date_cols))
  )

# Add new summary survey data to the Master File
ncap_survey_summary_master_new <- bind_rows(
  ncap_survey_summary_master_old, ncap_survey_summary_data
)

# Export Master File
write_csv(
  ncap_survey_summary_master_new,
  file = "NCAPSurveySummary_SurveyResponsesDataset.csv", 
  col_names = T, 
  na = ""
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# End of Script ---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls())