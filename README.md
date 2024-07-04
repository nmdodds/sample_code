These three scripts make up the data processing pipeline for an internal agency BI dashboard. The data used in this pipeline cannot be shared, and as such cannot be ran. Furthermore, I've had to remove some code within the ETL script. Specifically, the working directorie locations have been removed for security purposes. 
\nI wrote these scripts such that anyone on the team with basic knowledge of coding in R could update the needed values and run them. 
\nA short summary of the scripts: 
\nETL Script: standard ETL script. A small difference is in that we do not have a relational database or cloud storage for security purposes. As such, the script points to different locations on an agency share drive to pull from. 
\nOpen FEMA API: Connects to and pulls data from the FEMA Open Data Portal
\nSurvey Details: Pulls in indvidual survey files and creates an aggregated details file. 
