# Load libraries
library(dplyr)
library(readxl)
library(tidyverse)
library(openxlsx)

## Screening 

# Load csv/excel files 
screened <- read_excel("Data/Screening/articles_screened.xlsx", 
                                sheet = "data check")

# Create clean exclusion reasons
excluded_reasons <- screened$Notes
excluded_reasons[grepl("analysis of previous rct|analyzing old rcts|studying previous results|analysis of old rcts",
                       excluded_reasons)] <- "analysis of previous rct"
excluded_reasons[grepl("protocol", excluded_reasons)] <- "protocol"
excluded_reasons[grepl("not an rct|drug analysis|irrelevant|interview not rct|no trials discussed in detail|not rct and uses intervention|metanalysis|observational not rct|pooled analysis|does not do a trial|editorial comment|Combined result of two studies|retrospective analysis|review article|review artcile on botox|posthoc|meta analysis|meta-analysis|metanlysis|literature search of several rcts|discusses other trials|combined result of two studies|analysis of old trials", excluded_reasons)] <- "not rct"
excluded_reasons[grepl("not 2 arms|not two arms|three arms|not 2 arm|no 2 arms", excluded_reasons)] <- "not 2 arms"
excluded_reasons[grepl("no studying meds|no meds|not studying meds|procedure|procedural intervention|behavioral intervention", excluded_reasons)] <- "not studying meds"
excluded_reasons[grepl("faecal incontinene|mixed incontinence|not overactive|not relevant|not stuying|not studying incontinence", excluded_reasons)] <- "not studying oab"
excluded_reasons[grepl("not two different meds|only studying one med", excluded_reasons)] <- "not 2 arms"
excluded_reasons[grepl("no outcomes|no binary|no p value reported with binary outcomes", excluded_reasons)] <- "no binary outcome"
excluded_reasons[grepl("German|Japanese", excluded_reasons)] <- "not english"
excluded_reasons[grepl("RETRACTED", excluded_reasons)] <- "retracted"

# Check and replace notes (exclusion reason)
unique(excluded_reasons)
screened$Notes <- excluded_reasons

# Write to clean xlsx
write.xlsx(screened, "Data/Clean/articles_screened_clean.xlsx")
rm(screened)

## Data collection

# Studies
studies <- read_excel("Data/Data Collection/articles_final.xlsx",
                      sheet = "Studies") %>% filter(Included=="y")
studies$Arm1.Size <- as.numeric(studies$Arm1.Size)

# Combine variations of journal names
journals <- studies$Journal
journals[grepl("BJU", journals)] <- "BJU International"
journals[grepl("Curr Med Res Opin|CURRENT MEDICAL RESEARCH AND OPINION", journals)] <- "Current Medical Research and Opinion"
journals[grepl("J Urol|Journal of Urology|JOURNAL OF UROLOGY", journals)] <- "Journal of Urology"
journals[grepl("Urology journal", journals)] <- "Urology"
journals[grepl("PAKISTAN JOURNAL OF MEDICAL & HEALTH SCIENCES", journals)] <- "Pakistan Journal of Medical & Health Sciences"
journals[grepl("Int J Clin Pract", journals)] <- "International Journal of Clinical Practice"
journals[grepl("Obstet Gynecol", journals)] <- "Obstetrics & Gynecology"
studies$Journal <- journals

studies$Publication.Year <- as.numeric(studies$Publication.Year)
studies$Control.Arm <- ifelse(is.na(studies$Control.Arm), "Active Comparator",
                              ifelse(studies$Control.Arm == 2 | studies$Control.Arm == 1, 
                                     "Placebo Controlled", "Active Comparator"))
studies$Lost.Fu.Pct <- (as.numeric(studies$Lost.Fu) / studies$Sample.Size) * 100
studies$Study.Phase <- ifelse(studies$Study.Phase == 0, "Unable to Locate", 
                       ifelse(studies$Study.Phase == 3, "III",
                       ifelse(studies$Study.Phase == 4, "IV", NA)))
studies$Blinding <- ifelse(is.na(studies$Blinding), "Not Reported", 
                    ifelse(studies$Blinding == "double", "Double",
                    ifelse(studies$Blinding == "single", "Single",
                    ifelse(studies$Blinding == "not blinded", "Unblinded", NA))))
studies$Lost.Fu.Unknown <- is.na(studies$Lost.Fu)

# Outcomes
outcomes <- read_excel("Data/Data Collection/articles_final.xlsx",
                       sheet = "Outcomes")

# Write merged file for analysis
merged <- left_join(outcomes, studies, by = "Study.ID", 
                    suffix = c(".Outcome", ".Study"))

# Calculate number of outcomes
merged <- merged %>%
  group_by(Study.ID) %>%
  mutate(Num.Outcomes = n()) %>%
  ungroup()

studies <- left_join(studies, merged %>% select(Study.ID, Num.Outcomes) %>% distinct(), 
                     by = "Study.ID")

write.xlsx(merged, "Data/Clean/articles_merged_clean.xlsx")