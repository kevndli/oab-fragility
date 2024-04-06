## Packages
library(fragility)
library(readxl)

## Fragility analyses
merged <- read_excel("Data/Clean/articles_merged_clean.xlsx")

# Manual corrections, if data is fully cleaned should run without these
# merged <- merged %>% filter(Outcome.Arm1.Event <= Arm1.Size)
# merged <- merged %>% filter(Outcome.Arm2.Event <= Arm2.Size)
merged$Outcome.Test <- ifelse(merged$Outcome.Test %in% c("Fisher", "chisq", "OR", "RD"), 
                              merged$Outcome.Test, 
                              "Fisher")

# Add fragility index data
merged$Frag.Pvalue <- NA
merged$FI <- NA
merged$FQ <- NA
merged$dir <- NA
for(i in 1:nrow(merged)) {
  test <- merged[i,]$Outcome.Test
  frag <- frag.study(e0 = Outcome.Arm1.Event, n0 = Arm1.Size, 
               e1 = Outcome.Arm2.Event, n1 = Arm2.Size, 
               data = merged[i,], methods = test, alpha = 0.05)
  merged[i, "Frag.Pvalue"] <- frag$pval
  merged[i, "FI"] <- frag$FI
  merged[i, "FQ"] <- frag$FQ
  merged[i, "dir"] <- frag$dir
}

# Add stratifying variables
merged <- merged %>% mutate(
  `Outcome Type` = ifelse(Outcome.Type == "primary", "Primary", 
                   ifelse(Outcome.Type == "secondary", "Secondary", 
                   ifelse(Outcome.Type == "adverse event", "Adverse Event", NA))),
  
  `Sample Size` = ifelse(Sample.Size<100, "Small (<100)", 
                  ifelse(Sample.Size<500, "Medium (100-499)", 
                  ifelse(Sample.Size>=500, "Large (>=500)", NA))),
  
  `Journal Impact` = ifelse(Journal.IF<3, "Low (<3)", 
                     ifelse(Journal.IF>=3, "High (>=3)", NA)),
  
  `Comparison Group` = Control.Arm,
  
  `Medication Class` = ifelse(Med.Class == "other", "Other",
                       ifelse(Med.Class == "b3agonist", "B3 Agonist",
                       ifelse(Med.Class == "anticholinergic", "Anticholinergic", NA))),
  
  `Trial Blinding` = Blinding,
  
  `Significance` = ifelse(dir == "non-significance altered to significance", "Non-Significant", 
                          ifelse(dir == "significance altered to non-significance", "Significant", NA)),
  
  `Trial Phase` = Study.Phase
)

rm(excluded_reasons, i, test, journals, frag)