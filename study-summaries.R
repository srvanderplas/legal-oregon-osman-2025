library(readxl)
library(dplyr)
library(tidyr)
library(purrr)


studies <- read_excel("data/Study_Summary.xlsx")

types <- c("Aperture shear" = "AS", "Breech Face" = "BF", "Bullet" = "B" ,  
           "Cartridge" = "C" ,  "Extractor" = "E", "Slide" =  "S")

clean_studies <- studies |>
  mutate(Volunteers = Volunteers == "T", 
         Data_Available = str_replace_all(
           Data_Available, 
           c("T" = "Y", "Partially" = "P", "F" = "N")), 
         Open = Open == "T",
         Algorithm = Algorithm == "T",
         isPhysical = EvidenceType == "Physical", 
         isPairStudy = StudyType == "Pair",
         isPeerReviewed = PeerReview == "Yes",
         isSciJournal = JournalType == "Research") |>
  mutate(ExamType = factor(ExamType, levels = names(types), ordered = T)) |>
  mutate(across(matches("^( [SD]S)_"), as.numeric))

reliable_studies <- clean_studies |>
  filter(rowSums(is.na(select(clean_studies, matches("^( [SD]S)_")))) == 0) |>
  filter(!Algorithm) |>
  filter(StudyType == "Pair")

reliable_study_cite <- paste(' [', paste(paste0('@', unique(reliable_studies$`Study Key`)), collapse = ";"), ']', sep = '')


library(RefManageR)
mybib <- RefManageR::ReadBib("refs.bib")

clean_studies <- clean_studies |>
  mutate(Study = purrr::map_chr(`Study Key`, 
                                ~cite(keys = ., bib = mybib, 
                                      bibpunct=c("", "",  ",", ", ", ","),
                                      abbreviate = T, longnamesfirst = F)))

study_chars <- clean_studies |>
  filter(!Algorithm) |>
  select(Study, ExamType, Volunteers, Data_Available, isPhysical, isPairStudy, Open, isPeerReviewed:isSciJournal) |>
  arrange(Study, ExamType) |>
  group_by(Study) |>
  mutate(ExamType = paste(ExamType, collapse = ", ") |> str_replace_all(types)) |>
  ungroup() |>
  mutate(RepSample = ifelse(Volunteers, "N", ""),
         isPhysical = ifelse(isPhysical,  "", "N"),
         Open = ifelse(Open, "", "N"),
         isPairStudy = ifelse(isPairStudy, "", "N"),
         isPeerReviewed = ifelse(isPeerReviewed,  "", "N"),
         isSciJournal = ifelse(isSciJournal, "", "N"),
  ) |>
  arrange(ExamType, Volunteers, Data_Available, isPairStudy, Open, isPeerReviewed) |>
  rename(Type = ExamType, `Representative Sample` = RepSample, `Data Available` = Data_Available, `Physical Item` = isPhysical, `Open Set` = Open, `Paired Set` = isPairStudy, `Peer Review` = isPeerReviewed, `Scientific Journal` = isSciJournal) |>
  unique() |>
  select(Study,  Type, `Representative Sample`, `Data Available`:`Scientific Journal`)


study_cdsr <- clean_studies %>%
  filter(!Algorithm) %>%
  arrange(ExamType, Year) %>%
  mutate(ExamType = str_replace_all(ExamType, types)) %>%
  select(Study, Type = ExamType, 
         `# Same Source` = SS_Tot, `# Diff Source` = DS_Tot, 
         `Correct Source Decision Rate` = CSDR, 
         `Overall Error Rate` = Overall_Error, 
         `Inconclusive Rate` = Inconclusive_Rate) %>%
  mutate(across(c(`# Same Source`, `# Diff Source`), ~sprintf("% 5d", as.numeric(.)) %>% 
                  str_replace("NA", " ?"))) %>%
  mutate(across(c(`Correct Source Decision Rate` , `Overall Error Rate`, `Inconclusive Rate`), 
                ~sprintf("%0.4f", .) %>% 
                  str_replace("NA", "?")))
