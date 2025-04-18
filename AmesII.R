library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

source("make_tables.R")

#### -------------------------- Read in Data -------------------------- ####
ames2_accuracy_orig <- read_csv("data/AmesII-reliability.csv")
ames2_repeatability_orig <- read_csv("data/AmesII-repeatability.csv", skip = 1, name_repair = make.names)
ames2_reproducibility_orig <- read_csv("data/AmesII-reproducibility.csv", skip = 1, name_repair = make.names)

#### -------------------------- Ames II settings -------------------------- ####
ames2_conclusion_levels <- c("ID", "INC-A", "INC-B", "INC-C", "EL", "US", "Other")
ames2_res_scale <- c(brewer.pal(5, "RdBu"), "grey", "black")


#### -------------------------- Repeatability -------------------------- ####
ames2_repeatability <- ames2_repeatability_orig |>
  filter(!str_detect(X1st.eval, "Total")) |>
  pivot_longer(ID:Total.US, names_to = "X2nd.eval", values_to = "Count") |>
  filter(!str_detect(X2nd.eval, "Total")) |>
  rename(eval1 = X1st.eval, eval2 = X2nd.eval) |>
  mutate(eval2 = str_replace(eval2, "\\.", "-")) |>
  group_by(Type, Comparison) |>
  mutate(comparison_pct = Count/sum(Count)*100)

ames2_repeatability_us <- ames2_repeatability |> 
  mutate(suitable1 = eval1 != "US", 
         suitable2 = eval2 != "US", 
         match = (suitable1 == suitable2)) |>
  group_by(suitable1, suitable2, match) |>
  summarize(Count = sum(Count)) |>
  ungroup() |>
  mutate(class = if_else(match & suitable1, "Suitable", if_else(match, "Unsuitable", "Conflict"))) |>
  select(-suitable1, -suitable2, -match) |>
  mutate(rate = Count/sum(Count) * 100) |>
  select(-Count) |>
  pivot_wider(names_from = class, values_from = rate, values_fn = sum)

#### -------------------------- Reproducibility -------------------------- ####
ames2_reproducibility <- ames2_reproducibility_orig |>
  filter(!str_detect(X1st.eval, "Total")) |>
  pivot_longer(ID:Total.US, names_to = "X2nd.eval", values_to = "Count") |>
  filter(!str_detect(X2nd.eval, "Total")) |>
  rename(eval1 = X1st.eval, eval2 = X2nd.eval) |>
  mutate(eval2 = str_replace(eval2, "\\.", "-")) |>
  group_by(Type, Comparison) |>
  mutate(comparison_pct = Count/sum(Count)*100)

ames2_reproducibility_us <- ames2_reproducibility |> 
  mutate(suitable1 = eval1 != "US", 
         suitable2 = eval2 != "US", 
         match = (suitable1 == suitable2)) |>
  group_by(suitable1, suitable2, match) |>
  summarize(Count = sum(Count)) |>
  ungroup() |>
  mutate(class = if_else(match & suitable1, "Suitable", if_else(match, "Unsuitable", "Conflict"))) |>
  select(-suitable1, -suitable2, -match) |>
  mutate(rate = Count/sum(Count) * 100) |>
  select(-Count) |>
  pivot_wider(names_from = class, values_from = rate, values_fn = sum)

#### -------------------------- Accuracy -------------------------- ####
ames2_accuracy <- ames2_accuracy_orig |>
  filter(!str_detect(Eval, "Total")) |>
  group_by(Type, Comparison) |>
  rename(Count = Total) |>
  mutate(Total = sum(Count), comparison_pct = Count/Total*100)

ames2_accuracy_us <- filter(ames2_accuracy, Eval == "US")

ames2_accuracy_csdr <- ames2_accuracy |>
  filter(!Eval %in% c("US", "Other")) |>
  mutate(csd = (Comparison == "SS" & Eval == "ID") | (Comparison == "DS" & Eval == "EL")) |>
  group_by(Type, Comparison) |>
  mutate(Total = sum(Count)) |>
  ungroup() |>
  group_by(Type, csd) |>
  summarize(Count = sum(Count)) |>
  group_by(Type) |>
  mutate(csdr = Count/sum(Count)*100) |>
  filter(csd) |>
  select(Type, csdr)

ames2_accuracy_sens_spec <- ames2_accuracy |>
  filter(!Eval %in% c("US", "Other")) |>
  mutate(error = !((Comparison == "SS" & Eval == "ID") | (Comparison == "DS" & Eval == "EL"))) |>
  group_by(Type, Comparison) |>
  summarize(rate = sum(Count*error)/sum(Count) * 100)
ames2_accuracy_sens <- filter(ames2_accuracy_sens_spec, Comparison == "SS")
ames2_accuracy_spec <- filter(ames2_accuracy_sens_spec, Comparison == "DS")


ames2_accuracy_fp_fn <- ames2_accuracy |>
  filter(!Eval %in% c("US", "Other")) |>
  mutate(error = ((Comparison == "SS" & Eval == "EL") | (Comparison == "DS" & Eval == "ID"))) |>
  group_by(Type, Comparison) |>
  summarize(rate = sum(Count*error)/sum(Count) * 100)
ames2_accuracy_fn <- filter(ames2_accuracy_fp_fn, Comparison == "SS")
ames2_accuracy_fp <- filter(ames2_accuracy_fp_fn, Comparison == "DS")

ames2_accuracy_ppv_npv <- ames2_accuracy |>
  filter(!Eval %in% c("US", "Other") & !str_detect(Eval, "INC")) |>
  mutate(error = ((Comparison == "SS" & Eval == "ID") | (Comparison == "DS" & Eval == "EL"))) |>
  group_by(Type, Eval) |>
  summarize(rate = sum(Count*error)/sum(Count) * 100)
ames2_accuracy_ppv <- filter(ames2_accuracy_ppv_npv, Eval == "ID")
ames2_accuracy_npv <- filter(ames2_accuracy_ppv_npv, Eval == "EL")



#### -------------------------- Dropout -------------------------- ####
ames2_first_round <- sprintf("%.2f%%", (1 - 173/256)*100)
ames2_first_to_last_round <- sprintf("%.2f%%", (1 - 79/173)*100)
ames2_recruited_completed_all <-  sprintf("%.2f%%", (1 - 79/256)*100)
ames2_bullet_nonresponse <- sprintf("%.2f%%", (1 - 10020/(256*15*6))*100)
ames2_cartridge_nonresponse <- sprintf("%.2f%%", (1 - 10110/(256*15*6))*100)
ames2_overall_nonresponse <- sprintf("%.2f%%", (1 - (10020 + 10110)/(256*30*6))*100)

