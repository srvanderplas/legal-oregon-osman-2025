library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

source("make_tables.R")

#### -------------------------- Read in Data -------------------------- ####
hicklin_accuracy_orig <- read_excel("data/Hicklin2024.xlsx", sheet = 1)
hicklin_repeatability_orig <- read_excel("data/Hicklin2024.xlsx", sheet = 2, skip = 1) |>
  rename(eval1=`1st Eval`)
hicklin_reproducibility_orig <- read_excel("data/Hicklin2024.xlsx", sheet = 3, skip = 1) |>
  rename(eval1=`1st Eval`)


#### -------------------------- Hicklin Settings -------------------------- ####
hicklin_conclusion_levels <- c("ID", "INC-A", "INC-B", "INC-C", "EL")

hicklin_res_scale <- c(brewer.pal(5, "RdBu"), "grey", "black")


#### -------------------------- Repeatability -------------------------- ####

hicklin_repeatability <- hicklin_repeatability_orig |>
  filter(!str_detect(eval1, "Total")) |>
  pivot_longer(ID:EL, names_to = "eval2", values_to = "Count") |>
  mutate(eval2 = str_replace(eval2, "\\.", "-")) |>
  mutate(across(Polygonal:SameAmmo, ~.=="T")) |>
  mutate(eval1 = factor(eval1, levels = hicklin_conclusion_levels, ordered = T)) |>
  mutate(eval2 = factor(eval2, levels = hicklin_conclusion_levels, ordered = T))

#### -------------------------- Reproducibility -------------------------- ####
hicklin_reproducibility <- hicklin_reproducibility_orig |>
  filter(!str_detect(eval1, "Total")) |>
  pivot_longer(ID:EL, names_to = "eval2", values_to = "Count") |>
  mutate(eval2 = str_replace(eval2, "\\.", "-")) |>
  mutate(across(Polygonal:SameAmmo, ~.=="T")) |>
  mutate(eval1 = factor(eval1, levels = hicklin_conclusion_levels, ordered = T)) |>
  mutate(eval2 = factor(eval2, levels = hicklin_conclusion_levels, ordered = T))


hicklin_reproducibility_safe <- hicklin_reproducibility_orig |>
  filter(!str_detect(eval1, "Total")) |>
  pivot_longer(ID:EL, names_to = "eval2", values_to = "Count") |>
  mutate(eval2 = str_replace(eval2, "\\.", "-")) |>
  mutate(across(Polygonal:SameAmmo, ~.=="T")) |>
  filter(!Polygonal & SameModel & SameAmmo) |>
  mutate(eval1 = factor(eval1, levels = hicklin_conclusion_levels, ordered = T)) |>
  mutate(eval2 = factor(eval2, levels = hicklin_conclusion_levels, ordered = T))

#### -------------------------- Accuracy -------------------------- ####
hicklin_accuracy <- hicklin_accuracy_orig |>
  select(-matches("Total")) |>
  pivot_longer(ID:US, names_to="Eval", values_to="Count") |>
  group_by(ComparisonType, Source) |>
  mutate(Total = sum(Count), comparison_pct = Count/Total*100) |>
  mutate(across(Polygonal:SameGunModel, ~.=="T"))


hicklin_accuracy_sens_spec <- hicklin_accuracy |>
  filter(!Eval %in% c("US", "Other")) |>
  mutate(error = !((Source == "SS" & Eval == "ID") | (Source == "DS" & Eval == "EL"))) |>
  group_by(ComparisonType, Source) |>
  summarize(rate = sum(Count*error)/sum(Count) * 100)
hicklin_accuracy_sens <- filter(hicklin_accuracy_sens_spec, Source == "SS")
hicklin_accuracy_spec <- filter(hicklin_accuracy_sens_spec, Source == "DS")


hicklin_accuracy_fp_fn <- hicklin_accuracy |>
  filter(!Eval %in% c("US", "Other")) |>
  mutate(error = ((Source == "SS" & Eval == "EL") | (Source == "DS" & Eval == "ID"))) |>
  group_by(ComparisonType, Source) |>
  summarize(rate = sum(Count*error)/sum(Count) * 100)
hicklin_accuracy_fn <- filter(hicklin_accuracy_fp_fn, Source == "SS")
hicklin_accuracy_fp <- filter(hicklin_accuracy_fp_fn, Source == "DS")

hicklin_accuracy_ppv_npv <- hicklin_accuracy |>
  filter(!Eval %in% c("US", "Other") & !str_detect(Eval, "INC")) |>
  mutate(error = ((Source == "SS" & Eval == "ID") | (Source == "DS" & Eval == "EL"))) |>
  group_by(ComparisonType, Eval) |>
  summarize(rate = sum(Count*error)/sum(Count) * 100)
hicklin_accuracy_ppv <- filter(hicklin_accuracy_ppv_npv, Eval == "ID")
hicklin_accuracy_npv <- filter(hicklin_accuracy_ppv_npv, Eval == "EL")


