library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggh4x)
library(RColorBrewer)
library(forcats)

ames2_orig <- read_csv("data/AmesII-repeatability.csv", skip = 1, name_repair = make.names)

conclusion_levels =  c("ID", "INC-A", "INC-B", "INC-C", "EL")

inc_same <- c("ID", "INC", "EL") 

suitable_check = function(x) x != "US"
conclusion_check = function(x) if_else(x == "US", NA, x) |> factor(levels = conclusion_levels, ordered = T)

res_labels <- c("Un/Suitable", "ID to EL", "3 closer to EL", "2 closer to EL", 
                "1 closer to EL", "Same", "1 closer to ID", 
                "2 closer to ID", "3 closer to ID", "EL to ID") |> 
  setNames(object = c(-5:4))

res_labels_inc_same <- c("Un/Suitable",
  "ID to EL", "ID to INC", "Same", 
  "EL to INC", "EL to ID") |> 
  setNames(object = c(-3:2))

res_scale <- c("grey", brewer.pal(9, "RdBu"))
res_scale_inc_same <- c("grey", brewer.pal(5, "RdBu"))

ames2 <- ames2_orig |> filter(!str_detect(X1st.eval, "Total")) |>
  pivot_longer(ID:Total.US, names_to = "X2nd.eval", values_to = "Count") |>
  filter(!str_detect(X2nd.eval, "Total")) |>
  rename(eval1 = X1st.eval, eval2 = X2nd.eval) |>
  mutate(eval2 = str_replace(eval2, "\\.", "-"))


ames2_suitable <- ames2 |>
  mutate(across(matches("eval"), ~suitable_check(.))) |>
  group_by(Type, Comparison, eval1, eval2) |>
  summarize(Count = sum(Count)) |> 
  ungroup() |>
  mutate(concordant = eval1==eval2) |>
  select(-c(eval1, eval2)) |>
  pivot_wider(id_cols = c(Type:Comparison), names_from = concordant, values_fn = sum, values_from = Count) |>
  rename(Concordant = `TRUE`, Discordant = `FALSE`) |>
  mutate(repeatable_rate = Concordant/(Concordant+Discordant))


ames2_agreement <- ames2 |>
  mutate_at(
    vars(matches("eval")), ~factor(., levels = conclusion_levels, ordered = T)) |>
  na.omit() |>
  mutate(res = as.numeric(eval1) - as.numeric(eval2)) 

ames2_agreement_sum <- ames2_agreement |>
  group_by(Type, Comparison, res) |>
  summarize(Count = sum(Count)) |>
  bind_rows(
    select(ames2_suitable, Type, Comparison, Count = Discordant) |>
      mutate(res = -5)
  ) |>
  arrange(Type, Comparison, res) |>
  mutate(res = factor(res, levels = res_labels, labels = names(res_labels), ordered = T)) |>
  group_by(Type, Comparison) |>
  mutate(Prop = Count/sum(Count)) |>
  ungroup()


ggplot(ames2_agreement_sum, aes(y = 1, weight = Prop, fill = res)) + 
  stat_count() + geom_bar(color = "black") +
  facet_nested(Comparison + Type ~., switch = "y") +
  scale_x_continuous("Proportion of Responses", minor_breaks = seq(0, 10)/10)  + 
  scale_fill_manual("Rating Change", values = res_scale) +
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.byrow = T) 
  
ames2_agreement_inc_same <- ames2 |>
  mutate_at(
    vars(matches("eval")), \(x) factor(x, levels = conclusion_levels, ordered = T) |>
      fct_collapse(INC = paste("INC", LETTERS[1:3], sep = "-"))
      ) |>
  na.omit() |>
  group_by(Type, Comparison, eval1, eval2) |>
  summarize(Count = sum(Count)) |>
  ungroup() |>
  mutate(res = as.numeric(eval1) - as.numeric(eval2)) |>
  bind_rows(
    select(ames2_suitable, Type, Comparison, Count = Discordant) |>
      mutate(res = -3)
  ) |>
  arrange(Type, Comparison, res) |>
  mutate(res = factor(res, levels = res_labels_inc_same, 
                      labels = names(res_labels_inc_same), 
                      ordered = T)) |>
  group_by(Type, Comparison) |>
  mutate(Prop = Count/sum(Count)) |>
  ungroup()


ggplot(ames2_agreement_inc_same, aes(y = 1, weight = Prop, fill = res)) + 
  stat_count() + geom_bar(color = "black") +
  facet_nested(Type + Comparison~., switch = "y") +
  scale_x_continuous("Proportion of Responses", minor_breaks = seq(0, 10)/10)  + 
  scale_fill_manual("Rating Change", values = res_scale_inc_same) +
  theme(axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.byrow = T) 


library(ggpcp)
