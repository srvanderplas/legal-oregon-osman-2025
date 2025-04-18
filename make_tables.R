library(knitr)
library(kableExtra)
library(dplyr)
library(tidyr)
library(purrr)
library(confintr)


make_ci_tbl <- function(x, n){
  if (n > 10 & n >= x) {
    res <- ci_proportion(x, n)
    data.frame(Estimate = res$estimate, CI_LB = res$interval[1], CI_UB = res$interval[2])
  } else {
    data.frame(Estimate = NA, CI_LB = NA, CI_UB = NA)
  }
}


make_repeat_ci <- function(tbl) {
  tbl|>
    mutate(tbl = map2(x, n, make_ci_tbl)) |>
    unnest(tbl) |>
    select(-c(x, n))
}

make_csdr_ci <- function(tbl) {
  nest(tbl, data = -c(x, n))|>
    mutate(ci = map2(x, n, make_ci_tbl)) |>
    unnest(c(data, ci)) |>
    select(-c(csd, Count, x))
}

format_ci_tbl <- function(tbl, fmt="%0.2f") {
  stopifnot(all(c("Estimate", "CI_LB", "CI_UB") %in% names(tbl)))
  est_str <- sprintf("%s%%%% (%s, %s)", fmt, fmt, fmt)
  tbl |>
    mutate(est_ci = sprintf(est_str, Estimate*100, CI_LB*100, CI_UB*100)|>
             str_replace("^NA.*$", "---")) |>
    select(-c(Estimate, CI_LB, CI_UB))
}

add_overall_row <- function(df, ...) {
  bind_rows(
    df,
    summarize(df, ...)
  )
}

cdl_tbl_opts <- c("Overall", "ID", "INC-A", "INC-B", "INC-C", "EL")
