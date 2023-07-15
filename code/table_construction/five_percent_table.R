
# load libraries needed
pacman::p_load(pacman, dplyr, tibble, readr, tidyr)

# source functions needed
## ...

processed_data <- readr::read_csv("processed_data/processed_wald_stats.csv") |>
                  as_tibble()

# get critical values corresponding to each (tau, 0)
crit_vals <- processed_data |>
             filter(m == 0) |>
             group_by(tau) |>
             summarise(
               cv_05 = quantile(wald_stat, prob = 0.95)
             ) |>
             ungroup()

crit_val_func <- function(cv_df, t) {
  cv_df |>
    filter(tau == t) |>
    pull(cv_05) |>
    as.numeric()
}

processed_data |>
  ungroup() |>
  group_by(tau, m) |>
  summarise(
    Rejection_Rate = sum(as.numeric(wald_stat > crit_val_func(crit_vals, mean(as.numeric(tau))))) / n()
  ) |>
  ungroup() |>
  pivot_wider(
    names_from = m,
    values_from = Rejection_Rate
  ) |>
  rename(`Tau` = `tau`,
         `M=0` = `0`,
         `M=2` = `2`,
         `M=4` = `4`,
         `M=6` = `6`,
         `M=8` = `8`,
         `M=10` = `10`) |>
  select(Tau, `M=0`, `M=2`, `M=4`, `M=6`, `M=8`, `M=10`)
