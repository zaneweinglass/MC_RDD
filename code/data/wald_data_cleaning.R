
# load libraries needed
pacman::p_load(pacman, dplyr, tibble, readr, tidyr)

# generated data
wald_data <- readr::read_csv("generated_data/sim_wald_stats.csv") |>
             as_tibble()

# tau values
tau_1 <- c("tm_1_0", "tm_1_2", "tm_1_4", "tm_1_6", "tm_1_8", "tm_1_10", "tm_1_inf")
tau_2 <- c("tm_2_0", "tm_2_2", "tm_2_4", "tm_2_6", "tm_2_8", "tm_2_10", "tm_2_inf")
tau_3 <- c("tm_3_0", "tm_3_2", "tm_3_4", "tm_3_6", "tm_3_8", "tm_3_10", "tm_3_inf")
tau_4 <- c("tm_4_0", "tm_4_2", "tm_4_4", "tm_4_6", "tm_4_8", "tm_4_10", "tm_4_inf")
tau_5 <- c("tm_5_0", "tm_5_2", "tm_5_4", "tm_5_6", "tm_5_8", "tm_5_10", "tm_5_inf")
tau_6 <- c("tm_6_0", "tm_6_2", "tm_6_4", "tm_6_6", "tm_6_8", "tm_6_10", "tm_6_inf")
tau_7 <- c("tm_7_0", "tm_7_2", "tm_7_4", "tm_7_6", "tm_7_8", "tm_7_10", "tm_7_inf")
tau_8 <- c("tm_8_0", "tm_8_2", "tm_8_4", "tm_8_6", "tm_8_8", "tm_8_10", "tm_8_inf")

# m values
m_0 <- c("tm_1_0", "tm_2_0", "tm_3_0", "tm_4_0", "tm_5_0", "tm_6_0", "tm_7_0", "tm_8_0")
m_2 <- c("tm_1_2", "tm_2_2", "tm_3_2", "tm_4_2", "tm_5_2", "tm_6_2", "tm_7_2", "tm_8_2")
m_4 <- c("tm_1_4", "tm_2_4", "tm_3_4", "tm_4_4", "tm_5_4", "tm_6_4", "tm_7_4", "tm_8_4")
m_6 <- c("tm_1_6", "tm_2_6", "tm_3_6", "tm_4_6", "tm_5_6", "tm_6_6", "tm_7_6", "tm_8_6")
m_8 <- c("tm_1_8", "tm_2_8", "tm_3_8", "tm_4_8", "tm_5_8", "tm_6_8", "tm_7_8", "tm_8_8")
m_10 <- c("tm_1_10", "tm_2_10", "tm_3_10", "tm_4_10", "tm_5_10", "tm_6_10", "tm_7_10", "tm_8_10")
m_inf <- c("tm_1_inf", "tm_2_inf", "tm_3_inf", "tm_4_inf", "tm_5_inf", "tm_6_inf", "tm_7_inf", "tm_8_inf")

# reformat data
wald_data <- wald_data |>
             pivot_longer(
               cols = tm_1_0:tm_8_inf,
               names_to = "tau_m",
               values_to = "wald_stat"
             ) |>
             mutate(
               tau = case_when(
                 tau_m %in% tau_1 ~ "0.01",
                 tau_m %in% tau_2 ~ "0.02",
                 tau_m %in% tau_3 ~ "0.03",
                 tau_m %in% tau_4 ~ "0.04",
                 tau_m %in% tau_5 ~ "0.05",
                 tau_m %in% tau_6 ~ "0.06",
                 tau_m %in% tau_7 ~ "0.07",
                 tau_m %in% tau_8 ~ "0.08"
               ),
               m = case_when(
                 tau_m %in% m_0 ~ "0",
                 tau_m %in% m_2 ~ "2",
                 tau_m %in% m_4 ~ "4",
                 tau_m %in% m_6 ~ "6",
                 tau_m %in% m_8 ~ "8",
                 tau_m %in% m_10 ~ "10",
                 tau_m %in% m_inf ~ "inf",
               )
             ) |>
             select(tau, m, wald_stat) |>
             arrange(tau, m)

# store processed data
file.create("processed_data/processed_wald_stats.csv")
readr::write_csv(wald_data, file = "processed_data/processed_wald_stats.csv")
