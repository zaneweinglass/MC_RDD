
# load libraries needed
pacman::p_load(pacman, dplyr, tibble, readr, infer, rdrobust, purrr, furrr, parallelly, tictoc)

# source functions needed
source("code/functions/random_draw_func.R")
source("code/functions/compute_wald_func.R")
source("code/functions/perform_and_aggregate_wald_func.R")
source("code/functions/merge_samples_func.R")

# Key Referenced Values
num_reps <- 157
samp_size <- 500
seeds <- 1:64

# create list of 10,048 samples (157 from each of the 64 seeds)
samples <- map(seeds, random_draw) %>%
           sample_merge(., samp_size) %>%
           group_split()

# set up parallel computing
n_workers <- as.numeric(parallelly::availableCores())
plan("multisession", workers = n_workers)

# perform and aggregate Wald stats (~ 30min run time)
tic()
sim_results <- furrr::future_map(samples, perform_and_aggregate)
toc()

# deconstruct parallel computing
plan("sequential")

# merge results
sim_results <- sim_results |>
               bind_rows()

# store results
file.create("generated_data/sim_wald_stats.csv")
readr::write_csv(sim_results, file = "generated_data/sim_wald_stats.csv")
