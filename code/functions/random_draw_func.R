
# function used to generate a random sample of key values given a seed
random_draw <- function(input_seed) {
  
  set.seed(input_seed)
  
  tibble(
    X = 2 * rbeta(2, 4, n = num_reps * samp_size) - 1,
    U = rnorm(mean = 0, sd = 0.1295, n = num_reps * samp_size)
  ) |>
  infer::rep_sample_n(
    size = samp_size,
    reps = num_reps,
    replace = F
  ) |>
  ungroup() |>
  mutate(seed_id = rep(seed, num_reps * samp_size))
  
}