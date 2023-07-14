
# function that merges multiple samples
sample_merge <- function(input_samples, sample_size) {
  input_samples |>
    bind_rows() |>
    select(seed_id, replicate, X, U) |>
    rename(rep_id = replicate) |>
    group_by(seed_id, rep_id) |>
    summarise(obs_id = 1:sample_size,
              X = X,
              U = U)
}