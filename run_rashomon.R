

run_rashomon <- function(X, y, pmax = 5, m = 90, alpha = 0.5) {
  all_vars <- colnames(X)
  M <- list()
  M[[1]] <- fit_models_dim(X, y, 1, m)
  best_df <- select_best(M[[1]], alpha)
  for (k in 2:pmax) {
    grown <- grow_from(best_df, all_vars, k_next = k, m = m)
    M[[k]] <- grown
    best_df <- select_best(grown, alpha)
  }
  count_list <- lapply(M, function(df) {
    tab <- table(unlist(df$vars))
    data.frame(var = names(tab), count = as.integer(tab))
  })
  aic_list <- lapply(seq_along(M), function(k) {
    data.frame(k = k, aic = M[[k]]$aic)
  })
  aic_df <- do.call(rbind, aic_list)

  list(
    M = M,
    count_by_dim = count_list,
    aic_by_dim = aic_df
  )
}
