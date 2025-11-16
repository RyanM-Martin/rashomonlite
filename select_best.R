select_best <- function(models_df, alpha = 0.5) {
  nsel <- max(1, floor(alpha * nrow(models_df)))
  models_df[order(models_df$aic)][seq_len(nsel), ]
}
