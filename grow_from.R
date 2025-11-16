grow_from <- function(best_df, all_vars, k_next, m) {
  grown <- list()
  grown_keys <- character(0)
  for (i in seq_len(nrow(best_df))) {
    base_vars <- best_df$vars[[i]]

    addable <- setdiff(all_vars, base_vars)
    for (v in addable) {
      new_vars <- sort(c(base_vars, v))
      key <- paste(new_vars, collapse = ",")

      if (!(key %in% grown_keys)) {
        grown[[length(grown) + 1]] <- new_vars
        grown_keys <- c(grown_keys, key)
      }
      if (length(grown) >= m) break
    }
    if (length(grown) >= m) break
  }
  out <- lapply(grown, function(vs) {
    df <- data.frame(y = y, X[, vs, drop = FALSE])
    fit <- glm(y ~ ., data = df, family = binomial)
    list(
      vars  = vs,
      aic   = AIC(fit),
      coef  = coef(fit),
      pvals = summary(fit)$coefficients[, 4]
    )
  })
  data.frame(
    vars  = I(lapply(out, `[[`, "vars")),
    aic   = sapply(out, `[[`, "aic"),
    coef  = I(lapply(out, `[[`, "coef")),
    pvals = I(lapply(out, `[[`, "pvals")),
    stringsAsFactors = FALSE
  )
}
