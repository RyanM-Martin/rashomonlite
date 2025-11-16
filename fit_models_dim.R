# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

fit_models_dim <- function(X, y, k, m) {
  vars_all <- colnames(X)
  combs <- combn(vars_all, k, simplify = FALSE)
  combs <- combs[seq_len(min(length(combs), m))]

  out <- lapply(combs, function(vs) {
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


select_best <- function(models_df, alpha = 0.5) {
  nsel <- max(1, floor(alpha * nrow(models_df)))
  models_df[order(models_df$aic)][seq_len(nsel), ]
}


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

