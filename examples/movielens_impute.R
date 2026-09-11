library("tidyverse")
library("data.table")
library("ggplot2")

##### data preprocessing to convert to tabular matrix of ratings 
#### movies by column, users by row
ratings <- fread(text = gsub("::", "\t", 
                             readLines("data/ml-10M100K/ratings.dat")),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines("data/ml-10M100K/movies.dat"), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>%
  mutate(movieId = as.numeric(movieId),
         title = as.character(title),
         genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

rating.matrix <- as.matrix(pivot_wider(movielens, 
                                       id_cols = "userId", 
                                       names_from = "movieId", 
                                       values_from = "rating"))
colnames(rating.matrix)[-1] <- movies$title[match(as.numeric(colnames(rating.matrix)[-1]),movies$movieId)]

#### SVDimpute framework using IRLBA

library(RSpectra)
library(Matrix)

svd_impute <- function(X, k, max_iter = 100, tol = 1e-5,
                       svd_tol = 1e-4, svd_maxitr = 100) {
  
  n <- nrow(X)
  p <- ncol(X)
  
  # Extract observed entries
  obs_i    <- which(!is.na(X), arr.ind = TRUE)
  obs_vals <- as.numeric(X[obs_i])
  obs_row  <- obs_i[, 1L]
  obs_col  <- obs_i[, 2L]
  
  rm(X); gc()
  
  # Build sparse matrix of observed entries
  X_sp <- sparseMatrix(
    i    = obs_row,
    j    = obs_col,
    x    = obs_vals,
    dims = c(n, p)
  )
  rm(obs_row, obs_col); gc()
  
  # Expand dgCMatrix column pointers to per-entry column indices
  expand_col_ptrs <- function(ptr) rep(seq_along(diff(ptr)), diff(ptr))
  sp_i <- X_sp@i + 1L
  sp_j <- expand_col_ptrs(X_sp@p)
  
  # Sanity check
  stopifnot(all.equal(as.numeric(X_sp[cbind(sp_i, sp_j)]), obs_vals))
  
  # Initialize U, d, V randomly
  set.seed(42)
  U <- matrix(rnorm(n * k, sd = 0.01), n, k)
  V <- matrix(rnorm(p * k, sd = 0.01), p, k)
  d <- rep(1, k)
  
  losses <- numeric(max_iter)
  n_iter <- max_iter
  
  for (iter in seq_len(max_iter)) {
    
    # Current approximation at observed entries
    UD     <- sweep(U, 2, d, `*`)
    UV_obs <- rowSums(UD[sp_i, , drop = FALSE] * V[sp_j, , drop = FALSE])
    
    # Sparse residual R = P_obs(X - UDV')
    R_sp <- sparseMatrix(
      i    = sp_i,
      j    = sp_j,
      x    = as.numeric(obs_vals - UV_obs),
      dims = c(n, p)
    )
    
    # A = UDV' + R_sp (never materialized)
    # A  %*% x = U(D(V'x)) + R_sp %*% x
    # A' %*% x = V(D(U'x)) + R_sp' %*% x
    Afun <- function(x, args) {
      as.numeric(U %*% (d * crossprod(V, x)) + R_sp %*% x)
    }
    Atfun <- function(x, args) {
      as.numeric(V %*% (d * crossprod(U, x)) + crossprod(R_sp, x))
    }
    
    # Rank-k SVD via RSpectra
    s <- svds(Afun, k = k, nu = k, nv = k, Atrans = Atfun, args = NULL,
              dim = c(n, p),
              opts = list(tol = svd_tol, maxitr = svd_maxitr))
    
    # Store U, d, V separately
    U <- s$u
    d <- s$d
    V <- s$v
    
    # RMSE on observed entries
    UD_new       <- sweep(U, 2, d, `*`)
    UV_obs_new   <- rowSums(UD_new[sp_i, , drop = FALSE] * V[sp_j, , drop = FALSE])
    losses[iter] <- sqrt(mean((UV_obs_new - obs_vals)^2))
    
    cat(sprintf("Iter %d | RMSE: %.6f\n", iter, losses[iter]))
    
    if (iter > 1 && abs(losses[iter - 1L] - losses[iter]) < tol) {
      n_iter <- iter
      losses <- losses[seq_len(n_iter)]
      break
    }
    
    if (iter == max_iter) losses <- losses[seq_len(max_iter)]
  }
  
  list(
    U      = U,
    d      = d,
    V      = V,
    losses = losses,
    n_iter = n_iter
  )
}
#### filter by movies/users  with sufficient ratings
#### run and view results

min_ratings_per_movie <- 1000  # tune this
min_ratings_per_user <- 50
keep_movies <- colSums(!is.na(rating.matrix)) >= min_ratings_per_movie
keep_users  <- rowSums(!is.na(rating.matrix)) >= min_ratings_per_user
rating.matrix.filtered <- rating.matrix[keep_users, keep_movies]

fit <- svd_impute(rating.matrix.filtered[,-1], k = 50, max_iter = 200)



plot_svd_impute_loss <- function(result) {
  df <- data.frame(
    iteration = seq_along(result$losses),
    loss      = result$losses
  )
  
  ggplot(df, aes(x = iteration, y = loss)) +
    geom_line(colour = "steelblue") +
    geom_point(colour = "steelblue") +
    labs(
      title    = sprintf("SVDimpute convergence  [k=%d, %d iters]",
                         ncol(result$svd$v), result$n_iter),
      x        = "Iteration",
      y        = "RMSE (observed entries)"
    ) +
    theme_minimal()
}
plot_svd_impute_loss(fit)


recommend <- function(fit, item_names = NULL, query_item, top_n = 10) {
  
  # Resolve character query to integer index
  if (is.character(query_item)) {
    if (is.null(item_names))
      stop("item_names must be provided when query_item is a character string")
    idx <- match(query_item, item_names)
    if (is.na(idx))
      stop(sprintf("'%s' not found in item_names", query_item))
    query_item <- idx
  }
  
  # V is p x k, rows are items in latent space
  # Scale by singular values so dimensions are weighted by importance
  V_scaled <- sweep(fit$V, 2, fit$d, `*`)  # p x k
  
  # Cosine similarity between query item and all others
  query_vec  <- V_scaled[query_item, ]
  norms      <- sqrt(rowSums(V_scaled^2))
  query_norm <- norms[query_item]
  
  sims <- as.numeric(V_scaled %*% query_vec) / (norms * query_norm)
  
  # Exclude the query item itself
  sims[query_item] <- -Inf
  
  # Top N most similar
  top_idx <- order(sims, decreasing = TRUE)[seq_len(top_n)]
  
  result <- data.frame(
    item       = if (!is.null(item_names)) item_names[top_idx] else top_idx,
    similarity = round(sims[top_idx], 4)
  )
  
  rownames(result) <- NULL
  result
}

recommend_unscaled <- function(fit, item_names = NULL, query_item, top_n = 10,
                      item_counts = NULL, min_count = 0) {
  
  if (is.character(query_item)) {
    if (is.null(item_names))
      stop("item_names must be provided when query_item is a character string")
    idx <- match(query_item, item_names)
    if (is.na(idx))
      stop(sprintf("'%s' not found in item_names", query_item))
    query_item <- idx
  }
  
  # Use raw V (orthonormal) rather than scaling by d
  # With high missingness, d scaling amplifies popularity bias
  V_scaled   <- fit$V   # p x k, rows already unit norm
  query_vec  <- V_scaled[query_item, ]
  norms      <- sqrt(rowSums(V_scaled^2))
  query_norm <- norms[query_item]
  
  sims <- as.numeric(V_scaled %*% query_vec) / (norms * query_norm)
  
  sims[query_item] <- -Inf
  
  if (!is.null(item_counts) && min_count > 0)
    sims[item_counts < min_count] <- -Inf
  
  top_idx <- order(sims, decreasing = TRUE)[seq_len(top_n)]
  
  data.frame(
    item       = if (!is.null(item_names)) item_names[top_idx] else top_idx,
    similarity = round(sims[top_idx], 4),
    n_ratings  = if (!is.null(item_counts)) item_counts[top_idx] else NA,
    row.names  = NULL
  )
}

recommend(fit, item_names = colnames(rating.matrix.filtered)[-1], query_item = "Toy Story (1995)", top_n = 10)
recommend_unscaled(fit, item_names = colnames(rating.matrix.filtered)[-1], query_item = "Toy Story (1995)", top_n = 10)

save(rating.matrix.filtered, recommend, recommend_unscaled, plot_svd_impute_loss, fit, file = "movielens_impute_and_recommend.Rdata")