# Pareto utilities for multi-objective optimization

#' Check if one solution dominates another
#'
#' @param a Numeric vector of scores for solution A
#' @param b Numeric vector of scores for solution B
#'
#' @return Logical; TRUE if A dominates B
#' @noRd
pareto_dominates <- function(a, b) {
  a <- as.numeric(a)
  b <- as.numeric(b)

  a[is.na(a)] <- -Inf
  b[is.na(b)] <- -Inf

  all(a >= b) && any(a > b)
}

#' Find non-dominated solutions (Pareto frontier)
#'
#' @param scores Matrix or data frame of scores (rows = solutions, cols = metrics)
#'
#' @return Integer vector of indices on the Pareto frontier
#' @noRd
pareto_frontier <- function(scores) {
  scores <- as.matrix(scores)
  n <- nrow(scores)

  if (n == 0) {
    return(integer(0))
  }

  dominated <- rep(FALSE, n)

  for (i in seq_len(n)) {
    if (dominated[i]) {
      next
    }
    for (j in seq_len(n)) {
      if (i == j) {
        next
      }
      if (pareto_dominates(scores[j, ], scores[i, ])) {
        dominated[i] <- TRUE
        break
      }
    }
  }

  which(!dominated)
}

#' Compute Pareto ranks for a set of solutions
#'
#' @param scores Matrix or data frame of scores (rows = solutions, cols = metrics)
#'
#' @return Integer vector of Pareto ranks (1 = best front)
#' @noRd
pareto_ranks <- function(scores) {
  scores <- as.matrix(scores)
  n <- nrow(scores)

  if (n == 0) {
    return(integer(0))
  }

  ranks <- rep(NA_integer_, n)
  remaining <- seq_len(n)
  current_rank <- 1L

  while (length(remaining) > 0) {
    front <- pareto_frontier(scores[remaining, , drop = FALSE])
    if (length(front) == 0) {
      ranks[remaining] <- current_rank
      break
    }
    front_indices <- remaining[front]
    ranks[front_indices] <- current_rank
    remaining <- setdiff(remaining, front_indices)
    current_rank <- current_rank + 1L
  }

  ranks
}

#' Compute crowding distance for Pareto fronts
#'
#' @param scores Matrix or data frame of scores (rows = solutions, cols = metrics)
#' @param ranks Integer vector of Pareto ranks
#'
#' @return Numeric vector of crowding distances (higher is better)
#' @noRd
pareto_crowding_distance <- function(scores, ranks) {
  scores <- as.matrix(scores)
  n <- nrow(scores)
  m <- ncol(scores)

  if (n == 0) {
    return(numeric(0))
  }

  distances <- rep(0, n)
  fronts <- split(seq_len(n), ranks)

  for (front_indices in fronts) {
    if (length(front_indices) <= 2) {
      distances[front_indices] <- Inf
      next
    }

    front_scores <- scores[front_indices, , drop = FALSE]
    for (k in seq_len(m)) {
      metric_values <- front_scores[, k]
      order_idx <- order(metric_values, na.last = TRUE)
      sorted_indices <- front_indices[order_idx]

      distances[sorted_indices[1]] <- Inf
      distances[sorted_indices[length(sorted_indices)]] <- Inf

      range_val <- max(metric_values, na.rm = TRUE) -
        min(metric_values, na.rm = TRUE)

      if (is.na(range_val) || range_val == 0) {
        next
      }

      for (i in 2:(length(sorted_indices) - 1)) {
        prev_val <- metric_values[order_idx[i - 1]]
        next_val <- metric_values[order_idx[i + 1]]
        if (is.na(prev_val) || is.na(next_val)) {
          next
        }
        distances[sorted_indices[i]] <- distances[sorted_indices[i]] +
          (next_val - prev_val) / range_val
      }
    }
  }

  distances
}
