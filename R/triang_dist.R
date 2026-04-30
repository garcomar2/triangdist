check_params <- function(min, max, mode) {
  if (any(min >= max)) {
    stop("min must be smaller than max.", call. = FALSE)
  }

  if (any(mode < min | mode > max)) {
    stop("mode must be between min and max.", call. = FALSE)
  }
}

recycle_args <- function(...) {
  args <- list(...)
  len <- max(lengths(args))
  lapply(args, rep_len, length.out = len)
}

#' Density of the Triangular Distribution
#'
#' Computes the density of the triangular distribution.
#'
#' @param x Numeric vector of quantiles.
#' @param min Numeric vector of lower limits.
#' @param max Numeric vector of upper limits.
#' @param mode Numeric vector of modes.
#'
#' @return A numeric vector with density values.
#' @export
#'
#' @examples
#' dtriang(0.5, min = 0, max = 1, mode = 0.5)
dtriang <- function(x, min, max, mode) {
  args <- recycle_args(x = x, min = min, max = max, mode = mode)

  x <- args$x
  min <- args$min
  max <- args$max
  mode <- args$mode

  check_params(min, max, mode)

  density <- numeric(length(x))

  # Parte creciente de la densidad
  left <- x >= min & x <= mode

  # Parte decreciente de la densidad
  right <- x > mode & x <= max

  density[left & mode != min] <- 2 * (x[left & mode != min] -
                                        min[left & mode != min]) /
    ((max[left & mode != min] - min[left & mode != min]) *
       (mode[left & mode != min] - min[left & mode != min]))

  density[right & mode != max] <- 2 * (max[right & mode != max] -
                                         x[right & mode != max]) /
    ((max[right & mode != max] - min[right & mode != max]) *
       (max[right & mode != max] - mode[right & mode != max]))

  # Caso especial: mode coincide con min
  density[x == min & mode == min] <- 2 / (max[x == min & mode == min] -
                                            min[x == min & mode == min])

  # Caso especial: mode coincide con max
  density[x == max & mode == max] <- 2 / (max[x == max & mode == max] -
                                            min[x == max & mode == max])

  density
}

#' Distribution Function of the Triangular Distribution
#'
#' Computes the cumulative distribution function of the triangular distribution.
#'
#' @param q Numeric vector of quantiles.
#' @param min Numeric vector of lower limits.
#' @param max Numeric vector of upper limits.
#' @param mode Numeric vector of modes.
#'
#' @return A numeric vector with cumulative probabilities.
#' @export
#'
#' @examples
#' ptriang(0.5, min = 0, max = 1, mode = 0.5)
ptriang <- function(q, min, max, mode) {
  args <- recycle_args(q = q, min = min, max = max, mode = mode)

  q <- args$q
  min <- args$min
  max <- args$max
  mode <- args$mode

  check_params(min, max, mode)

  probability <- numeric(length(q))

  probability[q >= max] <- 1

  # Parte izquierda de la función de distribución
  left <- q > min & q <= mode

  # Parte derecha de la función de distribución
  right <- q > mode & q < max

  probability[left & mode != min] <- ((q[left & mode != min] -
                                         min[left & mode != min])^2) /
    ((max[left & mode != min] - min[left & mode != min]) *
       (mode[left & mode != min] - min[left & mode != min]))

  probability[right & mode != max] <- 1 -
    ((max[right & mode != max] - q[right & mode != max])^2) /
      ((max[right & mode != max] - min[right & mode != max]) *
       (max[right & mode != max] - mode[right & mode != max]))

  # Caso especial: mode coincide con min
  probability[right & mode == min] <- 1 -
    ((max[right & mode == min] - q[right & mode == min])^2) /
      (max[right & mode == min] - min[right & mode == min])^2

  # Caso especial: mode coincide con max
  probability[left & mode == max] <- ((q[left & mode == max] -
                                         min[left & mode == max])^2) /
    (max[left & mode == max] - min[left & mode == max])^2

  probability
}

#' Quantile Function of the Triangular Distribution
#'
#' Computes the quantile function of the triangular distribution.
#'
#' @param p Numeric vector of probabilities.
#' @param min Numeric vector of lower limits.
#' @param max Numeric vector of upper limits.
#' @param mode Numeric vector of modes.
#'
#' @return A numeric vector with quantiles.
#' @export
#'
#' @examples
#' qtriang(0.5, min = 0, max = 1, mode = 0.5)
qtriang <- function(p, min, max, mode) {
  args <- recycle_args(p = p, min = min, max = max, mode = mode)

  p <- args$p
  min <- args$min
  max <- args$max
  mode <- args$mode

  check_params(min, max, mode)

  if (any(p < 0 | p > 1)) {
    stop("p must be between 0 and 1.", call. = FALSE)
  }

  fc <- (mode - min) / (max - min)

  quantile <- numeric(length(p))

  # Rama izquierda de la inversa de la CDF
  left <- p <= fc

  # Rama derecha de la inversa de la CDF
  right <- p > fc

  quantile[left] <- min[left] +
    sqrt(p[left] * (max[left] - min[left]) * (mode[left] - min[left]))

  quantile[right] <- max[right] -
    sqrt((1 - p[right]) * (max[right] - min[right]) *
           (max[right] - mode[right]))

  quantile
}

#' Random Generation from the Triangular Distribution
#'
#' Generates random values from the triangular distribution using inverse
#' transform sampling.
#'
#' @param n Number of observations.
#' @param min Numeric vector of lower limits.
#' @param max Numeric vector of upper limits.
#' @param mode Numeric vector of modes.
#'
#' @return A numeric vector of random values.
#' @importFrom stats runif
#' @export
#'
#' @examples
#' rtriang(5, min = 0, max = 1, mode = 0.5)
rtriang <- function(n, min, max, mode) {
  qtriang(runif(n), min = min, max = max, mode = mode)
}
