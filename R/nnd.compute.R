# Function for NND computation

#' Compute NND metrics for a pair of spot sets.
#'
#' @param foreach For each spot in `foreach`,
#' @param to compute its distance to the nearest neighbor in `to` spot set.
#' @param z.proximal Only spots in close enough z-proximity is computed.
#'
#' @return `y3628::metaRcrd` object storing the NND results. For spots that have
#' no Z-proximal spots in the `to` spot set, NAs are returned.
#' @export
#'
#' @examples
#' # TODO
ij_nnd.compute <- function(foreach, to, z.proximal){
  # nnd.compute(spotIn.x, spotIn.y)
  #   return <mRcrd [length(foreach)]>
  #     data field = f.x, f.y, t.x, t.y
  #     meta field = f.idx, t.idx, nnd

  # Get data
  f.size <- length(foreach)
  f.x <- vctrs::field(foreach, "x")
  f.y <- vctrs::field(foreach, "y")
  f.z <- vctrs::field(foreach, "z")
  f.idx <- vctrs::field(foreach, "idx")

  t.size <- length(to)
  t.x <- vctrs::field(to, "x")
  t.y <- vctrs::field(to, "y")
  t.z <- vctrs::field(to, "z")
  t.idx <- vctrs::field(to, "idx")

  # Loop to compute
  nnd.t.x <- rep(0, f.size)
  nnd.t.y <- rep(0, f.size)
  nnd.t.idx <- rep(0, f.size)
  nnd.val <- rep(0, f.size)
  for (i in seq(f.size)){
    # For each target, compute dist of xy
    dist <- rep(0, t.size)
    for (j in seq(t.size))
      dist[j] <- ((f.x[i] - t.x[j])^2 + (f.y[i] - t.y[j])^2)
    dist <- sqrt(dist)
    # Filter out targets NOT in z proximity
    dist_z <- abs(f.z[i] - t.z)
    dist[dist_z > z.proximal] <- NA
    # Get nearest neighbor
    nnd.which <- which.min(dist)
    # Fetch field values of nearest neighbor
    if (length(nnd.which) == 0){
      # No spot in Z-proximity; all NAs after filtering
      nnd.t.x[i] <- NA_real_
      nnd.t.y[i] <- NA_real_
      nnd.t.idx[i] <- NA_integer_
      nnd.val[i] <- NA_real_
    } else{
      # Fetch the neighbor value
      nnd.t.x[i] <- t.x[nnd.which]
      nnd.t.y[i] <- t.y[nnd.which]
      nnd.t.idx[i] <- t.idx[nnd.which]
      nnd.val[i] <- min(dist, na.rm = TRUE)
    }
  }

  # Return
  result <- data.frame(
    f.x = f.x, f.y = f.y, t.x = nnd.t.x, t.y = nnd.t.y,
    f.idx = f.idx, t.idx = nnd.t.idx, nnd = nnd.val
  )
  result <- y3628::metaRcrd(
    result, meta.fields = c("f.idx", "t.idx", "nnd")
  )
  return(result)
}
