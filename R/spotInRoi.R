# Parse data and mRcrd helpers

#' Read data for spotInRoi analysis of the Puncta Tracker ImageJ plugin.
#'
#' @param file File passed to `readr::read_csv`.
#' @param parse.spotOut Whether to parse spotOut array data.
#' @param ... Other parameters passed to `readr::read_csv`.
#'
#' @return `tibble` of the spotInRoi analysis result.
#' @export
#'
#' @examples
#' # TODO
spotInRoi_read_csv <- function(file, parse.spotOut = FALSE, ...){
  # Read in csv
  df <- readr::read_csv(
    file,
    col_names =
      c("idx", "tp", "meas", "plane", "equi.D", "spot.count",
        "spotIn.coord", "spotIn.dist",
        "spotOut.coord", "spotOut.dist"),
    col_types = "iciidicccc", skip = 1, ...
  )
  # Check if there is any cell that has 0 spot
  sel_0 <- df$spot.count == 0
  if (any(sel_0)){
    rlang::warn(sprintf(
      "%d out of %d cells record NO spot in ROI. These are removed from analysis.",
      sum(sel_0), length(sel_0)))
  }
  # Remove 0 spot cells
  df <- df[!sel_0,]
  # Parse spotIn array data
  spotIn <- lapply(
    1:nrow(df),
    \(idx) .parse_meta(df$spotIn.coord[idx], df$spotIn.dist[idx]))
  df$spotIn <- spotIn
  # Optionally parse spotOut array data
  if (parse.spotOut){
    spotOut <- lapply(
      1:nrow(df),
      \(idx) .parse_meta(df$spotOut.coord[idx], df$spotOut.dist[idx]))
    df$spotOut <- spotOut
  }

  # Remove the pre-parse columns
  df$spotIn.coord <- df$spotOut.coord <- df$spotIn.dist <- df$spotOut.dist <- NULL

  return(df)
}

#' Parse spot array JSON data to metaRcrd.
#'
#' @param ser.coord Serialized coord.
#' @param ser.dist Serialized dist.
#'
#' @return A `y3628::metaRcrd` object.
#'
#' @examples
#' # Does not apply. Internal use only.
.parse_meta <- function(ser.coord, ser.dist){

  # Helpers
  ser.array <- function(x){
    stringr::str_replace_all(x, "\\(", "[") |>
      stringr::str_replace_all("\\)", "]")
  }
  fromJSON <- \(x) {ser.array(x) |> jsonlite::fromJSON()}

  # Parse coord
  coord <- fromJSON(ser.coord)
  colnames(coord) <- c("x", "y", "z")
  coord <- tibble::as_tibble(coord)
  coord$idx <- 1:nrow(coord)
  coord$dist <- fromJSON(ser.dist)

  # Construct
  y3628::metaRcrd(coord, meta.fields = c("idx", "dist"))
}
