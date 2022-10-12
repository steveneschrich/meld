

#' Identify duplicated column names from a join
#'
#' @description Identifies column names that exist as ending in both `.x`
#' and `.y`, indicating that they had the same column name during a dplyr
#' join.
#'
#' @details The purpose of this function is to identify column names that
#' have been joined from two tables, but have the same name. By default,
#' [dplyr::left_join()] and the like will add a `.x` and `.y` to the
#' name. Often, I've had the problem that I cannot join by them (because
#' they are incomplete). So I'd rather clean them up after the join (using
#' meld). This function identifies the column names that need to be cleaned
#' up.
#'
#' @param x A data frame
#' @param pattern The pattern to determine collisions.
#'
#' @return A list of column names (without extension) that are duplicated.
#' @export
#'
#' @examples
#' \dontrun{
#' identify_collisions(data.frame(A.x=c(1,2,3), A.y=c(2,3,4)))
#' }
identify_collisions <- function(x, pattern ="\\.[xy]$") {

  colnames(x) |>
    stringr::str_subset(pattern) |>
    stringr::str_remove(pattern) |>
    unique()
}

#' Meld column collisions in a data frame
#'
#' @description Combine columns that appear to be collisions (ending in .x and
#' .y) by melding them.
#'
#' @details When using dplyr, sometimes you do not want to join by a column because
#' the data may be partially missing. When doing this, both columns are in the result
#' but appended with `.x` and `.y`. This function will identify these column pairs
#' and [meld()] them into a single variable (without the `.x` or `.y` extension).
#'
#' Note that the side effect of [meld()] is to [base::stop()] if any values are not
#' matched (other than NA) in the two columns. This provides a safe way to combine
#' data if you are not sure about it's contents.
#'
#' @param x A data frame with (possibly) column names with collisions.
#'
#' @return A data frame with the repeated columns melded.
#' @export
#'
#' @examples
#' \dontrun{
#' meld_collisions(dplyr::left_join(iris, iris, by="Species"))
#' }
meld_collisions <- function(x) {

  collisions <- tidyr::expand_grid(
    vars = identify_collisions(x),
    extensions = c("x","y")
  ) |>
    tidyr::unite(col = "collisions",.data$vars, .data$extensions, sep=".",remove=FALSE) |>
    dplyr::select(-.data$extensions) |>
    tidyr::chop(cols=c(.data$collisions)) |>
    tibble::deframe()


  meld(x, !!!collisions)
}
