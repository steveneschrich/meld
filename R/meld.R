#' @describeIn vec_meld Binary operator for two vectors
#' @export
`%^%` <- function(a,b) {
  vec_meld(a,b)
}

#' Meld two vectors, warning if unequal
#'
#' @description The operator [dplyr::coalesce()] is useful for combining
#' vectors, however if the values do not agree then coalesce use the first
#' non-NA result. This function will print a warning if there
#' are value mismatches.
#'
#' @param a Vector to meld
#' @param b Vector to meld
#' @param coalesce Logical. Should the coalesce approach (select the first non-NA) be used? The
#' function will warn if there are unequal values, but use the first non-NA.
#'
#' @return A vector melding a and b (see [dplyr::coalesce()]).
#' @export
#'
#' @examples
#' \dontrun{
#' c("A","B","C") %^% c("A",NA,"C")
#' # [1] "A" "B" "C"
#' }
vec_meld <- function(a, b, coalesce=FALSE) {
  if (! all(a %==% b) ) {
    msg <- "Fields in comparison are not equivalent, cannot combine correctly."
    logger::log_warn(msg)
    if ( !coalesce )
      stop(msg)
  }
  # Merging involves picking the non-NA value (or using NA)
  dplyr::coalesce(a,b)
}


#' Meld variables in data frame
#'
#' Combine variables in a data frame into a single, new variable (coalesce) with warnings
#' unless the fields are equal (or one is NA).
#'
#' @param x A data frame to operate on.
#' @param ... Named lists (e.g., `new1=c("old1","old2)`). Specifies the new variable name (`new1`) that
#'   results from melding the two old variables (`old1` and `old2`). List of old variables can be
#'   more than 2.
#' @param remove Logical, should the original variables be removed from the data frame (default is TRUE).
#' @param drop Logical. Should mismatched pairs be dropped (changes number of rows)? Otherwise,
#' a warning is printed.
#' @param coalesce Logical. Should the coalesce approach (select the first non-NA) be used? The
#' function will warn if there are unequal values, but use the first non-NA.
#' @return A data frame with the old variables melded into new variables and old variables removed (assuming
#' `remove=TRUE`).
#' @export
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @examples
#' meld(data.frame(a=c(1,2,3), b=c(NA,2,3)), new=c("a","b"))
#' #   new
#' # 1   1
#' # 2   2
#' # 3   3
meld <- function(x, ..., remove = TRUE, drop = FALSE, coalesce=FALSE) {

  # The input can be a series of newname = c(oldvalue1, oldvalue2).
  fields <- rlang::list2(...)
  cols <- colnames(x)

  # Fuse each new field (names)
  for (f in names(fields)) {
    new_var <- f
    old_vars <- intersect(fields[[f]], cols)

    # To accommodate any number of old variables, we rename the first one
    # Often, it may be a = a if we are coalescing a list. But that does work.
    #if ( length(old_vars) > 0 ) {
    #  x<-dplyr::rename(x, {{ new_var }} := .data[[old_vars[1]]])
    #}
    new_column <- rep(NA, nrow(x))

    if ( length(old_vars) > 0) {

      # Then combine all of the other old variable names
      for ( ov in 1:length(old_vars) ) {
        # Provide specific warning if field cannot be combined.
        equivalent <- new_column %==% x[[old_vars[ov]]]
        if (! all(equivalent)) {
          msg <- glue::glue("Fields are not equivalent so cannot combine correctly: {new_var}, {old_vars[ov]}.")
          if ( drop ) {
            logger::log_warn("{glue::glue(msg)}\nDropping mismatched rows (drop = TRUE).")
            x <- dplyr::filter(x, equivalent)
            new_column <- new_column[equivalent]
          } else if (coalesce) {
            logger::log_warn("{glue::glue(msg)}\nReverting to coalesce (first choice wins).")
          } else {
            logger::log_warn(msg)
            stop(msg)
          }
        }
        # Meld the two columns together.
        new_column <- vec_meld(new_column, x[[old_vars[ov]]], coalesce)
        # Then optionally remove the variable.
        if ( remove )
          x <- dplyr::select(x, -.data[[old_vars[ov]]])

      }
    }
    x <- dplyr::mutate(x, {{new_var}} := new_column)
  }

  # This selects the original columns in the original order.
  x <- dplyr::select(x, dplyr::any_of(cols), names(fields))

  x
}
