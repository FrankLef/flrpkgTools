#' Normalize Data Depending on Some Criteria.
#'
#' Normalize data depending on some criteria.
#'
#' The data will be normalize using a dataframe, the \code{base} that will be
#' joined. Typically, for example, the \code{base} would be a summary of sales
#' by year and entity. Each column is normalized using the formula
#' \code{data$cols * scale / base}.
#'
#' @param data Data.frame to normalize.
#' @param cols Names of the columns to normalize.
#' @param id_cols Names of columns used to join \code{data} and \code{base}.
#' @param scale Number used to multiply the resulting data.
#' @param base Data.frame containing the base amounts used to normalize.
#' @param inverse FALSE (default): Normalize the data. TRUE: Inverse the
#' normalization.
#' @param keep FALSE (default): Don't keep the column with the base amount.
#' TRUE: keep it.
#' @param base_amt Name of the column where the base amount is. Defaults is
#' "base_amt".  This name must NOT be in the column names of \code{data}.
#'
#' @return Data.frame with normalized data.
#' @export
#'
#' @examples
#' \dontrun{
#' TODO
#' }
normaliz <- function(data, cols, id_cols, scale = 1, base, inverse = FALSE,
                     keep = FALSE, base_amt = "base_amt") {
  checkmate::assert_data_frame(base, min.rows = 1, min.cols = 2)
  checkmate::assert_names(base_amt, subset.of = names(base))
  checkmate::assert_names(base_amt, disjunct.from = names(data))
  checkmate::assert_names(id_cols, subset.of = names(base))
  checkmate::assert_number(scale, lower = 1e-9, upper = 1e9)
  checkmate::assert_flag(inverse)
  checkmate::assert_flag(keep)
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 2)
  checkmate::assert_names(base_amt, disjunct.from = names(data))
  checkmate::assert_names(cols, subset.of = names(data))
  checkmate::assert_names(id_cols, subset.of = names(data))

  data <- data |>
    dplyr::left_join(y = base, by = id_cols)

  if (!inverse) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        .cols = tidyselect::all_of(cols),
        .fns = \(x) x * scale / .data[[base_amt]]
      ))
  } else {
    data <- data |>
      dplyr::mutate(dplyr::across(
        .cols = tidyselect::all_of(cols),
        .fns = \(x) x * .data[[base_amt]] / scale
      ))
  }

  if (!keep) data <- data |> dplyr::select(-tidyselect::all_of(base_amt))
}
