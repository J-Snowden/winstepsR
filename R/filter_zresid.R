#' @title filter_zresid
#'
#' @description Removes responses from a data set based on user-supplied
#' z-residual thresholds.
#'
#' @param df A data set object.
#' @param xfile A data set object that is created from an XFILE or Observation
#'  File exported from WINSTEPS.
#' @param id_col The name of the column where person names are located.
#' @param items The column name(s) of item(s) that will be filtered.
#' @param zthresh A vector containing the high and low values of the
#' z-residuals used to filter the data.
#' @return A data frame object that with the responses outside the z-residual
#' thresholds removed, and a data frame object with the removed responses.
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang :=

filter_zresid <- function(df, xfile, id_col, items, zthresh) {

  # Check if data is a data frame
  if (!is.data.frame(df)) {
    stop("Input 'df' is not a data frame.")
  }

  # Check that xfile is a data frame
  if (!is.data.frame(xfile)) {
    stop("Input 'xfile' is not a data frame.")
  }

  # Check that id_col is a character string
  if (!is.character(id_col)) {
    stop("Input 'id_col' is not a character string.")
  }

  # Check that id_col is a character string
  if (!(is.character(items) && is.vector(items))) {
    stop("Input 'items' is not a character string for a single item, or a vector of character strings for multiple items.")
  }

  # Check that ztresh is a vector with 2 values, the first being higher
  if (!is.vector(zthresh) || length(zthresh) != 2 || zthresh[1] <= zthresh[2]) {
    stop("Input 'zthresh' must be a vector of length 2 with the first element greater than the second.")
  }

  tictoc::tic("total")

  count_filtered <- 0

  filtered_xfile <- data.frame()

  for (i in 1:(length(items))) {

    temp_filtered_xfile <- xfile %>%
      dplyr::filter((xfile$ZSCORE > zthresh[1] | xfile$ZSCORE < zthresh[2]) &
                      xfile$`ITEM LABEL` %in% items[i])

    count_filtered <- count_filtered + nrow(temp_filtered_xfile)

    df <- df %>%
      dplyr::mutate("{items[i]}" := replace(df[[items[i]]], df[[id_col]]
                                     %in% temp_filtered_xfile$`PERSON LABEL`, NA))

    filtered_xfile <- dplyr::bind_rows(filtered_xfile, temp_filtered_xfile)

    cat((paste0(nrow(temp_filtered_xfile), " responses (",
                round((nrow(temp_filtered_xfile) / nrow(df)*100), 3),
                "%) removed from ", items[i], "\n")))
  }

  cat(paste("There were a total of", format(count_filtered, big.mark =","),
            "datapoints removed out of", format(nrow(xfile), big.mark=","),
            "datapoints in the dataset.\n"))
  cat(paste0("That's ", round((count_filtered / nrow(xfile)*100), 3),
             "% of all responses.\n"))

  out <- list(df, filtered_xfile)

  tictoc::toc()

  return(out)
}
