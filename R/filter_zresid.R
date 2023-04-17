#' @title filter_zresid
#'
#' @description Removes responses from a data set based on user-supplied
#' z-residual thresholds
#'
#' @param data A data set object
#' @param xfile A XFILE or Observation File exported from WINSTEPS
#' @param id_col The name of the column where person names are located
#' @param items The column name(s) of item(s) that will be filtered
#' @param zthresh A vector containing the high and low values of the
#' z-residuals used to filter the data
#' @return A data frame object that with the responses outside the z-residual
#' thresholds removed, and a data frame object with the removed responses
#' @export
#' @importFrom dplyr "%>%"

filter_zresid <- function(data, xfile, id_col, items, zthresh) {

  tic("total")

  count_filtered <- 0

  filtered_xfile <- data.frame()

  for (i in 1:(length(items))) {

    temp_filtered_xfile <- xfile %>%
      dplyr::filter((xfile$ZSCORE > zthresh[1] | xfile$ZSCORE < zthresh[2]) &
                      xfile$`ITEM LABEL` %in% items[i])

    count_filtered <- count_filtered + nrow(temp_filtered_xfile)

    data <- data %>%
      mutate("{items[i]}" := replace(.data[[items[i]]], .data[[id_col]]
                                     %in% temp_filtered_xfile$`PERSON LABEL`, NA))

    filtered_xfile <- bind_rows(filtered_xfile, temp_filtered_xfile)

    message((paste0(nrow(filtered_xfile), " responses (",
                    round((nrow(filtered_xfile) / nrow(data)*100), 3),
                    "%) removed from ", items[i])))
  }

  message(paste("There were a total of", format(count_filtered, big.mark =","),
                "datapoints removed out of", format(nrow(xfile), big.mark=","),
                "datapoints in the dataset."))
  message(paste0("That's ", round((count_filtered / nrow(xfile)*100), 3),
                 "% of all responses."))

  out <- list(data, filtered_xfile)

  toc()

  return(out)
}
