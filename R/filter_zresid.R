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

  count_filtered <- 0

  for (i in 1:(length(items))) {

    tic(paste(items[i], "filtered and mutated,"))

    filtered_xfile <- xfile %>%
      dplyr::filter((xfile$ZSCORE > zthresh[1] | xfile$ZSCORE < zthresh[2]) &
               xfile$`ITEM LABEL` == items[i])

    count_filtered <- count_filtered + nrow(filtered_xfile)

    filtered_data <- data %>%
      mutate("{items[i]}" := replace(.data[[items[i]]], .data[[id_col]]
                                   %in% filtered_xfile$`PERSON LABEL`, NA))


    assign(paste0("XFILE_", items[i]), filtered_xfile, 1)

    message((paste("  ", nrow(filtered_xfile), "responses removed")))
  }

  # tic("saving data")
  #
  # if (length(items) > 1) {
  #   write_sav(data, paste0(name, "_", length(items), '_items_treated_r.sav'))
  # } else {
  #   write_sav(data, paste0(name, "_", items, '_treated_r.sav'))
  # }
  #
  # toc()

  name <- paste0(length(items), '_items_treated')

  message(paste("There were a total of", format(count_filtered, big.mark =","),
                "datapoints removed out of", format(nrow(xfile), big.mark=","),
                "datapoints in the dataset."))
  message(paste0("Thats ", round((count_filtered / nrow(xfile)*100), 3),
                 "% of all responses."))
  toc()
  #return(data)
}
