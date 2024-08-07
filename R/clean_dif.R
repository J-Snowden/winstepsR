#' @title clean_dif
#'
#' @description Reads in a DIF file (Table 30) from Winsteps and removes all
#' but the significant rows
#'
#' @param filename DIF file exported from Winsteps
#' @param outname Name to save the exported file
#' @return A .txt file with only significant DIF rows remaining.
#' @export
#' @importFrom magrittr %>%

clean_dif <- function(filename, outname) {

  # Check that filename is a character string
  if (!is.character(filename)) {
    stop("Input 'filename' is not a character string.")
  }

  # Check that outname is a character string
  if (!is.character(outname)) {
    stop("Input 'outname' is not a character string.")
  }

  'Person Class' <- 'Person Class 2' <- 'DIF.CONTRAST' <- 'Name' <-
    'Welch.Prob.' <- 'MH.Prob' <- 'Prob.' <- 'Person.Class' <-
    'Person.Class.2' <- NULL

  tictoc::tic()
  data <- brio::read_lines(filename)
  data <- data[!grepl("-----", data)]

  # Table 30.1 --------------------------------------------------------------

  table30.1_colnames <- c("Person Class", "Obs-Exp Average", "DIF MEASURE",
                          "DIF S.E.", "Person Class 2", "Obs-Exp Average2",
                          "DIF MEASURE 2", "DIF S.E. 2", "DIF CONTRAST", "JOINT S.E.",
                          "Welch t", "Welch d.f", "Welch Prob.", "MH Chi", "MH Prob",
                          "CUMLOR", "Active Slides", "Item Number", "Name")

  table30.1_end <- min(which(grepl("Width of Mantel", data) == TRUE))

  table30.1 <- data[9:table30.1_end-1]
  table30.1 <- gsub("\\|$", "", table30.1)
  table30.1 <- stringr::str_split(table30.1, "\\s{1,7}")
  table30.1 <- lapply(table30.1, function(x) x[1:22])

  table30.1_df <- do.call(rbind.data.frame, table30.1)

  shift_row_data <- function(df) {
    shift_if_numeric <- function(row) {
      # Check if column 3 is numeric
      if (!is.na(as.numeric(row[3]))) {
        # Shift everything from column 3 to the right
        row <- c(row[1:2], "", row[3:(length(row)-1)])
      }

      # Check if column 8 (which is now 9 if we shifted before) is numeric
      if (!is.na(as.numeric(row[8]))) {
        # Shift everything from column 8 to the right
        row <- c(row[1:7], "", row[8:(length(row)-1)])
      }

      return(row)
    }

    # Apply the shift function to each row
    df_shifted <- t(apply(df, 1, shift_if_numeric))

    # Convert back to data frame and set column names
    df_shifted <- as.data.frame(df_shifted, stringsAsFactors = FALSE)
    colnames(df_shifted) <- colnames(df)

    return(df_shifted)
  }

  # Apply the function to your dataframe
  table30.1_df <- shift_row_data(table30.1_df)

  table30.1_df <- table30.1_df %>%
    dplyr::select(2, 4:7, 9:22) %>%
    stats::setNames(table30.1_colnames) %>%
    dplyr::filter(`Person Class` != ".") %>%
    dplyr::filter(`Person Class 2` != ".")

  table30.1_df <- data.frame(lapply(table30.1_df, function(x) {
    gsub(">", "", x)}))
  table30.1_df <- data.frame(lapply(table30.1_df, function(x) {
    gsub("<", "", x)}))
  table30.1_df <- data.frame(lapply(table30.1_df, function(x) {
    gsub("|", "", x, fixed = TRUE)}))

  table30.1_df <- table30.1_df %>%
    dplyr::mutate_at(c(1:18), as.numeric)

  sheet1 <- table30.1_df %>%
    dplyr::filter(`DIF.CONTRAST` <= -0.64 & `Welch.Prob.` <= 0.05 |
                    `DIF.CONTRAST` >= 0.64 & `Welch.Prob.` <= 0.05) %>%
    dplyr::arrange(`DIF.CONTRAST`)

  end_of_1 <- nrow(sheet1)/2

  sheet1.1 <- sheet1[1:end_of_1,] %>%
    dplyr::arrange(`Person.Class`, `Person.Class.2`, `Name`, `DIF.CONTRAST`,
                   `MH.Prob`) %>%
    dplyr::add_row()

  sheet1.2 <- sheet1[(end_of_1+1):nrow(sheet1),] %>%
    dplyr::arrange(`Person.Class`, `Person.Class.2`, `Name`, `DIF.CONTRAST`,
                   `MH.Prob`)

  sheet1 <- dplyr::bind_rows(sheet1.1, sheet1.2)

  sheet2 <- table30.1_df %>%
    dplyr::filter(`DIF.CONTRAST` <= -0.64 & `MH.Prob` <= 0.05 |
                    `DIF.CONTRAST` >= 0.64 & `MH.Prob` <= 0.05) %>%
    dplyr::arrange(`DIF.CONTRAST`)

  end_of_1 <- nrow(sheet2)/2

  sheet2.1 <- sheet2[1:end_of_1,] %>%
    dplyr::arrange(`Person.Class`, `Person.Class.2`, `Name`, `DIF.CONTRAST`,
                   `MH.Prob`) %>%
    dplyr::add_row()

  sheet2.2 <- sheet2[(end_of_1+1):nrow(sheet1),] %>%
    dplyr::arrange(`Person.Class`, `Person.Class.2`, `Name`, `DIF.CONTRAST`,
                   `MH.Prob`)

  sheet2 <- dplyr::bind_rows(sheet2.1, sheet2.2)


  # Table 30.2 --------------------------------------------------------------

  table30.2_colnames <- c("Person Class", "Count", "Score", "Average", "Expect",
                          "Measure", "DIF Score", "DIF Measure", "DIF Size",
                          "DIF S.E.", "DIF t", "d.f.", "Prob.", "Item Number",
                          "Name")

  table30.2 <- data[(table30.1_end + 12):(which(grepl("TABLE 30.3", data)
                                                == TRUE)-3)]

  table30.2 <- gsub("\\|$", "", table30.2)

  table30.2_split <- stringr::str_split(table30.2, "\\s{1,7}")

  table30.2_split <- lapply(table30.2_split, function(x) x[1:17])

  table30.2_df <- do.call(rbind.data.frame, table30.2_split)

  table30.2_df <- table30.2_df %>%
    dplyr::select(2, 4:17) %>%
    stats::setNames(table30.2_colnames) %>%
    dplyr::filter(`Person Class` != ".")

  table30.2_df <- data.frame(lapply(table30.2_df, function(x) {
    gsub(">", "", x)}))
  table30.2_df <- data.frame(lapply(table30.2_df, function(x) {
    gsub("<", "", x)}))
  table30.2_df <- data.frame(lapply(table30.2_df, function(x) {
    sub("|", "", x, fixed = TRUE)}))

  table30.2_df <- table30.2_df %>%
    dplyr::mutate_at(c(1:14), as.numeric)

  #return(table30.2_df)
  table30.2_df <- table30.2_df %>%
    dplyr::filter(`Prob.` <= 0.05 & `DIF.Size` <= -0.64 |
                    `Prob.` <= 0.05 & `DIF.Size` >= 0.64)

  # Write File --------------------------------------------------------------

  writexl::write_xlsx(list("30.1 - Rasch-Welch" = sheet1, "30.1 - Mantel-Haenszel"
                           = sheet2, "Table 30.2" = table30.2_df),
                      paste0("Significant_", outname, ".xlsx"))

  cat(paste0('Results saved as ', '"Significant_', outname, '.xlsx"\n'))
  cat(paste0("Save location: ", getwd(), "\n"))

  tictoc::toc()
}
