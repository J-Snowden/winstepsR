#' @title clean_dif
#'
#' @description Reads in a DIF file (Table 30) from Winsteps and removes all
#' but the significant rows
#'
#' @param filename DIF file exported from Winsteps
#' @param outname Name to save the exported file
#' @return A .txt file with only significant DIF rows remaining.
#' @export
#' @importFrom dplyr "%>%"

clean_dif <- function(filename, outname) {

  tic()
  data <- read_lines(filename)
  data <- data[!grepl("-----", data)]

  # Table 30.1 --------------------------------------------------------------

  table30.1_colnames <- c("Person Class", "Obs-Exp Average", "DIF MEASURE",
                          "DIF S.E.", "Person Class 2", "Obs-Exp Average2",
                          "DIF MEASURE 2", "DIF S.E. 2", "DIF CONTRAST", "JOINT S.E.",
                          "Welch t", "Welch d.f", "Welch Prob.", "MH Chi", "MH Prob",
                          "CUMLOR", "Active Slides", "Item Number", "Name")

  table30.1_end <- min(which(grepl("Width of Mantel", data) == TRUE))

  table30.1 <- data[9:table30.1_end-1]
  table30.1 <- str_split(table30.1, "\\s{1,7}")
  suppressWarnings(table30.1_df <- do.call(rbind.data.frame, table30.1))

  table30.1_df <- table30.1_df %>%
    select(2, 4:7, 9:22) %>%
    setNames(table30.1_colnames) %>%
    filter(`Person Class` != ".") %>%
    filter(`Person Class 2` != ".")

  table30.1_df <- data.frame(lapply(table30.1_df, function(x) {
    gsub(">", "", x)}))
  table30.1_df <- data.frame(lapply(table30.1_df, function(x) {
    gsub("<", "", x)}))
  table30.1_df <- data.frame(lapply(table30.1_df, function(x) {
    gsub("|", "", x, fixed = TRUE)}))

  table30.1_df <- table30.1_df %>%
    mutate_at(c(1:18), as.numeric)

  sheet1 <- table30.1_df %>%
    filter(`DIF.CONTRAST` <= -0.64 & `Welch.Prob.` <= 0.05 |
             `DIF.CONTRAST` >= 0.64 & `Welch.Prob.` <= 0.05) %>%
    arrange(`DIF.CONTRAST`)

  end_of_1 <- nrow(sheet1)/2

  sheet1.1 <- sheet1[1:end_of_1,] %>%
    arrange(`Person.Class`, `Person.Class.2`, `Name`, `DIF.CONTRAST`,
            `MH.Prob`) %>%
    add_row()

  sheet1.2 <- sheet1[(end_of_1+1):nrow(sheet1),] %>%
    arrange(`Person.Class`, `Person.Class.2`, `Name`, `DIF.CONTRAST`,
            `MH.Prob`)

  sheet1 <- bind_rows(sheet1.1, sheet1.2)

  sheet2 <- table30.1_df %>%
    filter(`DIF.CONTRAST` <= -0.64 & `MH.Prob` <= 0.05 |
             `DIF.CONTRAST` >= 0.64 & `MH.Prob` <= 0.05) %>%
    arrange(`DIF.CONTRAST`)

  end_of_1 <- nrow(sheet2)/2

  sheet2.1 <- sheet2[1:end_of_1,] %>%
    arrange(`Person.Class`, `Person.Class.2`, `Name`, `DIF.CONTRAST`,
            `MH.Prob`) %>%
    add_row()

  sheet2.2 <- sheet2[(end_of_1+1):nrow(sheet1),] %>%
    arrange(`Person.Class`, `Person.Class.2`, `Name`, `DIF.CONTRAST`,
            `MH.Prob`)

  sheet2 <- bind_rows(sheet2.1, sheet2.2)


  # Table 30.2 --------------------------------------------------------------

  table30.2_colnames <- c("Person Class", "Count", "Score", "Average", "Expect",
                          "Measure", "DIF Score", "DIF Measure", "DIF Size",
                          "DIF S.E.", "DIF t", "d.f.", "Prob.", "Item Number",
                          "Name")

  table30.2 <- data[(table30.1_end + 12):(which(grepl("TABLE 30.3", data)
                                                == TRUE)-3)]

  table30.2_split <- str_split(table30.2, "\\s{1,7}")
  suppressWarnings(table30.2_df <- do.call(rbind.data.frame, table30.2_split))

  table30.2_df <- table30.2_df %>%
    select(2, 4:17) %>%
    setNames(table30.2_colnames) %>%
    filter(`Person Class` != ".")

  table30.2_df <- data.frame(lapply(table30.2_df, function(x) {
    gsub(">", "", x)}))
  table30.2_df <- data.frame(lapply(table30.2_df, function(x) {
    gsub("<", "", x)}))
  table30.2_df <- data.frame(lapply(table30.2_df, function(x) {
    sub("|", "", x, fixed = TRUE)}))

  table30.2_df <- table30.2_df %>%
    mutate_at(c(1:14), as.numeric)

  #return(table30.2_df)
  table30.2_df <- table30.2_df %>%
    filter(`Prob.` <= 0.05)


  # Write File --------------------------------------------------------------

  write_xlsx(list("30.1 - Rasch-Welch" = sheet1, "30.1 - Mantel-Haenszel"
                  = sheet2, "Table 30.2" = table30.2_df),
             paste0("Significant_", outname, ".xlsx"))
  message(paste0('Results saved as ', '"Significant_', outname, '.xlsx"'))
  message(paste0("Save location: ", getwd()))
  toc()
}
