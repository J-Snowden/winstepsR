#' @title psychometrics_table
#'
#' @description Reads in a WINSTEPS output tables and extracts psychometrics
#'
#' @param filename DIF file exported from Winsteps
#' @param outname Name to save the exported file
#' @return A dataframe with psychometrics for input constructs
#' @export
#' @importFrom magrittr %>%
#'
psychometrics_table <- function(filename) {
  startingdir <- getwd()

  #create psychometrics dataframe
  psy_cols <- c("Construct", "Person Separation", "Person Reliability",
                "Item Separation", "Item Reliability", "Cronbachs Alpha",
                "Mean Infit MNSQ", "Infit Low", "Infit High", "Mean Outfit MNSQ",
                "Outfit Low", "Outfit High", "Items >= Outfit 1.3",
                "Var. Exp. Perons", "Var. Exp. Items",  "First Contrast")

  psychometrics <- data.frame(matrix(NA, ncol = 16))

  colnames(psychometrics) <- psy_cols

  #loop over filepaths and names
  for (filename in filename) {
    setwd(filename[1])

    # Check that filename[2] is a character string
    if (!is.character(filename[2])) {
      stop("Input 'filename[2]' is not a character string.")
    }

    # Table 10 --------------------------------------------------------------
    #Person and Item Sep
    table10_data <- brio::read_lines(paste0(filename[2], "_Table_10.txt"))[4]
    pattern <- "-?\\d+(?:\\.\\d+)?|\\.\\d+"
    fit_stats <- as.numeric(unlist(regmatches(table10_data,
                                              gregexpr(pattern, table10_data))))

    #Table 10 as df
    table10_data <- brio::read_lines(paste0(filename[2], "_Table_10.txt"))
    table10_data <- table10_data[!grepl("-----", table10_data)]

    # table10.1_colnames <- c("Entry Number", "Total Score", "Total Count",
    #                         "JMLE Measure", "Model S.E.", "Infit MNSQ",
    #                         "Infit ZSTD", "Outfit MNSQ", "Outfit ZSTD",
    #                         "PTMEASUR-AL", " Corr.", "Exp", "Exact Obs%",
    #                         "Match Exp%", "Item")
    #
    # table10.1_colnames_anchor <- c("Entry Number", "Total Score", "Total Count",
    #                                "JMLE Measure", "Model S.E.", "Infit MNSQ",
    #                                "Infit ZSTD", "Outfit MNSQ", "Outfit ZSTD",
    #                                "PTMEASUR-AL", " Corr.", "Exp", "Exact Obs%",
    #                                "Match Exp%", "Displace", "Item")

    table10.1_end <- min(which(grepl("TABLE 10.3", table10_data) == TRUE))

    table10.1 <- table10_data[13:table10.1_end-4]

    # Remove "| and |" and split the strings
    result_list <- str_split(str_replace_all(table10.1, "\\|", " "), "\\s+")

    # Convert the list to a table10_data frame
    result_df <- data.frame(do.call(rbind, result_list), stringsAsFactors = FALSE) %>%
      select(-1, -ncol(.)) %>%
      mutate_at(c(1:3, 5:9, 11:14), as.numeric)

    # if (ncol(result_df) == 15) {
    #   colnames(result_df) <- table10.1_colnames
    # } else if (ncol(result_df) == 16) {
    #   colnames(result_df) <- table10.1_colnames_anchor
    # }

    mean_infit <- round(mean(result_df$`X7`), 2)
    mean_outfit <- round(mean(result_df$`X9`), 2)
    #high_infit <- sum(result_df$`Infit MNSQ` > 1.3)
    high_outfit <- sum(result_df$`X9` > 1.3)
    min_infit <- min(result_df$`X7`)
    max_infit <- max(result_df$`X7`)
    min_outfit <- min(result_df$`X9`)
    max_outfit <- max(result_df$`X9`)

    # Extract the variable name without the .txt extension
    df_name <- sub("\\.txt$", "", tools::file_path_sans_ext(filename[2]))

    # Assign the table10_data frame to a variable in the global environment
    #assign(df_name, result_df, envir = .GlobalEnv)

    # Table 3 --------------------------------------------------------------
    table3_data <- brio::read_lines(paste0(filename[2], "_Table_3.txt"))
    table3_alphaline <- grep("\"TEST\" RELIABILITY =", table3_data)
    alpha <- as.numeric(str_extract(table3_data[table3_alphaline],
                                    '(?<=RELIABILITY = ).{0,4}'))


    # Table 23 --------------------------------------------------------------
    table23_data <- brio::read_lines(paste0(filename[2], "_Table_23.txt"))[8:12]

    # Extract percentages using regular expressions
    table23_percentages <- regmatches(table23_data, gregexpr("[0-9]+\\.[0-9]+%",
                                                             table23_data))
    table23_percentages <- unlist(table23_percentages)[c(2, 4)]  # Select the desired percentages

    # Extract number using string splitting
    eigenvalue <- as.numeric(unlist(regmatches(table23_data[5],
                                               gregexpr("[0-9]+\\.[0-9]+",
                                                        table23_data[5]))))

    # Combine into table ------------------------------------------------------

    psy_line <- c(filename[2], fit_stats, alpha, mean_infit, min_infit,
                  max_infit, mean_outfit, min_outfit, max_outfit,
                  high_outfit, table23_percentages, eigenvalue[1])

    psychometrics <- rbind(psychometrics, psy_line)

    #assign("psy_line", psy_line, envir = .GlobalEnv)

    setwd(startingdir)

  }

  psychometrics <- psychometrics[-1, ]

  psychometrics$`Var. Exp. Perons` <- parse_number(psychometrics$`Var. Exp. Perons`) / 100
  psychometrics$`Var. Exp. Items` <- parse_number(psychometrics$`Var. Exp. Items`) / 100

  psychometrics <- mutate_at(psychometrics, vars(2:16), as.numeric)

  #assign("psychometrics", psychometrics, envir = .GlobalEnv)
  return(psychometrics)

}
