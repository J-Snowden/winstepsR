#' @title r_control_file
#'
#' @description Creates a Winsteps control file from an R data.frame.
#'
#' @param name The name you want to save the control file as.
#' @param df A data set object.
#' @param first_item The numeric location of the first item in your dataset.
#' @param num_items The number of items you want to add to the control file.
#' @param person_id_col The name of the column where person names are located.
#' @param ... Additional arguments passed to internal functions.
#' @param groups Optional. String input for adding a ISGROUPS line.
#' @param irefer Optional. String input for adding a IREFER line.
#' @param ifile Optional. Location of a .txt ifile.
#' @param sfile Optional. Location of a .txt sfile.
#' @param demographics Optional. Column names of demographic variables to
#' include in the control file.
#' @return Creates a Winsteps control file in the current directory.
#' @export
#' @importFrom magrittr %>%

r_control_file <- function(name, df, first_item, num_items, person_id_col, ...,
                            groups = NULL, irefer = NULL, ifile = NULL,
                            sfile = NULL, demographics = NULL) {

  # Check that name is a character string
  if (!is.character(name)) {
    stop("Input 'name' is not a character string.")
  }

  # Check if df is a data frame
  if (!is.null(df) && !is.data.frame(df)) {
    stop("Input 'demographics' is not a data frame.")
  }

  # Check that first_item is a double
  if (!is.double(first_item)) {
    stop("Input 'first_item' is not a double.")
  }

  # Check that num_items is a double
  if (!is.double(num_items)) {
    stop("Input 'num_items' is not a double.")
  }

  # Check that person_id_col is a character string
  if (!is.character(person_id_col)) {
    stop("Input 'name' is not a character string.")
  }

  # Check that groups is a character string
  if (!is.null(groups) && !is.character(groups)) {
    stop("Input 'groups' must be a character string.")
  }

  # Check that irefer is a character string
  if (!is.null(irefer) && !is.character(irefer)) {
    stop("Input 'irefer' must be a character string.")
  }

  # Check that ifile is a character string
  if (!is.null(ifile) && !is.character(ifile)) {
    stop("Input 'ifile' must be a character string.")
  }

  # Check that sfile is a character string
  if (!is.null(sfile) && !is.character(sfile)) {
    stop("Input 'sfile' must be a character string.")
  }

  # Check if demographics is a data frame
  if (!is.null(demographics) && !is.data.frame(demographics)) {
    stop("Input 'demographics' is not a data frame.")
  }


  tictoc::tic("generating control file")

  groups_string <- ""
  irefer_string <- ""
  ifile_string <- ""
  sfile_string <- ""


  df <- df %>%
    dplyr::select(dplyr::all_of(first_item):(dplyr::all_of(first_item)+(num_items-1)),
                  dplyr::all_of(person_id_col)) %>%
    dplyr::mutate("blank" = " ") %>%
    dplyr::relocate(dplyr::all_of(person_id_col), .after = "blank")

  if (!is.null(demographics)) {
    demographics <- demographics %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

    demographics[is.na(demographics)] <- "."

    #do the next 2 lines need to be in here? Or are they better outside?
    df$NewSID <- stringr::str_pad(df$NewSID, max(nchar(df[[person_id_col]])),
                                  "right")

    df$NewSID <- paste0(stringr::str_c(df$NewSID, ' '))

    for (i in 1:length(demographics)) {
      df$NewSID <- paste0(stringr::str_c(df$NewSID, eval(parse(text = paste(
        "demographics$", colnames(demographics[i]))))), " ")

    }

  } else {demographics <- c()
  }

  name1 <- grep(person_id_col, colnames(df))
  namlen <- max(nchar(df[[person_id_col]]))
  xwide <- length(max(df[1:(1+(num_items-1))], na.rm = TRUE))

  out_string1 <- paste0("&INST\n",
                        'Title= "', name, ' - generated in R"\n',
                        "; Created: ", Sys.time(), "\n",
                        "; \n",
                        "; Cases processed = ", nrow(df), "\n",
                        "; Variables processed = ", ncol(df), "\n",
                        "ITEM1 = 1 ; Starting column of item responses\n",
                        "NI = ", num_items, " ; Number of items\n",
                        "NAME1 = ", name1, " ; Starting column for person label in data record\n",
                        "NAMLEN = ", namlen, " ; Length of person label\n",
                        "XWIDE = ", xwide, " ; Matches the widest data value observed\n",
                        "; GROUPS = 0 ; Partial Credit model: in case items have different rating scales\n")

  if (!is.null(groups)) {
    groups_string <- "ISGROUPS = *\n"
    for (i in 1:length(groups)) {
      groups_string <- append(groups_string, paste0(groups[i], "\n"))
    }
    groups_string <- append(groups_string, "*\n")
  }

  if (!is.null(irefer)) {
    irefer_string <- "IREFER = *\n"
    for (i in 1:length(irefer)) {
      irefer_string <- append(irefer_string, paste0(irefer[i], "\n"))
    }
    irefer_string <- append(irefer_string, "*\n")
  }

  unique <- as.vector(as.matrix(df[1:(1+(num_items-1))])) %>%
    unique() %>%
    sort()

  codes <- c("CODES = ", ".", unique, " ; matches the data\n")

  newsid <- toupper(person_id_col)
  sidlen <- (namlen - (length(demographics) + 1)) - (length(demographics))
  newsid_str <- paste0("@", newsid, " = 1E", sidlen, " ;")

  win_demo <- c()
  if (!is.null(demographics)) {
    for (i in 1:length(demographics)) {
      demo_col <- sidlen + (i * 2)
      #namlen - ((length(demographics) * 2) - 3) + 2
      win_demo <- append(win_demo, paste0("@", colnames(demographics[i]), " = ",
                                          demo_col, "E", demo_col, " ;"))
    }
    win_demo <- paste0(win_demo, collapse = "\n")
    win_demo <- paste0("\n", win_demo)

  }

  if (!is.null(ifile)) {
    ifile_string <- paste0("IAFILE = ", ifile, "\n")
  }

  if (!is.null(sfile)) {
    sfile_string <- paste0("SAFILE = ", sfile, "\n")
  }

  out_string2 <- paste0("TOTALSCORE = Yes ; Include extreme responses in reported scores\n",
                        ifile_string,
                        sfile_string,
                        "; Person Label variables: columns in label: columns in line\n",
                        newsid_str, win_demo,
                        "\n&END ; Item labels follow: columns in label\n")

  item_names = c()
  for (i in 1:num_items) {
    new_name <- colnames(df[i])
    item_names <- c(item_names, paste0(new_name, " ; ", "Item ", i, " : ", i,
                                       "-", i, "\n"))
  }
  end_names <- "END NAMES\n"

  new <- c(out_string1, groups_string, irefer_string, codes, out_string2,
           item_names, end_names)

  cat(new, file = paste0(name, '_cf_r.txt'), sep = "")

  utils::write.table(df, file = paste0(name, '_cf_r.txt'), row.names = FALSE,
                     col.names = FALSE,
                     na = ".", sep = "", quote = FALSE, append = TRUE)

  tictoc::toc()
}
