#' @title winsteps_output
#'
#' @description Interfaces with Winsteps, creating a BATCH file to analyze
#' a control file and export tables and files
#'
#' @param ctrlfile The filename of a Winsteps controlfile in the currecnt directory
#' @param shortname Name used to save exported files
#' @param ... Additional arguments passed to internal functions.
#' @param tables Optional. A vector containing the tables to be exported from Winsteps
#' @param files Optional. A vector containing the files to be exported from Winsteps
#' @param dif Optional. Column names for a DIF table to be exported
#' @param ifile Optional. Include 'ifile' for an ifile to be exported
#' @param sfile Optional. Include 'sfile' for an sfile to be exported
#' @return Exports select tables and files from Winsteps
#' @export
#' @importFrom magrittr %>%

winsteps_output <- function(ctrlfile, shortname, ..., tables = NULL,
                            files = NULL, dif = NULL, ifile = NULL,
                            sfile = NULL) {

  # Check that ctrlfile is a character string
  if (!is.character(ctrlfile)) {
    stop("Input 'ctrlfile' is not a character string.")
  }

  # Check that shortname is a character string
  if (!is.character(shortname)) {
    stop("Input 'shortname' is not a character string.")
  }

  # Check that tables is a number or vector
  if (!is.null(tables) && !(is.numeric(tables) ||
                            (is.vector(tables) && all(is.numeric(tables))))) {
    stop("tables must be NULL, a numeric value, or a vector of numeric values.")
  }

  # Check that files is a character string or vector
  if (!is.null(files) && !is.character(files) &&
      !is.vector(files, mode = "character")) {
    stop("dif must be either a character or a vector of characters.")
  }

  # Check that dif is a character string or vector
  if (!is.null(dif) && !is.character(dif) &&
      !is.vector(dif, mode = "character")) {
    stop("dif must be either a character or a vector of characters.")
  }

  # Check that ifile is a character string
  if (!is.null(ifile) && !is.character(ifile)) {
    stop("ifile must be a character string.")
  }

  # Check that sfile is a character string
  if (!is.null(sfile) && !is.character(sfile)) {
    stop("ifile must be a character string.")
  }

  ifile_text <- ""
  sfile_text <- ""

  first <- paste("START /w C:\\winsteps\\WINSTEPS BATCH=YES ")

  sink(paste(shortname, "bat", sep = "."))

  wd <- getwd()
  ctrltxt <- file.path(wd, ctrlfile)

  if (!is.null(tables)) {
    outtables <- c()
    outtext <- c()

    for (i in 1:length(tables)) {
      outtext <- append(outtext, file.path(wd, paste0(shortname, "_Table_",
                                                      tables[i], ".txt")))

      table <- tables[i]
      tableout <- list()

      for (i in 1:(table-1)) {
        tableout <- paste0("0", tableout)
      }
      templist <- paste0("TABLES=", tableout, 1)
      outtables <- append(outtables, templist)
    }
  }

  if(!is.null(dif)) {

    diftable <- "TABLES=000000000000000000000000000001"
    outdif <- c()
    difname <- c()

    for (i in 1:length(dif)) {
      outdif <- append(outdif, file.path(wd, paste0(shortname, "_DIF_",
                                                    dif[i], ".txt")))
      difname <- append(difname, paste0("DIF=@", dif[i]))

    }
  }

  if (!is.null(files)) {
    blank <- file.path(wd, "Blankoutfile.txt ")
    outfiles <- c()

    for (i in 1:length(files)) {
      if(files[i] == "ifile" | files[i] == "sfile"){
        outfiles <- append(outfiles, paste0(files[i], "=", shortname, "_",
                                            files[i], ".txt"))
      } else {
        outfiles <- append(outfiles, paste0(files[i], "=", shortname, "_",
                                            files[i], ".xlsx"))
      }
    }
    outfiles <- toString(outfiles)
  }

  if(!is.null(ifile)) {
    ifile_text <- paste0("IAFILE=", file.path(wd, paste0(ifile, ".txt")))
  }

  if(!is.null(sfile)) {
    sfile_text <- paste0("SAFILE=", file.path(wd, paste0(sfile, ".txt")))
  }

  if (!is.null(tables)) {
    for (i in 1:length(outtables)) {
      cat(paste(first, ctrltxt, outtext[i], ifile_text, sfile_text,
                outtables[i], sep = " "), step = "\n")
    }
  }

  if (!is.null(files)) {
    cat(paste(first, ctrltxt, blank, outfiles, ifile_text, sfile_text), step = "\n")
  }

  if (!is.null(dif)) {
    for (i in 1:length(dif)) {
      cat(paste(first, ctrltxt, outdif[i], difname[i], diftable, ifile_text, sfile_text), step = "\n")
    }
  }

  cat(paste("EXIT"))
  sink()

  shell(paste0(shortname, ".bat"))

  unlink("Blankoutfile.txt")
  unlink(paste0(shortname, ".bat"))
}
