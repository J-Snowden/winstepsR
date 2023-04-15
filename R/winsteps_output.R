winsteps_output <- function(ctrlfile, shortname, ..., tables = NULL, 
                            files = NULL, dif = NULL, ifile = NULL,
                            sfile = NULL) {
  
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
      difname <- append(difname, paste0("dif=@", dif[i]))
      
    }
  }
  
  if (!is.null(files)) {
    blank <- file.path(wd, "Blankoutfile.txt ")
    outfiles <- c()
    
    for (i in 1:length(files)) {
      outfiles <- append(outfiles, paste0(files[i], "=", shortname, "_", 
                                          files[i], ".xlsx"))
    }
    outfiles <- toString(outfiles)
  }
  
  ifile_text <- ""
  sfile_text <- ""
  
  if(!is.null(ifile)) {
    ifile_text <- paste0("IAFILE=", file.path(wd, paste0(ifile, ".txt")))
  }
  
  if(!is.null(sfile)) {
    sfile_text <- paste0("SAFILE=", file.path(wd, paste0(sfile, ".txt")))
  }
  
  first <- paste("START /w C:\\winsteps\\WINSTEPS BATCH=YES ")
  
  sink(paste(shortname, "bat", sep = "."))
  
  if (!is.null(tables)) {
    for (i in 1:length(outtables)) {
      cat(paste(first, ctrltxt, outtext[i], ifile_text, sfile_text, 
                outtables[i], sep = " "), step = "\n")
    }
  }
  
  if (!is.null(files)) {
    cat(paste(first, ctrltxt, blank, outfiles), step = "\n")
  }
  
  if (!is.null(dif)) {
    for (i in 1:length(dif)) {
      cat(paste(first, ctrltxt, outdif[i], difname[i], diftable), step = "\n")
    }
  }
  
  cat(paste("EXIT"))
  sink()
  
  shell(paste0(shortname, ".bat"))
  
  unlink("Blankoutfile.txt")
  unlink(paste0(shortname, ".bat"))
}