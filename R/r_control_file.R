r_control_file <- function(name, data, item1, ni, id_col, ..., groups = NULL,
                           ifile = NULL, sfile = NULL, demographics = NULL) {
  
  tic("generating control file")
 
  groups_string <- ""
  
  data <- data %>%
    select(all_of(item1):(all_of(item1)+(ni-1)), all_of(id_col)) %>%
    mutate("blank" = " ") %>%
    relocate(all_of(id_col), .after = "blank")
  
  if (!is.null(demographics)) {
    demographics <- demographics %>%
      mutate(across(everything(), as.character))

    demographics[is.na(demographics)] <- "."

    #do the next 2 lines need to be in here? Or are they better outside?
    data$NewSID <- stringr::str_pad(data$NewSID, max(nchar(data[[id_col]])), 
                                    "right")
    
    data$NewSID <- paste0(str_c(data$NewSID, ' '))
    
    for (i in 1:length(demographics)) {
      data$NewSID <- paste0(str_c(data$NewSID, eval(parse(text = paste(
        "demographics$", colnames(demographics[i]))))), " ")
      
    }
    
  } else {demographics <- c()
  }
  
  name1 <- grep(id_col, colnames(data))
  namlen <- max(nchar(data[[id_col]]))
  xwide <- length(max(data[1:(1+(ni-1))], na.rm = TRUE))
  
  out_string1 <- paste0("&INST\n",
                        'Title= "', name, ' - generated in R"\n',
                        "; Created: ", Sys.time(), "\n",
                        "; \n",
                        "; Cases processed = ", nrow(data), "\n",
                        "; Variables processed = ", ncol(data), "\n",
                        "ITEM1 = 1 ; Starting column of item responses\n",
                        "NI = ", ni, " ; Number of items\n",
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
  
  unique <- as.vector(as.matrix(data[1:(1+(ni-1))])) %>%
    unique() %>%
    sort()
  
  codes <- c("CODES = ", ".", unique, " ; matches the data\n")
  
  newsid <- toupper(id_col)
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
  
  out_string2 <- paste0("TOTALSCORE = Yes ; Include extreme responses in reported scores\n",
                        "; Person Label variables: columns in label: columns in line\n",
                        newsid_str, win_demo,
                        "\n&END ; Item labels follow: columns in label\n")
  
  item_names = c()
  for (i in 1:ni) {
    new_name <- colnames(data[i])
    item_names <- c(item_names, paste0(new_name, " ; ", "Item ", i, " : ", i, 
                                       "-", i, "\n"))
  }
  end_names <- "END NAMES\n"
  
  new <- c(out_string1, groups_string, codes, out_string2, item_names, end_names)
  
  cat(new, file = paste0(name, '_cf_r.txt'), sep = "")
  
  write.table(data, file = paste0(name, '_cf_r.txt'), row.names = FALSE, 
              col.names = FALSE,
              na = ".", sep = "", quote = FALSE, append = TRUE)
  
  toc()
}
