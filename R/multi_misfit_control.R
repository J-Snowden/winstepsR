multi_mistfit_control <- function(name, data, item1, ni, id_col, xfile, items,
                                  zthresh) {
  tic("total")

  count_filtered <- 0
  
  for (i in 1:(length(items))) {
    
    tic(paste(items[i], "filtered and mutated,"))
    
    filtered_xfile <- xfile %>%
      filter((xfile$ZSCORE > zthresh[1] | xfile$ZSCORE < zthresh[2]) & 
               xfile$`ITEM LABEL` == items[i])
    
    count_filtered <- count_filtered + nrow(filtered_xfile)
    
    filtered_data <- data %>% 
      mutate("{items[i]}" := replace(.data[[items[i]]], .data[[id_col]] 
                                   %in% filtered_xfile$`PERSON LABEL`, NA))
  
  
    assign(paste0("XFILE_", items[i]), filtered_xfile, 1)
    
    toc()
    
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
  
  name <- paste0(name, "_", length(items), '_items_treated')
  
  r_control_file(name, filtered_data, item1, ni, id_col)
  message(paste("There were a total of", format(count_filtered, big.mark =","),
                "datapoints removed out of", format(nrow(xfile), big.mark=","),
                "datapoints in the dataset."))
  message(paste0("Thats ", round((count_filtered / nrow(xfile)*100), 3),
                 "% of all responses."))
  toc()
  #return(data)
}