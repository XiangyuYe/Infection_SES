Descrp <- function(data_mat,types,ndigits){
  
  descrp_list <- lapply(1:ncol(data_mat), function(x){
    varx <- data_mat[,x]
    type <- types[x]
    namex <- colnames(data_mat)[x]
    if (type == "qualitative") {
      tabx <- table(varx)
      dfx <- data.frame(Variable = namex,
                        sub_variable = names(tabx),
                        Description = paste0(tabx, 
                                             "(", 
                                             prop.table(tabx) %>% round(ndigits),
                                             ")"))
      return(dfx) 
    } else if (type == "normal"){
      dfx <- data.frame(Variable = namex,
                        sub_variable = "",
                        Description = paste0(mean(varx, na.rm =T) %>% round(ndigits), 
                                             "++", 
                                             sd(varx, na.rm =T) %>% round(ndigits) ))
      return(dfx)    
    } else if (type == "non-normal"){
      quantx <- quantile(varx, na.rm =T)
      dfx <- data.frame(Variable = namex,
                        sub_variable = "",
                        Description = paste0(quantx[3] %>% round(ndigits),
                                             "(",
                                             quantx[2] %>% round(ndigits),
                                             ",",
                                             quantx[4] %>% round(ndigits),
                                             ")"))
      return(dfx)
    }
  })
  return(Reduce(rbind, descrp_list))
}


