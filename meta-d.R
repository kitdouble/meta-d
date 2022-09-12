
calc_metad <- function(data, identifier){
  require(metaSDT)
  for(i in unique(data[, identifier])){
    tryCatch({
      
      df = data[data[,identifier]== i,] 
      
      nR_S1 <- table(df$conf, df$correct_response, df$response)[,,1]
      nR_S2 <- table(df$conf, df$correct_response, df$response)[,,2]
      nR_S1 <- c(rev(nR_S1[,1]), nR_S1[,2])
      nR_S2 <- c(rev(nR_S2[,1]), nR_S2[,2])
      
      adj_f = 1/length(nR_S1)
      nR_S1_adj = nR_S1 + adj_f
      nR_S2_adj = nR_S2 + adj_f
      
      x <- fit_meta_d_MLE(nR_S1_adj,nR_S2_adj)
      x <- as.data.frame(x)
      x$ID <- i
      if(unique(data[, identifier])[1] != i){meta_data <- rbind(meta_data, x)}
      if(unique(data[, identifier])[1]== i){meta_data <- x}
      
    }, error=function(e){print(paste("Error calculating for",i))})
  }
  meta_data <- meta_data[!duplicated(meta_data$ID),c("ID","da", "meta_da", "M_diff", "M_ratio")]
  return(meta_data)
}
