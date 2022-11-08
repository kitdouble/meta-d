m_ratio <- function(data, identifier){
  
  for(i in unique(data[, identifier])){
    
    x <- data[data[,identifier] == i,]
    
    # Separate based on stimulus
    S1 <- subset(x, stimulus == "S1")
    S2 <- subset(x, stimulus == "S2")
    
    
    # In the format A1, A2, A3, B3, B2, B1 (Where A = correct response and B = incorrect, 1 = highest conf, 3 = lowest conf)
    nR_S1 <- c(rev(table(S1$accuracy, S1$conf)[2,]), table(S1$accuracy, S1$conf)[1,])
    nR_S2 <- c(rev(table(S2$accuracy, S2$conf)[1,]), table(S2$accuracy, S2$conf)[2,])
    
    
    fit_MLE <- fit_meta_d_MLE(nR_S1,nR_S2)
    MLE_M_ratio <- fit_MLE$M_ratio[1]
    look <- data.frame(ID = i, MLE_M_ratio = MLE_M_ratio)
    
    if(unique(data[,identifier])[1] != i){meta_data <- rbind(meta_data, look)}
    if(unique(data[,identifier])[1] == i){meta_data <- look}
    
  }
  return(meta_data)
}
