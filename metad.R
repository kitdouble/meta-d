m_ratio <- function(data, identifier, stimulus, confidence, accuracy, SSE = F){
  
  data[,confidence] <- as.factor(data[,confidence])
  for(i in unique(data[, identifier])){
    
    x <- data[data[,identifier] == i,]
    stims <- unique(x[,stimulus])
    
    
    # Separate based on stimulus
    S1 <- x[x[,stimulus] == stims[1], ]
    S2 <- x[x[,stimulus] == stims[2], ]

    
    
    # In the format A1, A2, A3, B3, B2, B1 (Where A = correct response and B = incorrect, 1 = highest conf, 3 = lowest conf)
    nR_S1 <- c(rev(table(S1[,accuracy], S1[,confidence])[2,]), table(S1[,accuracy], S1[,confidence])[1,])
    nR_S2 <- c(rev(table(S2[,accuracy], S2[,confidence])[1,]), table(S2[,accuracy], S2[,confidence])[2,])
    
    # N.B. if nR_S1 or nR_S2 contain zeros, this may interfere with estimation of  meta-d' this adds a small adjustment factor
    # When using this correction method, it is recommended to add the adjustment 
    # factor to ALL data for all subjects, even for those subjects whose data is 
    # not in need of such correction, in order to avoid biases in the analysis 
    # (cf Snodgrass & Corwin, 1988).

adj_f = 1/length(nR_S1)
nR_S1 = nR_S1 + adj_f
nR_S2 = nR_S2 + adj_f

    
    
    MLE_M_ratio <- NA
    SSE_M_ratio <- NA
    tryCatch({
    fit_MLE <- fit_meta_d_MLE(nR_S1,nR_S2)
    MLE_M_ratio <- fit_MLE$M_ratio[1]
    meta_da <- fit_MLE$meta_da[1]
    }, error=function(e){cat("ERROR :", i, "\n")})
    
    if(SSE == T){
      tryCatch({
      fit_SSE <- fit_meta_d_SSE(nR_S1,nR_S2)
      meta_da <- fit_SSE$meta_da[1]
      SSE_M_ratio <- fit_SSE$M_ratio[1]
      }, error=function(e){cat("ERROR :", i, "\n")})
      look <- data.frame(ID = i, MLE_M_ratio = MLE_M_ratio, meta_da = meta_da, SSE_M_ratio = SSE_M_ratio)
    } else{
      look <- data.frame(ID = i, MLE_M_ratio = MLE_M_ratio, meta_da = meta_da)
    }
    
    if(unique(data[,identifier])[1] != i){meta_data <- rbind(meta_data, look)}
    if(unique(data[,identifier])[1] == i){meta_data <- look}
    
  }
  return(meta_data)
}
