# library(mclust)
#
# sampledata <- generate_sample(1000, 5)
#
#
# resa <-
#   for(i in 1:10){
#   if(i == 1){
#     ress <- c(NA)
#   }
#   ress[i] <- list(fit_em(sampledata, i))
#   if(i == 10){
#     return(ress)
#   }
#   }
#
# BICa <- for(i in 1:10){
#   if(i == 1){
#     BICs <- c(NA)
#   }
#   BICs[i] <- ress[[i]]$BIC[1]
#   if(i == 10){
#     return(BICs)
#   }
# }
