library(tidyverse)
library(gridExtra)
library(smacof)

setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/combinedTraits-1000words-advs-lemma-book-centric/")
#setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/random-from-book-1000words-all-words-person-centric/")

## Creating a list with all co-ocurrence matrices for all outputs
allmatrices <- list.files(paste0("."),
                          pattern = "(.*)_network_matrix.csv",
                          full.names = T)

alllinks <- list.files(paste0("."),
                       pattern = "(.*)_links.csv",
                       full.names = T)

allnodes <- list.files(paste0("."),
                       pattern = "(.*)_nodes.csv",
                       full.names = T)

thresholds <- list(0,0.05,0.1,0.15,0.25,-0.8,-0.6,-0.4,-0.2)

for(thresh in thresholds){
  if (!file.exists(as.character(thresh))){
    dir.create(as.character(thresh))
  }
  
  ## Fetching the data of the co-ocurrence matrices
  cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))
  link <- lapply(alllinks, function(x) read.table(x, header = T, check.names = F,
                                                  stringsAsFactors = F))
  node <- lapply(allnodes, function(x) read.table(x, header = T, check.names = F,
                                                  stringsAsFactors = F))
  
  for(bb in 1:length(cooc)){
    rownames(cooc[[bb]]) <- cooc[[bb]][,1]
    cooc[[bb]][,1] <- NULL
    if(thresh > 0){
      # rare co-occurring terms
      rareT <- which(rowSums(cooc[[bb]]) <= ceiling(max(rowSums(cooc[[bb]])) * thresh))
      # frequent co-occ terms
      freqT <- which(rowSums(cooc[[bb]]) > ceiling(max(rowSums(cooc[[bb]])) * thresh))
      # rare terms only co-occurring with rare terms
      #finalRare <- which(colSums(cooc[[bb]][freqT,]) == 0)
      if(length(rareT)>0){
        cooc[[bb]] <- cooc[[bb]][-rareT,-rareT]
      }
    } else if(thresh < 0){
      # rare co-occurring terms
      rareT <- which(rowSums(cooc[[bb]]) <= ceiling(max(rowSums(cooc[[bb]])) * (-1 * thresh)))
      # frequent co-occ terms
      freqT <- which(rowSums(cooc[[bb]]) > ceiling(max(rowSums(cooc[[bb]])) * (-1 * thresh)))
      # rare terms only co-occurring with rare terms
      #finalRare <- which(colSums(cooc[[bb]][freqT,]) == 0)
      if(length(rareT)>0){
        cooc[[bb]] <- cooc[[bb]][rareT,rareT]
      }
    }
  }
  
  
  
  index <- 0
  for (sent in cooc) {
    index <- index + 1
    ## cluster text
    #sent[lower.tri(sent)] <- 0
    #Mydissimilarity_10values_r <- smacof::sim2diss(cor(cooc[[1]]), method = "rank", to.dist = T)
    write.table(sent, paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"_network_matrix.csv"),sep=" ")
    test <- Rtsne::Rtsne(sent,check_duplicates=FALSE,
                         pca=TRUE, perplexity = max(1,floor(nrow(sent)/3)-1), 
                         theta=0, dims=2)
    
    result = tryCatch({
      loadings2 <- NULL
      loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
      write.table(loadings2$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-2factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
      write.table(loadings2$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-2factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings2})
    
    result = tryCatch({
      loadings3 <- NULL
      loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
      write.table(loadings3$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-3factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
      write.table(loadings3$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-3factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings3})
    
    result = tryCatch({
      loadings4 <- NULL
      loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
      write.table(loadings4$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-4factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
      write.table(loadings4$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-4factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings4})  
    
    result = tryCatch({
      loadings5 <- NULL
      loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
      write.table(loadings5$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-5factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
      write.table(loadings5$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-5factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings5})  
    
    result = tryCatch({
      loadings6 <- NULL
      loadings6 <- psych::fa(cor(sent),nfactors = 6,fm="minres",rotate = "varimax")
      write.table(loadings6$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-6factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings6 <- psych::fa(cor(sent),nfactors = 6,fm="minres",rotate = "varimax")
      write.table(loadings6$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-6factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings6})
    
    result = tryCatch({
      loadings7 <- NULL
      loadings7 <- psych::fa(cor(sent),nfactors = 7,fm="minres",rotate = "varimax")
      write.table(loadings7$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-7factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings7 <- psych::fa(cor(sent),nfactors = 7,fm="minres",rotate = "varimax")
      write.table(loadings7$loadings,paste0(thresh,"/",gsub("\\_links\\.csv","",alllinks[[index]]),"-7factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings7)})
  }
}


sense <- read.table("0.05/Sense_and_Sensibility-3factors.csv",header = T,sep=";")

fctrs <- colnames(sense)

sense_res <- as.data.frame(matrix(nrow=nrow(sense),ncol=2))

for(nxt in 1:nrow(sense)){
  sense_res[nxt,1]<-rownames(sense)[nxt]
  sense_res[nxt,2]<-names(which.max(sense[nxt,1:3]))
}

#plot(as.factor(sense_res$V2),as.factor(sense_res$V1))
plot(plyr::count(sense_res$V2)$x,plyr::count(sense_res$V2)$freq)
