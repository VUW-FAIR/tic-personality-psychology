library(tidyverse)
library(gridExtra)
library(smacof)

combineBooks <- function(book_ids, cooc) {
  # Collapse cooc matrix for books belonging to same author into single matrix
  # - Read in files, put all words into a data structure
  # - Create new matrix based on number of unique words
  # - Add data into new matrix from each book's original matrix
  book_words <- NULL
  for (i in book_ids) {
    book_words <- c(book_words, rownames(cooc[[i]]))
  }
  book_words <- unique(book_words)
  
  resList <- list()
  
  # Merge cooc matrices
  combined_cooc <- matrix(0, nrow = length(book_words), ncol = length(book_words))
  rownames(combined_cooc)<-book_words
  colnames(combined_cooc)<-book_words
  
  combined_cooc2 <- matrix(0, nrow = length(book_words), ncol = length(book_words))
  rownames(combined_cooc2)<-book_words
  colnames(combined_cooc2)<-book_words
  
  for(i in 1:nrow(combined_cooc)){
    for(j in 1:nrow(combined_cooc)){
      nextIndexVal <- 0
      for(k in book_ids){
        nextmat <- cooc[[k]]
        if(length(which(rownames(nextmat)==rownames(combined_cooc)[i])) > 0 & length(which(colnames(nextmat)==colnames(combined_cooc)[j]))>0){
          nextIndexVal <- nextIndexVal + nextmat[which(rownames(nextmat)==rownames(combined_cooc)[i]),which(colnames(nextmat)==colnames(combined_cooc)[j])]
        }
      }
      combined_cooc[i,j] <- nextIndexVal
      combined_cooc2[i,j] <- ceiling(nextIndexVal/length(book_ids))
    }
  }
  
  resList[[1]] <- combined_cooc
  resList[[2]] <- combined_cooc2
  
  return (resList)
}

folders <- list("allport-1000words-personal-traits-advs-lemma-book-centric",
                "value-dict-1000words-advs-lemma-book-centric",
                "pda1710-1000words-advs-lemma-book-centric",
                "pda500-1000words-advs-lemma-book-centric",
                "combinedTraits-1000words-advs-lemma-book-centric",
                "random-from-book-1000words-all-words-book-centric")

for(fold in folders){
  setwd(paste0("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/",fold))
  
  
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
  
  thresholds <- list(0)
  
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
  }
  
  
  austen_books <- c(7,13,17,20,22,23)
  dickens_books <- c(1,2,3,5,6,10,11,12,14,15,16,18,19,21,24)
  
  alllinks_extended <- list()
  book_names_extended <- list()
  cooc_modified <- cooc
  
  cooc_modified2 <- cooc
  
  # Add to cooc data
  comBooks <- combineBooks(austen_books, cooc)
  
  cooc_modified[[length(cooc_modified)+1]] <- comBooks[[1]]
  cooc_modified2[[length(cooc_modified2)+1]] <- comBooks[[2]]
  #book_names_extended[[length(book_names_extended)+1]] <- alllinks_extended[[length(alllinks_extended)+1]] <- "All Jane Austen Books"
  
  comBooks <- combineBooks(dickens_books, cooc)
  
  cooc_modified[[length(cooc_modified)+1]] <- comBooks[[1]]
  cooc_modified2[[length(cooc_modified2)+1]] <- comBooks[[2]]
  #book_names_extended[[length(book_names_extended)+1]] <- alllinks_extended[[length(alllinks_extended)+1]] <- "All Charles Dickens Books"
  
  comBooks <- combineBooks(c(1:25), cooc)
  
  cooc_modified[[length(cooc_modified)+1]] <- comBooks[[1]]
  cooc_modified2[[length(cooc_modified2)+1]] <- comBooks[[2]]
  #book_names_extended[[length(book_names_extended)+1]] <- alllinks_extended[[length(alllinks_extended)+1]] <- "All Books"
  
  write.table(cooc_modified[[26]],paste0("../",fold,"combined_austen_cooccurrence_matrix.csv"),sep=";",row.names = T,col.names = T)
  write.table(cooc_modified[[27]],paste0("../",fold,"combined_dickens_cooccurrence_matrix.csv"),sep=";",row.names = T,col.names = T)
  write.table(cooc_modified[[28]],paste0("../",fold,"combined_all_books_cooccurrence_matrix.csv"),sep=";",row.names = T,col.names = T)
  
  #cooc_modified2[[length(cooc_modified2)+1]] <- combineBooksAvg(austen_books, cooc)
  #cooc_modified2[[length(cooc_modified2)+1]] <- combineBooksAvg(dickens_books, cooc)
  #cooc_modified2[[length(cooc_modified2)+1]] <- combineBooksAvg(c(1:25), cooc)
  
  write.table(cooc_modified2[[26]],paste0("../",fold,"combined_austen_cooccurrence_matrix_avg.csv"),sep=";",row.names = T,col.names = T)
  write.table(cooc_modified2[[27]],paste0("../",fold,"combined_dickens_cooccurrence_matrix_avg.csv"),sep=";",row.names = T,col.names = T)
  write.table(cooc_modified2[[28]],paste0("../",fold,"combined_all_books_cooccurrence_matrix_avg.csv"),sep=";",row.names = T,col.names = T)
  
  for(nextCombi in c(26,27,28)){
    sent <- cooc_modified[[nextCombi]]
    
    labCombi<-""
    if(nextCombi==26){
      labCombi<-"Austen"
    } else if(nextCombi==27){
      labCombi<-"Dickens"
    } else{
      labCombi<-"AllBooks"
    }
    ### factor analysis
    
    tryCatch({
      loadings2 <- NULL
      loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
      write.table(loadings2$loadings,paste0("../",fold,"-",labCombi,"-2factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
      write.table(loadings2$loadings,paste0("../",fold,"-",labCombi,"-2factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings2})
    
    tryCatch({
      loadings3 <- NULL
      loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
      write.table(loadings3$loadings,paste0("../",fold,"-",labCombi,"-3factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
      write.table(loadings3$loadings,paste0("../",fold,"-",labCombi,"-3factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings3})
    
    tryCatch({
      loadings4 <- NULL
      loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
      write.table(loadings4$loadings,paste0("../",fold,"-",labCombi,"-4factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
      write.table(loadings4$loadings,paste0("../",fold,"-",labCombi,"-4factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings4})  
    
    tryCatch({
      loadings5 <- NULL
      loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
      write.table(loadings5$loadings,paste0("../",fold,"-",labCombi,"-5factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
      write.table(loadings5$loadings,paste0("../",fold,"-",labCombi,"-5factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings5})  
    
    tryCatch({
      loadings6 <- NULL
      loadings6 <- psych::fa(cor(sent),nfactors = 6,fm="minres",rotate = "varimax")
      write.table(loadings6$loadings,paste0("../",fold,"-",labCombi,"-6factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings6 <- psych::fa(cor(sent),nfactors = 6,fm="minres",rotate = "varimax")
      write.table(loadings6$loadings,paste0("../",fold,"-",labCombi,"-6factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings6})
    
    tryCatch({
      loadings7 <- NULL
      loadings7 <- psych::fa(cor(sent),nfactors = 7,fm="minres",rotate = "varimax")
      write.table(loadings7$loadings,paste0("../",fold,"-",labCombi,"-7factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings7 <- psych::fa(cor(sent),nfactors = 7,fm="minres",rotate = "varimax")
      write.table(loadings7$loadings,paste0("../",fold,"-",labCombi,"-7factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings7)})
    
    tryCatch({
      loadings8 <- NULL
      loadings8 <- psych::fa(cor(sent),nfactors = 8,fm="minres",rotate = "varimax")
      write.table(loadings8$loadings,paste0("../",fold,"-",labCombi,"-8factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings8 <- psych::fa(cor(sent),nfactors = 8,fm="minres",rotate = "varimax")
      write.table(loadings8$loadings,paste0("../",fold,"-",labCombi,"-8factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings8)})
    tryCatch({
      loadings9 <- NULL
      loadings9 <- psych::fa(cor(sent),nfactors = 9,fm="minres",rotate = "varimax")
      write.table(loadings9$loadings,paste0("../",fold,"-",labCombi,"-9factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings9 <- psych::fa(cor(sent),nfactors = 9,fm="minres",rotate = "varimax")
      write.table(loadings9$loadings,paste0("../",fold,"-",labCombi,"-9factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings9)})
    tryCatch({
      loadings10 <- NULL
      loadings10 <- psych::fa(cor(sent),nfactors = 10,fm="minres",rotate = "varimax")
      write.table(loadings10$loadings,paste0("../",fold,"-",labCombi,"-10factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings10 <- psych::fa(cor(sent),nfactors = 10,fm="minres",rotate = "varimax")
      write.table(loadings10$loadings,paste0("../",fold,"-",labCombi,"-10factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings10)})  
    tryCatch({
      loadings11 <- NULL
      loadings11 <- psych::fa(cor(sent),nfactors = 11,fm="minres",rotate = "varimax")
      write.table(loadings11$loadings,paste0("../",fold,"-",labCombi,"-11factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings11 <- psych::fa(cor(sent),nfactors = 11,fm="minres",rotate = "varimax")
      write.table(loadings11$loadings,paste0("../",fold,"-",labCombi,"-11factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings11)})  
    tryCatch({
      loadings12 <- NULL
      loadings12 <- psych::fa(cor(sent),nfactors = 12,fm="minres",rotate = "varimax")
      write.table(loadings12$loadings,paste0("../",fold,"-",labCombi,"-12factors.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings12 <- psych::fa(cor(sent),nfactors = 12,fm="minres",rotate = "varimax")
      write.table(loadings12$loadings,paste0("../",fold,"-",labCombi,"-12factors.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings12)})
    
    
    sent[lower.tri(sent)] <- 0
    
    
    ###MDS on traits PDA 500
    
    wordTrait <- read.table("../../resources/pda500-trait-matches.csv",sep=",",header = T)
    wordTrait$Term<-tolower(wordTrait$Term)
    
    traitCo <- matrix(nrow=5,ncol=5,data = 0)
    rownames(traitCo)<-c("A","C","E","N","O")
    colnames(traitCo)<-c("A","C","E","N","O")
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- as.character(wordTrait$Primary[which(wordTrait$Term==rownames(sent)[i])][1])
        colTerm <- as.character(wordTrait$Primary[which(wordTrait$Term==colnames(sent)[j])][1])
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 500 all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 500 all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    ###MDS on traits PDA 500 randomized
    traitCo <- matrix(nrow=5,ncol=5,data = 0)
    rownames(traitCo)<-c("A","C","E","N","O")
    colnames(traitCo)<-c("A","C","E","N","O")
    baseTraits<-c("A","C","E","N","O")
    traitCounts <- plyr::count(wordTrait$Primary)
    traitCounts <- traitCounts[-which(traitCounts$x=="-"),]
    traitProbs <- traitCounts$freq/sum(traitCounts$freq)
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1)
        colTerm <- sample(baseTraits, 1)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 500 all books random trait matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 500 all books random trait matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1,prob = traitProbs)
        colTerm <- sample(baseTraits, 1,prob = traitProbs)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 500 all books random trait matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 500 all books random trait matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    ## MDS on traits 1710
    
    wordTrait <- read.table("../../resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
    wordTrait$Word<-tolower(wordTrait$Word)
    
    #add the max trait variable without loading threshold
    wordTrait$maxValTrait <- ""
    wordTrait$thresholdValTrait <- ""
    for(i in 1:nrow(wordTrait)){
      wordTrait$maxValTrait[i] <- names(which.max(abs(wordTrait[i,5:9])))
      wordTrait$thresholdValTrait[i] <- ifelse(length(which(abs(wordTrait[i,5:9])>.25))>0,names(which.max(abs(wordTrait[i,5:9]))),"")
    }
    
    traitCo <- matrix(nrow=5,ncol=5,data = 0)
    rownames(traitCo)<-c("A","C","E","N","O")
    colnames(traitCo)<-c("A","C","E","N","O")
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- as.character(wordTrait$thresholdValTrait[which(wordTrait$Word==rownames(sent)[i])][1])
        colTerm <- as.character(wordTrait$thresholdValTrait[which(wordTrait$Word==colnames(sent)[j])][1])
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "" & colTerm != "")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 1710 all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 1710 all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    ###MDS on traits randomized
    traitCo <- matrix(nrow=5,ncol=5,data = 0)
    rownames(traitCo)<-c("A","C","E","N","O")
    colnames(traitCo)<-c("A","C","E","N","O")
    baseTraits<-c("A","C","E","N","O")
    traitCounts <- plyr::count(wordTrait$thresholdValTrait)
    traitCounts <- traitCounts[-which(traitCounts$x==""),]
    traitProbs <- traitCounts$freq/sum(traitCounts$freq)
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1)
        colTerm <- sample(baseTraits, 1)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 1710 all books random trait matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 1710 all books random trait matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1,prob = traitProbs)
        colTerm <- sample(baseTraits, 1,prob = traitProbs)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 1710 all books random trait matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 1710 all books random trait matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    
    
    #### MDS on VALUES
    
    wordVal <- read.table("../../resources/values.txt",sep=";")
    wordVal$V1 <- as.character(wordVal$V1)
    wordVal$V2 <- as.character(wordVal$V2)
    wordValCount<-plyr::count(wordVal$V1)
    if(length(which(wordValCount$freq>1))>0){
      redTerms <- as.character(wordValCount[which(wordValCount$freq>1),1])
      wordVal <- wordVal[-which(wordVal$V1 %in% as.character(wordValCount[which(wordValCount$freq>1),1])),]
    }
    
    traitCo <- matrix(nrow=10,ncol=10,data = 0)
    rownames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    colnames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- as.character(wordVal$V2[which(wordVal$V1==rownames(sent)[i])][1])
        colTerm <- as.character(wordVal$V2[which(wordVal$V1==colnames(sent)[j])][1])
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," Values all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," Values all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    
    
    traitCo <- matrix(nrow=10,ncol=10,data = 0)
    rownames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    colnames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    baseTraits<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    traitCounts <- plyr::count(wordVal$V2)
    #traitCounts <- traitCounts[-which(traitCounts$x=="-"),]
    traitProbs <- traitCounts$freq/sum(traitCounts$freq)
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1)
        colTerm <- sample(baseTraits, 1)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," Values all books randomised value matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," Values all books randomised value matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1,prob = traitProbs)
        colTerm <- sample(baseTraits, 1,prob = traitProbs)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," Values all books randomised value matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," Values all books randomised value matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    
    
    
    sent <- cooc_modified2[[nextCombi]]
    
    
    ### factor analysis
    
    tryCatch({
      loadings2 <- NULL
      loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
      write.table(loadings2$loadings,paste0("../",fold,"-",labCombi,"-2factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
      write.table(loadings2$loadings,paste0("../",fold,"-",labCombi,"-2factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings2})
    
    tryCatch({
      loadings3 <- NULL
      loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
      write.table(loadings3$loadings,paste0("../",fold,"-",labCombi,"-3factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
      write.table(loadings3$loadings,paste0("../",fold,"-",labCombi,"-3factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings3})
    
    tryCatch({
      loadings4 <- NULL
      loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
      write.table(loadings4$loadings,paste0("../",fold,"-",labCombi,"-4factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
      write.table(loadings4$loadings,paste0("../",fold,"-",labCombi,"-4factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings4})  
    
    tryCatch({
      loadings5 <- NULL
      loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
      write.table(loadings5$loadings,paste0("../",fold,"-",labCombi,"-5factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
      write.table(loadings5$loadings,paste0("../",fold,"-",labCombi,"-5factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings5})  
    
    tryCatch({
      loadings6 <- NULL
      loadings6 <- psych::fa(cor(sent),nfactors = 6,fm="minres",rotate = "varimax")
      write.table(loadings6$loadings,paste0("../",fold,"-",labCombi,"-6factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings6 <- psych::fa(cor(sent),nfactors = 6,fm="minres",rotate = "varimax")
      write.table(loadings6$loadings,paste0("../",fold,"-",labCombi,"-6factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={loadings6})
    
    tryCatch({
      loadings7 <- NULL
      loadings7 <- psych::fa(cor(sent),nfactors = 7,fm="minres",rotate = "varimax")
      write.table(loadings7$loadings,paste0("../",fold,"-",labCombi,"-7factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings7 <- psych::fa(cor(sent),nfactors = 7,fm="minres",rotate = "varimax")
      write.table(loadings7$loadings,paste0("../",fold,"-",labCombi,"-7factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings7)})
    
    tryCatch({
      loadings8 <- NULL
      loadings8 <- psych::fa(cor(sent),nfactors = 8,fm="minres",rotate = "varimax")
      write.table(loadings8$loadings,paste0("../",fold,"-",labCombi,"-8factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings8 <- psych::fa(cor(sent),nfactors = 8,fm="minres",rotate = "varimax")
      write.table(loadings8$loadings,paste0("../",fold,"-",labCombi,"-8factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings8)})
    tryCatch({
      loadings9 <- NULL
      loadings9 <- psych::fa(cor(sent),nfactors = 9,fm="minres",rotate = "varimax")
      write.table(loadings9$loadings,paste0("../",fold,"-",labCombi,"-9factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings9 <- psych::fa(cor(sent),nfactors = 9,fm="minres",rotate = "varimax")
      write.table(loadings9$loadings,paste0("../",fold,"-",labCombi,"-9factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings9)})
    tryCatch({
      loadings10 <- NULL
      loadings10 <- psych::fa(cor(sent),nfactors = 10,fm="minres",rotate = "varimax")
      write.table(loadings10$loadings,paste0("../",fold,"-",labCombi,"-10factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings10 <- psych::fa(cor(sent),nfactors = 10,fm="minres",rotate = "varimax")
      write.table(loadings10$loadings,paste0("../",fold,"-",labCombi,"-10factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings10)})  
    tryCatch({
      loadings11 <- NULL
      loadings11 <- psych::fa(cor(sent),nfactors = 11,fm="minres",rotate = "varimax")
      write.table(loadings11$loadings,paste0("../",fold,"-",labCombi,"-11factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings11 <- psych::fa(cor(sent),nfactors = 11,fm="minres",rotate = "varimax")
      write.table(loadings11$loadings,paste0("../",fold,"-",labCombi,"-11factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings11)})  
    tryCatch({
      loadings12 <- NULL
      loadings12 <- psych::fa(cor(sent),nfactors = 12,fm="minres",rotate = "varimax")
      write.table(loadings12$loadings,paste0("../",fold,"-",labCombi,"-12factors_avg.csv"),sep = ";")
    }, warning = function(warning_condition) {
      loadings12 <- psych::fa(cor(sent),nfactors = 12,fm="minres",rotate = "varimax")
      write.table(loadings12$loadings,paste0("../",fold,"-",labCombi,"-12factors_avg.csv"),sep = ";")
    }, error = function(error_condition) {}, finally={rm(loadings12)})
    
    
    sent[lower.tri(sent)] <- 0
    
    
    ###MDS on traits PDA 500
    
    wordTrait <- read.table("../../resources/pda500-trait-matches.csv",sep=",",header = T)
    wordTrait$Term<-tolower(wordTrait$Term)
    
    traitCo <- matrix(nrow=5,ncol=5,data = 0)
    rownames(traitCo)<-c("A","C","E","N","O")
    colnames(traitCo)<-c("A","C","E","N","O")
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- as.character(wordTrait$Primary[which(wordTrait$Term==rownames(sent)[i])][1])
        colTerm <- as.character(wordTrait$Primary[which(wordTrait$Term==colnames(sent)[j])][1])
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 500 all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 500 all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    ###MDS on traits PDA 500 randomized
    traitCo <- matrix(nrow=5,ncol=5,data = 0)
    rownames(traitCo)<-c("A","C","E","N","O")
    colnames(traitCo)<-c("A","C","E","N","O")
    baseTraits<-c("A","C","E","N","O")
    traitCounts <- plyr::count(wordTrait$Primary)
    traitCounts <- traitCounts[-which(traitCounts$x=="-"),]
    traitProbs <- traitCounts$freq/sum(traitCounts$freq)
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1)
        colTerm <- sample(baseTraits, 1)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 500 all books random trait matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 500 all books random trait matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1,prob = traitProbs)
        colTerm <- sample(baseTraits, 1,prob = traitProbs)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 500 all books random trait matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 500 all books random trait matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    ## MDS on traits 1710
    
    wordTrait <- read.table("../../resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
    wordTrait$Word<-tolower(wordTrait$Word)
    
    #add the max trait variable without loading threshold
    wordTrait$maxValTrait <- ""
    wordTrait$thresholdValTrait <- ""
    for(i in 1:nrow(wordTrait)){
      wordTrait$maxValTrait[i] <- names(which.max(abs(wordTrait[i,5:9])))
      wordTrait$thresholdValTrait[i] <- ifelse(length(which(abs(wordTrait[i,5:9])>.25))>0,names(which.max(abs(wordTrait[i,5:9]))),"")
    }
    
    traitCo <- matrix(nrow=5,ncol=5,data = 0)
    rownames(traitCo)<-c("A","C","E","N","O")
    colnames(traitCo)<-c("A","C","E","N","O")
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- as.character(wordTrait$thresholdValTrait[which(wordTrait$Word==rownames(sent)[i])][1])
        colTerm <- as.character(wordTrait$thresholdValTrait[which(wordTrait$Word==colnames(sent)[j])][1])
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "" & colTerm != "")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 1710 all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 1710 all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    ###MDS on traits randomized
    traitCo <- matrix(nrow=5,ncol=5,data = 0)
    rownames(traitCo)<-c("A","C","E","N","O")
    colnames(traitCo)<-c("A","C","E","N","O")
    baseTraits<-c("A","C","E","N","O")
    traitCounts <- plyr::count(wordTrait$thresholdValTrait)
    traitCounts <- traitCounts[-which(traitCounts$x==""),]
    traitProbs <- traitCounts$freq/sum(traitCounts$freq)
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1)
        colTerm <- sample(baseTraits, 1)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 1710 all books random trait matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 1710 all books random trait matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1,prob = traitProbs)
        colTerm <- sample(baseTraits, 1,prob = traitProbs)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," PDA 1710 all books random trait matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," PDA 1710 all books random trait matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    
    
    #### MDS on VALUES
    
    wordVal <- read.table("../../resources/values.txt",sep=";")
    wordVal$V1 <- as.character(wordVal$V1)
    wordVal$V2 <- as.character(wordVal$V2)
    wordValCount<-plyr::count(wordVal$V1)
    if(length(which(wordValCount$freq>1))>0){
      redTerms <- as.character(wordValCount[which(wordValCount$freq>1),1])
      wordVal <- wordVal[-which(wordVal$V1 %in% as.character(wordValCount[which(wordValCount$freq>1),1])),]
    }
    
    traitCo <- matrix(nrow=10,ncol=10,data = 0)
    rownames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    colnames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- as.character(wordVal$V2[which(wordVal$V1==rownames(sent)[i])][1])
        colTerm <- as.character(wordVal$V2[which(wordVal$V1==colnames(sent)[j])][1])
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," Values all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," Values all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    
    
    traitCo <- matrix(nrow=10,ncol=10,data = 0)
    rownames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    colnames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    baseTraits<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
    traitCounts <- plyr::count(wordVal$V2)
    #traitCounts <- traitCounts[-which(traitCounts$x=="-"),]
    traitProbs <- traitCounts$freq/sum(traitCounts$freq)
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1)
        colTerm <- sample(baseTraits, 1)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," Values all books randomised value matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," Values all books randomised value matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    
    for(i in 1:nrow(sent)){
      for(j in 1:nrow(sent)){
        rowTerm <- sample(baseTraits, 1,prob = traitProbs)
        colTerm <- sample(baseTraits, 1,prob = traitProbs)
        crossVal <- sent[i,j]
        if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
          traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
          traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
        }
      }
    }
    
    Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
    MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
    plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
    MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
    MDSmod_10values_r$stress
    plot(MDSmod_10values_r, main = paste0(fold,"-",labCombi," Values all books randomised value matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
    
    Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
    MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
    plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
    text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
    MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
    MDSmod_10values_c$stress
    plot(MDSmod_10values_c, main = paste0(fold,"-",labCombi," Values all books randomised value matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
  }
}
