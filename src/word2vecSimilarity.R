library(wordVectors)

library(tidyverse)
library(gridExtra)
library(smacof)

setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/")
allTextFiles <- list.files("resources/Text Files")

cobinedBook<-""
sWords <- c("a", "about", "above", "across", "after", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves", "the")

for(nextRun in 1:length(allTextFiles)){
  theSource <- gsub(' ','_', gsub('[[:digit:]][[:digit:]] ','',
                                  gsub(' text.txt','',allTextFiles[nextRun])))
  
  sourceText <- readChar(paste0('resources/Text Files/',allTextFiles[nextRun]),
                         file.info(paste0('resources/Text Files/',allTextFiles[nextRun]))$size)
  processedText <- gsub("\\r\\n", " ", tolower(sourceText), perl = T)
  processedText <- gsub("\\n", " ", processedText, perl = T)
  processedText <- gsub("\\s", " ", processedText, perl = T)
  processedText <- gsub("[[:punct:]]", " ", processedText, perl = T)
  processedText <- gsub("[[:digit:]]", " ", processedText, perl = T)
  j <- tm::Corpus(tm::VectorSource(processedText))
  jj <- tm::tm_map(j, tm::removeWords, tm::stopwords('en'))
  write(toString(jj[[1]]),file="resources/ourCorpus.txt",append=TRUE)
}

prep_word2vec(origin="resources/ourCorpus.txt",destination="resources/ourCorpusPrep.txt",lowercase=T,bundle_ngrams=2)

if (!file.exists("resources/ourCorpus.bin")) {model = train_word2vec("resources/ourCorpusPrep.txt","resources/ourCorpus.bin",vectors=300,threads=4,window=12,iter=5,negative_samples=0)} else model = read.vectors("resources/ourCorpus.bin")
#model = read.vectors("../../src/Java/Coding Features/data/word2vec.out")

#get all trait terms from a book

folders <- list("pda1710-1000words-advs-lemma-book-centric",
                "pda500-1000words-advs-lemma-book-centric",
                "allport-1000words-personal-traits-advs-lemma-book-centric",
                "value-dict-1000words-advs-lemma-book-centric",
                "combinedTraits-1000words-advs-lemma-book-centric",
                "random-from-book-1000words-all-words-book-centric",
                "combined-all-dict-1000words-book-centric")


#folders <- list("pda1710-1000words-advs-lemma-book-centric")

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
      
      euclid <- cooc[[bb]]
      
      # matrix(nrow=nrow(cooc[[1]]),ncol=nrow(cooc[[1]]),unlist(cooc[[1]]))
      thePairs<-list()
      flattened1<-list()
      flattened2<-list()
      for(i in 1:nrow(cooc[[bb]])){
        for(j in 1:nrow(cooc[[bb]])){
          thePairs[[length(thePairs)+1]] <- list(colnames(cooc[[bb]])[i],rownames(cooc[[bb]])[j])
        }
      }
      
      for(k in 1:length(thePairs)){
        termSim1 <- model[[thePairs[[k]][[2]]]]@.Data[1,]
        termSim2 <- model[[thePairs[[k]][[1]]]]@.Data[1,]
        if(thePairs[[k]][[2]] != thePairs[[k]][[1]] & (!is.nan(sum(termSim1)) & !is.nan(sum(termSim2)))){
          flattened1[[length(flattened1)+1]] <- cosineSimilarity(model[[thePairs[[k]][[2]]]],model[[thePairs[[k]][[1]]]])[1,1]
          flattened2[[length(flattened2)+1]] <- dist(rbind(termSim1,termSim2))[1]
        } else if(thePairs[[k]][[2]] == thePairs[[k]][[1]]){
          flattened1[[length(flattened1)+1]] <- 1
          flattened2[[length(flattened2)+1]] <-  0
        }
        else{
          if(nrow(termSim)==1){
            flattened1[[length(flattened1)+1]] <- -1
            flattened2[[length(flattened2)+1]] <- -1
          }else{
            flattened1[[length(flattened1)+1]] <- -1
            flattened2[[length(flattened2)+1]] <- -1
          }
        }
      }
      
      newM1 <- matrix(nrow=nrow(cooc[[bb]]),ncol=nrow(cooc[[bb]]),flattened1)
      rownames(newM1)<-rownames(cooc[[bb]])
      colnames(newM1)<-rownames(cooc[[bb]])
      
      newM2 <- matrix(nrow=nrow(cooc[[bb]]),ncol=nrow(cooc[[bb]]),flattened2)
      rownames(newM2)<-rownames(cooc[[bb]])
      colnames(newM2)<-rownames(cooc[[bb]])
      
      write.table(newM1,paste0("../",fold,"-",bb,"word2vec_distance_matrix.csv"),sep=";",row.names = T,col.names = T)
      write.table(newM2,paste0("../",fold,"-",bb,"word2vec_distance_matrix_euclidian.csv"),sep=";",row.names = T,col.names = T)
      
      sent <- cooc[[bb]]
      
      noVariTerms <- which(rownames(sent)==names(which(colSums(sent)==0)))
      if(length(noVariTerms)>0){
        sent <- sent[-noVariTerms,-noVariTerms]
      }
      
      ### factor analysis
      
      tryCatch({
        loadings2 <- NULL
        loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
        write.table(loadings2$loadings,paste0("../",fold,"-2factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings2 <- psych::fa(cor(sent),nfactors = 2,fm="minres",rotate = "varimax")
        write.table(loadings2$loadings,paste0("../",fold,"-2factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={loadings2})
      
      tryCatch({
        loadings3 <- NULL
        loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
        write.table(loadings3$loadings,paste0("../",fold,"-3factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings3 <- psych::fa(cor(sent),nfactors = 3,fm="minres",rotate = "varimax")
        write.table(loadings3$loadings,paste0("../",fold,"-3factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={loadings3})
      
      tryCatch({
        loadings4 <- NULL
        loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
        write.table(loadings4$loadings,paste0("../",fold,"-4factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings4 <- psych::fa(cor(sent),nfactors = 4,fm="minres",rotate = "varimax")
        write.table(loadings4$loadings,paste0("../",fold,"-4factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={loadings4})  
      
      tryCatch({
        loadings5 <- NULL
        loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
        write.table(loadings5$loadings,paste0("../",fold,"-5factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings5 <- psych::fa(cor(sent),nfactors = 5,fm="minres",rotate = "varimax")
        write.table(loadings5$loadings,paste0("../",fold,"-5factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={loadings5})  
      
      tryCatch({
        loadings6 <- NULL
        loadings6 <- psych::fa(cor(sent),nfactors = 6,fm="minres",rotate = "varimax")
        write.table(loadings6$loadings,paste0("../",fold,"-6factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings6 <- psych::fa(cor(sent),nfactors = 6,fm="minres",rotate = "varimax")
        write.table(loadings6$loadings,paste0("../",fold,"-6factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={loadings6})
      
      tryCatch({
        loadings7 <- NULL
        loadings7 <- psych::fa(cor(sent),nfactors = 7,fm="minres",rotate = "varimax")
        write.table(loadings7$loadings,paste0("../",fold,"-7factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings7 <- psych::fa(cor(sent),nfactors = 7,fm="minres",rotate = "varimax")
        write.table(loadings7$loadings,paste0("../",fold,"-7factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={rm(loadings7)})
      
      tryCatch({
        loadings8 <- NULL
        loadings8 <- psych::fa(cor(sent),nfactors = 8,fm="minres",rotate = "varimax")
        write.table(loadings8$loadings,paste0("../",fold,"-8factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings8 <- psych::fa(cor(sent),nfactors = 8,fm="minres",rotate = "varimax")
        write.table(loadings8$loadings,paste0("../",fold,"-8factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={rm(loadings8)})
      tryCatch({
        loadings9 <- NULL
        loadings9 <- psych::fa(cor(sent),nfactors = 9,fm="minres",rotate = "varimax")
        write.table(loadings9$loadings,paste0("../",fold,"-9factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings9 <- psych::fa(cor(sent),nfactors = 9,fm="minres",rotate = "varimax")
        write.table(loadings9$loadings,paste0("../",fold,"-9factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={rm(loadings9)})
      tryCatch({
        loadings10 <- NULL
        loadings10 <- psych::fa(cor(sent),nfactors = 10,fm="minres",rotate = "varimax")
        write.table(loadings10$loadings,paste0("../",fold,"-10factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings10 <- psych::fa(cor(sent),nfactors = 10,fm="minres",rotate = "varimax")
        write.table(loadings10$loadings,paste0("../",fold,"-10factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={rm(loadings10)})  
      tryCatch({
        loadings11 <- NULL
        loadings11 <- psych::fa(cor(sent),nfactors = 11,fm="minres",rotate = "varimax")
        write.table(loadings11$loadings,paste0("../",fold,"-11factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings11 <- psych::fa(cor(sent),nfactors = 11,fm="minres",rotate = "varimax")
        write.table(loadings11$loadings,paste0("../",fold,"-11factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={rm(loadings11)})  
      tryCatch({
        loadings12 <- NULL
        loadings12 <- psych::fa(cor(sent),nfactors = 12,fm="minres",rotate = "varimax")
        write.table(loadings12$loadings,paste0("../",fold,"-12factors-word2vec-dist.csv"),sep = ";")
      }, warning = function(warning_condition) {
        loadings12 <- psych::fa(cor(sent),nfactors = 12,fm="minres",rotate = "varimax")
        write.table(loadings12$loadings,paste0("../",fold,"-12factors-word2vec-dist.csv"),sep = ";")
      }, error = function(error_condition) {}, finally={rm(loadings12)})
      
      
      sent[lower.tri(sent)] <- 0
      
      
      ###MDS on traits PDA 500
      # 
      # wordTrait <- read.table("../../resources/pda500-trait-matches.csv",sep=",",header = T)
      # wordTrait$Term<-tolower(wordTrait$Term)
      # 
      # traitCo <- matrix(nrow=5,ncol=5,data = 0)
      # rownames(traitCo)<-c("A","C","E","N","O")
      # colnames(traitCo)<-c("A","C","E","N","O")
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- as.character(wordTrait$Primary[which(wordTrait$Term==rownames(sent)[i])][1])
      #     colTerm <- as.character(wordTrait$Primary[which(wordTrait$Term==colnames(sent)[j])][1])
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- PDA 500 all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- PDA 500 all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # ###MDS on traits PDA 500 randomized
      # traitCo <- matrix(nrow=5,ncol=5,data = 0)
      # rownames(traitCo)<-c("A","C","E","N","O")
      # colnames(traitCo)<-c("A","C","E","N","O")
      # baseTraits<-c("A","C","E","N","O")
      # traitCounts <- plyr::count(wordTrait$Primary)
      # traitCounts <- traitCounts[-which(traitCounts$x=="-"),]
      # traitProbs <- traitCounts$freq/sum(traitCounts$freq)
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- sample(baseTraits, 1)
      #     colTerm <- sample(baseTraits, 1)
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- PDA 500 all books random trait matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- PDA 500 all books random trait matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- sample(baseTraits, 1,prob = traitProbs)
      #     colTerm <- sample(baseTraits, 1,prob = traitProbs)
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- PDA 500 all books random trait matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- PDA 500 all books random trait matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # 
      # ## MDS on traits 1710
      # 
      # wordTrait <- read.table("../../resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
      # wordTrait$Word<-tolower(wordTrait$Word)
      # 
      # #add the max trait variable without loading threshold
      # wordTrait$maxValTrait <- ""
      # wordTrait$thresholdValTrait <- ""
      # for(i in 1:nrow(wordTrait)){
      #   wordTrait$maxValTrait[i] <- names(which.max(abs(wordTrait[i,5:9])))
      #   wordTrait$thresholdValTrait[i] <- ifelse(length(which(abs(wordTrait[i,5:9])>.25))>0,names(which.max(abs(wordTrait[i,5:9]))),"")
      # }
      # 
      # traitCo <- matrix(nrow=5,ncol=5,data = 0)
      # rownames(traitCo)<-c("A","C","E","N","O")
      # colnames(traitCo)<-c("A","C","E","N","O")
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- as.character(wordTrait$thresholdValTrait[which(wordTrait$Word==rownames(sent)[i])][1])
      #     colTerm <- as.character(wordTrait$thresholdValTrait[which(wordTrait$Word==colnames(sent)[j])][1])
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "" & colTerm != "")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- PDA 1710 all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- PDA 1710 all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # 
      # ###MDS on traits randomized
      # traitCo <- matrix(nrow=5,ncol=5,data = 0)
      # rownames(traitCo)<-c("A","C","E","N","O")
      # colnames(traitCo)<-c("A","C","E","N","O")
      # baseTraits<-c("A","C","E","N","O")
      # traitCounts <- plyr::count(wordTrait$thresholdValTrait)
      # traitCounts <- traitCounts[-which(traitCounts$x==""),]
      # traitProbs <- traitCounts$freq/sum(traitCounts$freq)
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- sample(baseTraits, 1)
      #     colTerm <- sample(baseTraits, 1)
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- PDA 1710 all books random trait matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- PDA 1710 all books random trait matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- sample(baseTraits, 1,prob = traitProbs)
      #     colTerm <- sample(baseTraits, 1,prob = traitProbs)
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- PDA 1710 all books random trait matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- PDA 1710 all books random trait matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # 
      # 
      # 
      # #### MDS on VALUES
      # 
      # wordVal <- read.table("../../resources/values.txt",sep=";")
      # wordVal$V1 <- as.character(wordVal$V1)
      # wordVal$V2 <- as.character(wordVal$V2)
      # wordValCount<-plyr::count(wordVal$V1)
      # if(length(which(wordValCount$freq>1))>0){
      #   redTerms <- as.character(wordValCount[which(wordValCount$freq>1),1])
      #   wordVal <- wordVal[-which(wordVal$V1 %in% as.character(wordValCount[which(wordValCount$freq>1),1])),]
      # }
      # 
      # traitCo <- matrix(nrow=10,ncol=10,data = 0)
      # rownames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      # colnames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- as.character(wordVal$V2[which(wordVal$V1==rownames(sent)[i])][1])
      #     colTerm <- as.character(wordVal$V2[which(wordVal$V1==colnames(sent)[j])][1])
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- Values all books // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- Values all books // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # 
      # 
      # 
      # traitCo <- matrix(nrow=10,ncol=10,data = 0)
      # rownames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      # colnames(traitCo)<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      # baseTraits<-c("AC","BE","CO","HE","PO","SE","SD","ST","TR","UN")
      # traitCounts <- plyr::count(wordVal$V2)
      # #traitCounts <- traitCounts[-which(traitCounts$x=="-"),]
      # traitProbs <- traitCounts$freq/sum(traitCounts$freq)
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- sample(baseTraits, 1)
      #     colTerm <- sample(baseTraits, 1)
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- Values all books randomised value matching // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- Values all books randomised value matching // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # 
      # for(i in 1:nrow(sent)){
      #   for(j in 1:nrow(sent)){
      #     rowTerm <- sample(baseTraits, 1,prob = traitProbs)
      #     colTerm <- sample(baseTraits, 1,prob = traitProbs)
      #     crossVal <- sent[i,j]
      #     if(rowTerm!=colTerm & (!is.na(rowTerm) & !is.na(colTerm)) & (rowTerm != "-" & colTerm != "-")){
      #       traitCo[rowTerm,colTerm] <- traitCo[rowTerm,colTerm] + crossVal
      #       traitCo[colTerm,rowTerm] <- traitCo[colTerm,rowTerm] + crossVal
      #     }
      #   }
      # }
      # 
      # Mydissimilarity_10values_r <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
      # MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
      # plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
      # MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_r$stress
      # plot(MDSmod_10values_r, main = paste0(fold,"- Values all books randomised value matching with probability distribution // Rank"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
      # Mydissimilarity_10values_c <- sim2diss(cor(traitCo), method = "corr", to.dist = T)
      # MDS_ordinal_10values_c <- mds(Mydissimilarity_10values_c, type="ordinal")
      # plot(MDS_ordinal_10values_c, plot.type = "Shepard", main="Ordinal")
      # text(55,.02, paste("Stress =", round(MDS_ordinal_10values_c$stress,2)))
      # MDSmod_10values_c <- mds(Mydissimilarity_10values_c, ndim = 2, type = c("ordinal"))
      # MDSmod_10values_c$stress
      # plot(MDSmod_10values_c, main = paste0(fold,"- Values all books randomised value matching with probability distribution // Correlation"), xlab = "Dimension 1", ylab = "Dimension 2")
      # 
    }
  }
}
