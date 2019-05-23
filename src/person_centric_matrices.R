

setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/all-adjectives-1000words-person-centric/")

alllinks <- list.files(paste0("."),
                       pattern = "(.*)_links.csv",
                       full.names = T)

allnodes <- list.files(paste0("."),
                       pattern = "(.*)_nodes.csv",
                       full.names = T)

link <-
  lapply(alllinks, function(x)
    read.table(
      x,
      header = T,
      check.names = F,
      stringsAsFactors = F
    ))

node <-
  lapply(allnodes, function(x)
    read.table(
      x,
      header = T,
      check.names = F,
      stringsAsFactors = F
    ))




socN1 <- list()

terms <- c()

for (lin in 1:nrow(node[[1]])) {
  nex <- unlist(strsplit(node[[1]]$tags[lin], ", "))
  
  pairings <- unlist(strsplit(nex,split = "::"))
  
  if (length(pairings) > 2) {
    
    pairings <- data.frame(person=pairings[seq(1,length(pairings)-1,by = 2)],term=pairings[seq(2,length(pairings),by = 2)],stringsAsFactors = F)
    
    if(length(unique(pairings$term))>1){
      terms <- unique(c(terms,unique(pairings$term)))
      
      pairings <- aggregate(term ~ person, pairings, paste, collapse=", ")
      
      for(i in 1:nrow(pairings)){
        if(is.null(socN1[[pairings[i,1]]])){
          socN1[[pairings[i,1]]] <- list(pairings[i,2])
        } else{
          socN1[[pairings[i,1]]][[length(socN1[[pairings[i,1]]])+1]] <- pairings[i,2]
        }
      }
    }
  }
}

terms <- sort(terms)

allCoMat <- matrix(nrow=length(socN1),ncol=length(terms),0)
colnames(allCoMat) <- terms
rownames(allCoMat) <- names(socN1)

allPersMats <- list()

for(i in 1:length(socN1)){
  for(j in 1:length(socN1[[i]])){
    templin <- unlist(strsplit(socN1[[i]][[j]], ', '))
    
    allCoMat[names(socN1)[i],templin] <- allCoMat[names(socN1)[i],templin] + 1
    
    fillTerms <- gtools::combinations(length(templin), 2, templin)
    
    if(length(allPersMats[[names(socN1)[i]]])>0){
      for(k in 1:nrow(fillTerms)){
        allPersMats[[names(socN1)[i]]][fillTerms[k,1],fillTerms[k,2]] <- allPersMats[[names(socN1)[i]]][fillTerms[k,1],fillTerms[k,2]] + 1
        allPersMats[[names(socN1)[i]]][fillTerms[k,2],fillTerms[k,1]] <- allPersMats[[names(socN1)[i]]][fillTerms[k,2],fillTerms[k,1]] + 1
      }
    } else{
      allPersMats[[names(socN1)[i]]] <- matrix(nrow=length(terms),ncol=length(terms),0)
      colnames(allPersMats[[names(socN1)[i]]]) <- terms
      rownames(allPersMats[[names(socN1)[i]]]) <- terms
      
      for(k in 1:nrow(fillTerms)){
        allPersMats[[names(socN1)[i]]][fillTerms[k,1],fillTerms[k,2]] <- allPersMats[[names(socN1)[i]]][fillTerms[k,1],fillTerms[k,2]] + 1
        allPersMats[[names(socN1)[i]]][fillTerms[k,2],fillTerms[k,1]] <- allPersMats[[names(socN1)[i]]][fillTerms[k,2],fillTerms[k,1]] + 1
      }
    }
  }
  allPersMats[[names(socN1)[i]]] <- allPersMats[[names(socN1)[i]]][-which(rowSums(allPersMats[[names(socN1)[i]]])==0),-which(rowSums(allPersMats[[names(socN1)[i]]])==0)]
}

#factors
psych::fa(cor(allCoMat),nfactors = 5,fm="minres",rotate = "varimax")
psych::principal(cor(allCoMat),nfactors = 5,rotate = "varimax")

#mds on all terms?
library(smacof)
library(ggplot2)
library(ggdendro)
library(dplyr)
Mydissimilarity_10values_r <- smacof::sim2diss(cor(allCoMat), method = "rank", to.dist = T)
MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")

MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
MDSmod_10values_r$stress
plot(MDSmod_10values_r, main = "1710 // Rank", xlab = "Dimension 1", ylab = "Dimension 2")

#mds 

termCoMat <- matrix(nrow=length(terms),ncol=length(terms),0)
colnames(termCoMat) <- terms
rownames(termCoMat) <- terms

for(i in 1:nrow(allCoMat)){
  tmpTerms <- names(which(allCoMat[i,]>0))
  tmpTerms <- gtools::combinations(length(tmpTerms), 2, tmpTerms)
  
  for(j in 1:nrow(tmpTerms)){
    termCoMat[tmpTerms[j,1],tmpTerms[j,2]] <- termCoMat[tmpTerms[j,1],tmpTerms[j,2]] + 1
    termCoMat[tmpTerms[j,2],tmpTerms[j,1]] <- termCoMat[tmpTerms[j,2],tmpTerms[j,1]] + 1
  }
}

#mds 500

sent <- termCoMat
sent[lower.tri(sent)] <- 0

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
#MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
#plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
#text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
MDSmod_10values_r$stress
plotdata <- as.data.frame(MDSmod_10values_r$conf)
plotdata$names <- rownames(MDSmod_10values_r$conf)

ggplot(plotdata, aes(D1, D2, label=names)) + 
  geom_point(size=2.3) +
  geom_text(check_overlap = TRUE, size=2.2, 
            hjust = "center", vjust = "bottom", nudge_x = 0, nudge_y = 0.015) +
  labs(x="", y="", title="MDS by Rank PDA 500") + theme_bw()


allDistances <- list()
allDistances[["A-C"]] <- dist(rbind(MDSmod_10values_r$conf[1,],MDSmod_10values_r$conf[2,]),method = "euclid")
allDistances[["A-E"]] <- dist(rbind(MDSmod_10values_r$conf[1,],MDSmod_10values_r$conf[3,]),method = "euclid")
allDistances[["A-N"]] <- dist(rbind(MDSmod_10values_r$conf[1,],MDSmod_10values_r$conf[4,]),method = "euclid")
allDistances[["A-O"]] <- dist(rbind(MDSmod_10values_r$conf[1,],MDSmod_10values_r$conf[5,]),method = "euclid")
allDistances[["C-E"]] <- dist(rbind(MDSmod_10values_r$conf[2,],MDSmod_10values_r$conf[3,]),method = "euclid")
allDistances[["C-N"]] <- dist(rbind(MDSmod_10values_r$conf[2,],MDSmod_10values_r$conf[4,]),method = "euclid")
allDistances[["C-O"]] <- dist(rbind(MDSmod_10values_r$conf[2,],MDSmod_10values_r$conf[5,]),method = "euclid")
allDistances[["E-N"]] <- dist(rbind(MDSmod_10values_r$conf[3,],MDSmod_10values_r$conf[4,]),method = "euclid")
allDistances[["E-O"]] <- dist(rbind(MDSmod_10values_r$conf[3,],MDSmod_10values_r$conf[5,]),method = "euclid")
allDistances[["N-O"]] <- dist(rbind(MDSmod_10values_r$conf[4,],MDSmod_10values_r$conf[5,]),method = "euclid")
barplot(unlist(allDistances))

#mds 1710

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
#MDS_ordinal_10values_r <- mds(Mydissimilarity_10values_r, type="ordinal")
#plot(MDS_ordinal_10values_r, plot.type = "Shepard", main="Ordinal")
#text(55,.02, paste("Stress =", round(MDS_ordinal_10values_r$stress,2)))
MDSmod_10values_r <- mds(Mydissimilarity_10values_r, ndim = 2, type = c("ordinal"))
MDSmod_10values_r$stress
plotdata <- as.data.frame(MDSmod_10values_r$conf)
plotdata$names <- rownames(MDSmod_10values_r$conf)

ggplot(plotdata, aes(D1, D2, label=names)) + 
  geom_point(size=2.3) +
  geom_text(check_overlap = TRUE, size=2.2, 
            hjust = "center", vjust = "bottom", nudge_x = 0, nudge_y = 0.015) +
  labs(x="", y="", title="MDS by Rank PDA 1710") + theme_bw()


allDistances <- list()
allDistances[["A-C"]] <- dist(rbind(MDSmod_10values_r$conf[1,],MDSmod_10values_r$conf[2,]),method = "euclid")
allDistances[["A-E"]] <- dist(rbind(MDSmod_10values_r$conf[1,],MDSmod_10values_r$conf[3,]),method = "euclid")
allDistances[["A-N"]] <- dist(rbind(MDSmod_10values_r$conf[1,],MDSmod_10values_r$conf[4,]),method = "euclid")
allDistances[["A-O"]] <- dist(rbind(MDSmod_10values_r$conf[1,],MDSmod_10values_r$conf[5,]),method = "euclid")
allDistances[["C-E"]] <- dist(rbind(MDSmod_10values_r$conf[2,],MDSmod_10values_r$conf[3,]),method = "euclid")
allDistances[["C-N"]] <- dist(rbind(MDSmod_10values_r$conf[2,],MDSmod_10values_r$conf[4,]),method = "euclid")
allDistances[["C-O"]] <- dist(rbind(MDSmod_10values_r$conf[2,],MDSmod_10values_r$conf[5,]),method = "euclid")
allDistances[["E-N"]] <- dist(rbind(MDSmod_10values_r$conf[3,],MDSmod_10values_r$conf[4,]),method = "euclid")
allDistances[["E-O"]] <- dist(rbind(MDSmod_10values_r$conf[3,],MDSmod_10values_r$conf[5,]),method = "euclid")
allDistances[["N-O"]] <- dist(rbind(MDSmod_10values_r$conf[4,],MDSmod_10values_r$conf[5,]),method = "euclid")
barplot(unlist(allDistances))



##### TESTS 



gr.jac <- smacof::sim2diss(cor(traitCo), method = "rank", to.dist = T)
hc <- hclust(gr.jac, method="ward.D2")
cut <- as.data.frame(cutree(hc, k=5))
names(cut) <- "cut"
cut$names <- rownames(cut)

hcdata <- dendro_data(hc, type="triangle")
hcdata$labels <- left_join(hcdata$labels, cut, by=c("label"="names"))

plotdata <- as.data.frame(MDSmod_10values_r$conf)
plotdata$names <- rownames(MDSmod_10values_r$conf)

ggplot(plotdata, aes(D1, D2, label=names)) + 
  geom_point(size=2.3) +
  geom_text(check_overlap = TRUE, size=2.2, 
            hjust = "center", vjust = "bottom", nudge_x = 0, nudge_y = 0.015) +
  labs(x="", y="", title="MDS by Rank PDA 1710") + theme_bw()
