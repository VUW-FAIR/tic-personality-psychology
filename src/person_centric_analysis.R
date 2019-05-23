library(tidyverse)

setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/combined-allwords-dictionary-sentence-person-centric/")
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


## Fetching the data of the co-ocurrence matrices
cooc <- lapply(allmatrices, function(x) read.table(x, header = T, check.names = F))
link <- lapply(alllinks, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))
node <- lapply(allnodes, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))

for(bb in 1:length(cooc)){
  rownames(cooc[[bb]]) <- cooc[[bb]][,1]
  cooc[[bb]][,1] <- NULL
}


nod <- node[[23]]

charDS <- sapply(nod$tags[which(nod$tags!="")], function(x){
  unlist(strsplit(x[[1]],split = ", "))
})

charDS <- unlist(flatten(charDS))
names(charDS) <- c()

pda500Dict <- read.table("../../resources/pda500-trait-matches.csv",sep=",",header = T)
pda500Dict$Term <- tolower(pda500Dict$Term)
pda500Dict$Primary<-as.character(pda500Dict$Primary)


pda1710Dict <- read.table("../../resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
pda1710Dict$Word<-tolower(pda1710Dict$Word)

#add the max trait variable without loading threshold
pda1710Dict$maxValTrait <- ""
pda1710Dict$thresholdValTrait <- ""
for(i in 1:nrow(pda1710Dict)){
  pda1710Dict$maxValTrait[i] <- names(which.max(abs(pda1710Dict[i,5:9])))
  pda1710Dict$thresholdValTrait[i] <- ifelse(length(which(abs(pda1710Dict[i,5:9])>.25))>0,names(which.max(abs(pda1710Dict[i,5:9]))),"")
}

valueDict <- wordVal <- read.table("../../resources/values.txt",sep=";")

charPers <- list()

for(i in 1:length(charDS)){
  pers_term <- unlist(strsplit(charDS[i],split = "::"))
  if(is.null(charPers[[pers_term[1]]])){
    charPers[[pers_term[1]]] <- list("A"=0,"C"=0,"E"=0,"O"=0,"N"=0,"-"=0)
    if(length(which(pda500Dict$Term==pers_term[2])) > 0){
      charPers[[pers_term[1]]][[pda500Dict[which(pda500Dict$Term==pers_term[2])[1],2]]] <- 1
    } else{
      charPers[[pers_term[1]]][["-"]] <- 1
    }
    
  } else{
    if(length(which(pda500Dict$Term==pers_term[2])) > 0){
      charPers[[pers_term[1]]][[pda500Dict[which(pda500Dict$Term==pers_term[2])[1],2]]] <- charPers[[pers_term[1]]][[pda500Dict[which(pda500Dict$Term==pers_term[2])[1],2]]] + 1
    } else{
      charPers[[pers_term[1]]][["-"]] <- charPers[[pers_term[1]]][["-"]] + 1
    }
  }
}

data=data.frame(person=character(6*length(charPers)),trait=character(6*length(charPers)),count=character(6*length(charPers)),stringsAsFactors = F)

index <- 0

for(pers in names(charPers)){
  data$person[c((6*index+1):(6*index+6))] <- pers
  
  data$trait[(6*index+1)] <- "A"
  data$trait[(6*index+2)] <- "C"
  data$trait[(6*index+3)] <- "E"
  data$trait[(6*index+4)] <- "O"
  data$trait[(6*index+5)] <- "N"
  data$trait[(6*index+6)] <- "-"
  
  data$count[(6*index+1)] <- charPers[[pers]]$A
  data$count[(6*index+2)] <- charPers[[pers]]$C
  data$count[(6*index+3)] <- charPers[[pers]]$E
  data$count[(6*index+4)] <- charPers[[pers]]$O
  data$count[(6*index+5)] <- charPers[[pers]]$N
  data$count[(6*index+6)] <- charPers[[pers]]$'-'
  
  index <- index + 1
}

# ggplot(data, aes(y=count, x=trait, color=trait, fill=trait)) + 
#   geom_bar(stat="identity") +    
#   facet_wrap(~person)
# 
# ggplot(data, aes(fill=person, y=count, x=trait)) + 
#   geom_bar( stat="identity")


pda500Dict <- read.table("../../resources/pda500-trait-matches.csv",sep=",",header = T)
pda500Dict$Term <- tolower(pda500Dict$Term)
pda500Dict$Primary<-as.character(pda500Dict$Primary)


pda1710Dict <- read.table("../../resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
pda1710Dict$Word<-tolower(pda1710Dict$Word)

#add the max trait variable without loading threshold
pda1710Dict$maxValTrait <- ""
pda1710Dict$thresholdValTrait <- ""
for(i in 1:nrow(pda1710Dict)){
  pda1710Dict$maxValTrait[i] <- names(which.max(abs(pda1710Dict[i,5:9])))
  pda1710Dict$thresholdValTrait[i] <- ifelse(length(which(abs(pda1710Dict[i,5:9])>.25))>0,names(which.max(abs(pda1710Dict[i,5:9]))),"")
}

valueDict <- wordVal <- read.table("../../resources/values.txt",sep=";")

finalCharDS <- c()

for(nod in node){
  charDS <- sapply(nod$tags[which(nod$tags!="")], function(x){
    unlist(strsplit(x[[1]],split = ", "))
  })
  
  charDS <- unlist(flatten(charDS))
  names(charDS) <- c()
  finalCharDS <- c(finalCharDS,charDS)
}

allTerms <- list()
charTerms <- list()

for(i in 1:length(finalCharDS)){
  pers_term <- unlist(strsplit(finalCharDS[i],split = "::"))
  allTerms[[length(allTerms)+1]]<-pers_term[2]
  if(is.null(charTerms[[pers_term[1]]])){
    charTerms[[pers_term[1]]] <- list()
    charTerms[[pers_term[1]]][[1]]<-pers_term[2]
  } else{
    charTerms[[pers_term[1]]][[length(charTerms[[pers_term[1]]])+1]]<-pers_term[2]
  }
}

allTerms <- unique(unlist(flatten(allTerms)))

outMat <- matrix(nrow=length(charTerms),ncol=length(allTerms),0)
rownames(outMat)<-names(charTerms)
colnames(outMat)<-allTerms

for(pers in names(charTerms)){
  pTerms <- unlist(charTerms[[pers]])
  outMat[pers,pTerms] <- outMat[pers,pTerms] + 1
}


df <- readr::read_csv("/Users/mlr/Downloads/SenseMatrix.csv")
df_corr <- cor(df)
#paran::paran(df_corr)
#psych::scree(df)
#psych::fa.parallel(df_corr)
df_corr <- psych::cor.smoother(df_corr)
#df_corr <- psych::cor.smooth(df_corr)


psych::principal(df_corr, nfactors = 6, rotate = "varimax") %>%
  psych::print.psych(., sort = T, cut = .40)




