pda500Dict <- read.table("resources/pda500-trait-matches.csv",sep=",",header = T)
pda500Dict$Term <- tolower(pda500Dict$Term)

pda1710Dict <- read.table("resources/pda1710_no_abbreviation_loadings_categories.csv",sep=",",header = T)
pda1710Dict$Word<-tolower(pda1710Dict$Word)

#add the max trait variable without loading threshold
pda1710Dict$maxValTrait <- ""
pda1710Dict$thresholdValTrait <- ""
for(i in 1:nrow(pda1710Dict)){
  pda1710Dict$maxValTrait[i] <- names(which.max(abs(pda1710Dict[i,5:9])))
  pda1710Dict$thresholdValTrait[i] <- ifelse(length(which(abs(pda1710Dict[i,5:9])>.25))>0,names(which.max(abs(pda1710Dict[i,5:9]))),"")
}

valueDict <- wordVal <- read.table("resources/values.txt",sep=";")

allportDict <- gsub("\\s*", "", tolower(readLines('resources/Personal Traits.txt')))
allportDict2 <- gsub("\\s*", "", tolower(readLines('resources/Metaphorical Doubtful.txt')))
allportDict3 <- gsub("\\s*", "", tolower(readLines('resources/Social Evaluations.txt')))
allportDict4 <- gsub("\\s*", "", tolower(readLines('resources/Temporary States.txt')))


## PDA500 to VALUES
print(paste0("PDA500 to Value coverage: ",length(which(pda500Dict$Term %in% valueDict$V1))/nrow(pda500Dict)))

plyr::count(as.character(pda500Dict[which(pda500Dict$Term %in% valueDict$V1),2]))

## PDA1710 to VALUE
print(paste0("PDA500 to Value coverage: ",length(which(pda1710Dict$Word %in% valueDict$V1))/nrow(pda1710Dict)))

plyr::count(as.character(pda1710Dict[which(pda1710Dict$Word %in% valueDict$V1),11]))

## ALLPORT to VALUES
print(paste0("Allport to Value coverage: ",length(which(allportDict %in% valueDict$V1))/length(allportDict)))
print(paste0("Allport Meta to Value coverage: ",length(which(allportDict2 %in% valueDict$V1))/length(allportDict2)))
print(paste0("Allport Social to Value coverage: ",length(which(allportDict3 %in% valueDict$V1))/length(allportDict3)))
print(paste0("Allport Temp to Value coverage: ",length(which(allportDict4 %in% valueDict$V1))/length(allportDict4)))

## ALLPORT to 500
print(paste0("Allport to PDA500 coverage: ",length(which(allportDict %in% pda500Dict$Term))/length(allportDict)))
print(paste0("PDA500 to Allport coverage: ",length(which(pda500Dict$Term %in% allportDict))/length(pda500Dict$Term)))

print(paste0("Allport Meta to PDA500 coverage: ",length(which(allportDict2 %in% pda500Dict$Term))/length(allportDict2)))
print(paste0("PDA500 to Allport Meta coverage: ",length(which(pda500Dict$Term %in% allportDict2))/length(pda500Dict$Term)))

print(paste0("Allport Social to PDA500 coverage: ",length(which(allportDict3 %in% pda500Dict$Term))/length(allportDict3)))
print(paste0("PDA500 to Allport Social coverage: ",length(which(pda500Dict$Term %in% allportDict3))/length(pda500Dict$Term)))

print(paste0("Allport Temp to PDA500 coverage: ",length(which(allportDict4 %in% pda500Dict$Term))/length(allportDict4)))
print(paste0("PDA500 to Allport Temp coverage: ",length(which(pda500Dict$Term %in% allportDict4))/length(pda500Dict$Term)))

## ALLPORT to 1710
print(paste0("Allport to PDA1710 coverage: ",length(which(allportDict %in% pda1710Dict$Word))/length(allportDict)))
print(paste0("PDA1710 to Allport coverage: ",length(which(pda1710Dict$Word %in% allportDict))/length(pda1710Dict$Word)))

print(paste0("Allport Meta to PDA1710 coverage: ",length(which(allportDict2 %in% pda1710Dict$Word))/length(allportDict2)))
print(paste0("PDA1710 to Allport Meta coverage: ",length(which(pda1710Dict$Word %in% allportDict2))/length(pda1710Dict$Word)))

print(paste0("Allport Social to PDA1710 coverage: ",length(which(allportDict3 %in% pda1710Dict$Word))/length(allportDict3)))
print(paste0("PDA1710 to Allport Social coverage: ",length(which(pda1710Dict$Word %in% allportDict3))/length(pda1710Dict$Word)))

print(paste0("Allport Temp to PDA1710 coverage: ",length(which(allportDict4 %in% pda1710Dict$Word))/length(allportDict4)))
print(paste0("PDA1710 to Allport Temp coverage: ",length(which(pda1710Dict$Word %in% allportDict4))/length(pda1710Dict$Word)))

## 500 to 1710
print(paste0("PDA500 to PDA1710 coverage: ",length(which(pda500Dict$Term %in% pda1710Dict$Word))/length(pda500Dict$Term)))
print(paste0("PDA1710 to PDA500 coverage: ",length(which(pda1710Dict$Word %in% pda500Dict$Term))/length(pda1710Dict$Word)))

## ALL TRAITS to VALUES

allTraitDict <- unique(c(pda500Dict$Term,pda1710Dict$Word,allportDict))
print(paste0("All traits to Value coverage: ",length(which(allTraitDict %in% valueDict$V1))/length(allTraitDict)))


write.csv2(allTraitDict,"resources/combinedTraitDict.txt",row.names = F,col.names = F)

# ALL words in all dictionaries

allWordsDict <- unique(c(pda500Dict$Term,pda1710Dict$Word,allportDict,as.character(valueDict$V1)))
write.csv2(allWordsDict,"resources/combinedWordDict.txt",row.names = F,col.names = F)
