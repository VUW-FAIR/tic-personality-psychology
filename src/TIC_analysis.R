
setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/pda1710-1000words-advs-lemma-book-centric/")

tempFeatures <- read.table("Sense_and_Sensibility_temporal_statistics.csv",header = F,sep=";")

#plot entropy
plot(tempFeatures$V1,tempFeatures$V4,type = 'l',xlab="Slice index",ylab="Shannon entropy")
plot(tempFeatures$V1,tempFeatures$V5,type = 'l',xlab="Slice index",ylab="Pielou evenness")
plot(tempFeatures$V1,tempFeatures$V8,type = 'l',xlab="Slice index",ylab="TIC Modularity")

termFeatures <- read.table("Sense_and_Sensibility-allFeatures-output.csv",header = T,sep=";")

#recurring terms
which(termFeatures$recs>0)