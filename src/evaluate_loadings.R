library(fpc)
library(vegan)
library(psych)

m1 <- as.matrix(read.table("0/Sense_and_Sensibility-5factors.csv",header = T,sep=";"))
m2 <- as.matrix(read.table("-0.6/Sense_and_Sensibility-5factors.csv",header = T,sep=";"))

matched <- rownames(m1)[which(rownames(m1) %in% rownames(m2))]

m1 <- m1[matched,]
m2 <- m2[matched,]

#Perform rotation
rotation <- protest(m1, m2)
#Produces same result
#rotation2 <- MCMCpack::procrustes(m1, m2, translation = TRUE, dilation = TRUE)


rotation$signif
rotation$Yrot
diag(fa.congruence(rotation$Yrot, m1))