library(poweRlaw)
setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/pda500-1000words-advs-lemma-book-centric/")
### load the combined book matrices

combined1710 <- read.table("../pda1710-1000words-advs-lemma-book-centriccombined_all_books_cooccurrence_matrix.csv",sep=";",header = T)
colnames(combined1710)<-rownames(combined1710)

combined500 <- read.table("../pda500-1000words-advs-lemma-book-centriccombined_all_books_cooccurrence_matrix.csv",sep=";",header = T)
colnames(combined500)<-rownames(combined500)

combinedValues <- read.table("../value-dict-1000words-advs-lemma-book-centriccombined_all_books_cooccurrence_matrix.csv",sep=";",header = T)
colnames(combinedValues)<-rownames(combinedValues)

combinedAllTraits <- read.table("../combinedTraits-1000words-advs-lemma-book-centriccombined_all_books_cooccurrence_matrix.csv",sep=";",header = T)
colnames(combinedAllTraits)<-rownames(combinedAllTraits)

combinedAllport <- read.table("../allport-1000words-personal-traits-advs-lemma-book-centriccombined_all_books_cooccurrence_matrix.csv",sep=";",header = T)
colnames(combinedAllTraits)<-rownames(combinedAllTraits)


x<-plyr::count(colSums(combinedAllport))$freq
x1 <- plyr::count(x)
plot(x1$freq,x1$x,xlab="Observed frequency of term recurrence",ylab="Term recurrence")
abline(h=max(x1$x)*0.8,col="blue")
arrows(0, max(x1$x)*0.8, 0, max(x1$x)*0.8-1, length = 0.1,col="blue")
text(0, max(x1$x)*0.8-2,labels = ceiling(max(x1$x)*0.8),col="blue",cex=0.8)
abline(h=max(x1$x)*0.6,col="blue")
arrows(0, max(x1$x)*0.6, 0, max(x1$x)*0.6-1, length = 0.1,col="blue")
text(0, max(x1$x)*0.6-2,labels = ceiling(max(x1$x)*0.6),col="blue",cex=0.8)
abline(h=max(x1$x)*0.4,col="blue")
arrows(0, max(x1$x)*0.4, 0, max(x1$x)*0.4-1, length = 0.1,col="blue")
text(0, max(x1$x)*0.4-2,labels = ceiling(max(x1$x)*0.4),col="blue",cex=0.8)
abline(h=max(x1$x)*0.2,col="blue")
arrows(0, max(x1$x)*0.2, 0, max(x1$x)*0.2-1, length = 0.1,col="blue")
text(0, max(x1$x)*0.2-2,labels = ceiling(max(x1$x)*0.2),col="blue",cex=0.8)

abline(h=max(x1$x)*0.05,col="red")
arrows(max(x1$freq), max(x1$x)*0.05, max(x1$freq), max(x1$x)*0.05+1, length = 0.1,col="red")
text(max(x1$freq), max(x1$x)*0.05+2,labels = ceiling(max(x1$x)*0.05),col="red",cex=0.8)
abline(h=max(x1$x)*0.15,col="red")
arrows(max(x1$freq), max(x1$x)*0.15, max(x1$freq), max(x1$x)*0.15+1, length = 0.1,col="red")
text(max(x1$freq), max(x1$x)*0.15+2,labels = ceiling(max(x1$x)*0.15),col="red",cex=0.8)
abline(h=max(x1$x)*0.25,col="red")
arrows(max(x1$freq), max(x1$x)*0.25, max(x1$freq), max(x1$x)*0.25+1, length = 0.1,col="red")
text(max(x1$freq), max(x1$x)*0.25+2,labels = ceiling(max(x1$x)*0.25),col="red",cex=0.8)

plot(x1$freq,x1$x,xlab="Observed frequency of term recurrence",ylab="Term recurrence",log="xy")


########################################################
##Continuous power law #
########################################################
m1 = conpl$new(x)
m1$setXmin(estimate_xmin(m1))
########################################################
##Exponential
########################################################
m2 = conexp$new(x)
m2$setXmin(m1$getXmin())
est2 = estimate_pars(m2)
m2$setPars(est2$pars)
########################################################
##Vuong's test #
########################################################
comp = compare_distributions(m1, m2)
plot(comp)


P = ecdf(x)
plot(P)
