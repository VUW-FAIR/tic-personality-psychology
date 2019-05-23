setwd(paste0("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/outputs save/pda500-1000words-advs-lemma-book-centric/"))


allnodes <- list.files(paste0("."),
                       pattern = "(.*)_nodes.csv",
                       full.names = T)

node <- lapply(allnodes, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))

len <- list()

for(nod in node){
  len[[length(len)+1]] <- nrow(nod)
}

