setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Projects/2018-psychological-trait-words-cascades/natureoutput/allAdjectives-all-slices/sentence/")

allnodes <- list.files(paste0("."),
                       pattern = "(.*)_nodes.csv",
                       full.names = T)
node <- lapply(allnodes, function(x) read.table(x, header = T, check.names = F,
                                                stringsAsFactors = F))
index <- 1
for(nod in node){
  combinations <- data.frame(id1=character(0),id2=character(0))
  for(i in 1:nrow(nod)){
    tmp <- unlist(strsplit(nod[i,2],", "))
    combs <- iterpc::getall(iterpc::iterpc(length(tmp),2,labels = tmp))
    combinations <- rbind(combinations,data.frame(id1=combs[,1],id2=combs[,2]))
  }
  combinations <- plyr::count(combinations,id1 ~ id2)
  colnames(combinations) <- c("id1","id2","label")
  h <- igraph::graph.data.frame(combinations, directed = F)
  igraph::E(h)$weight <- igraph::E(h)$label
  netm <- igraph::as_adjacency_matrix(h, attr = "weight", sparse = F)
  colnames(netm) <- igraph::V(h)$name
  rownames(netm) <- igraph::V(h)$name
  write.table(netm, paste0(gsub("_nodes\\.csv|\\.|/","",allnodes[index]),"_network_matrix.csv"), 
              col.names = NA, row.names = T,
              fileEncoding = "UTF-8",
              sep = " ")
  index <- index + 1
}

