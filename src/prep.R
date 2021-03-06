# Preprocessing script for a tool prototype that enables analysis capable of identifying salient psychological and narrative 
# dimensions associated with cultural values, personality, emotions and morality in texts
# For demonstration purposes the tool is configured to work on various Victorian Novels from Charles Dickens and other authors.
# Authors: Markus Luczak-Roesch, Tom Goldfinch, Johannes A. Karl

# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
#                                               "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

#set working directoy
setwd("/Users/mlr/OneDrive - Victoria University of Wellington - STAFF/Git/tic-personality-words/")

#Rprof(profile_out <- "profile.txt")

#import libs
#coreNLP::initCoreNLP(type = c("english"),mem="96g")

#housekeeping and helpers
options(scipen = 999)
allwords <- FALSE
stemTrait <- FALSE
personCentric <- TRUE
# Functions ---------------------------------------------------------------


degree.distribution <- function (graph, cumulative = FALSE, ...) 
{
  if (!igraph::is.igraph(graph)) {
    stop("Not a graph object")
  }
  cs <- igraph::degree(graph, ...)
  hi <- hist(cs, -1:max(cs), plot = FALSE)$count
  if (!cumulative) {
    res <- hi
  }
  else {
    res <- rev(cumsum(rev(hi)))
  }
  res
}
## TIC
tic_generate <- function(inputsequence) {
  nodes <- c()
  links <- c()
  roots <- c()
  last_node <- list()
  
  for(pp in 1:nrow(inputsequence)){
    tags <- unlist(strsplit(as.character(inputsequence[which(inputsequence[,1] == pp),2]), split=", "))
    nodes <- rbind(nodes, c(inputsequence[which(inputsequence[,1]== pp),1],
                            as.character(inputsequence[which(inputsequence[,1] == pp),2]),
                            inputsequence[which(inputsequence[,1] == pp),1]))
    
    for(jj in 1:length(tags)){
      cur_tag <- tags[jj]
      if(!is.null(unlist(last_node[cur_tag]))){ 
        source_node <- last_node[cur_tag]
        target_node <- pp
        links <- rbind(links, c(source_node, target_node, cur_tag))
      } else {
        roots <- rbind(roots, c(pp, cur_tag))
      }
      last_node[cur_tag] <- pp
    }
    
  }
  return(list(nodes, links, roots))
}

#get all text and char files
allTextFiles <- list.files("resources/Text Files")

for(sliceSize in list("sentence")){
#for(sliceSize in list(1000)){
#for(sliceSize in list(1000,"sentence")){
  dir.create(file.path("resources/output/", sliceSize), showWarnings = FALSE)
  
  for(nextRun in 1:length(allTextFiles)){
    theSource <- gsub(' ','_', gsub('[[:digit:]][[:digit:]] ','',
                                    gsub(' text.txt','',allTextFiles[nextRun])))
    
    sourceText <- readChar(paste0('resources/Text Files/',allTextFiles[nextRun]),
                           file.info(paste0('resources/Text Files/',allTextFiles[nextRun]))$size)
    processedText <- gsub("\\r\\n", " ", sourceText, perl = T)
    processedText <- gsub("\\n", " ", processedText, perl = T)
    
    
    #split by number of words and chapters
    if(is.numeric(sliceSize)){
      full_text <- strsplit(processedText, "(?i:Chapter [0-9A-Z]+[\\.]?[\\s.]?)", perl=T)[[1]]
      #corpus <- tm::VCorpus(tm::VectorSource(processedText))
      #corpus <- tm::tm_map(corpus, tm::content_transformer(tm::removePunctuation))
      #corpus <- tm::tm_map(corpus, tm::content_transformer(tm::removeNumbers))
      #tdm <- as.matrix(tm::TermDocumentMatrix(corpus,
      #                                        control = list(wordLength = c(3, Inf))))
      #random500 <- sample(rownames(tdm), 500)
      #readr::write_lines(random500, paste0("resources/output/", theSource,"_random.csv"))
      words300B <- c()
      spacyr::spacy_initialize()
      tmp <- lapply(full_text, function(x){
        snippet <- strsplit(x, "\\s+")[[1]]
        if(length(snippet > 1)){
          groupA <- rep(seq(ceiling(length(snippet) / sliceSize)), each = sliceSize)[1:length(snippet)]
          words300A <- split(snippet, groupA)
          words300B <- c(words300B, words300A)
          words300B
        }
      })
      for(s in 1:length(tmp)){
        if(length(tmp[[s]]) > 0){
          for(t in 1:length(tmp[[s]])){
            words300B <- c(words300B,paste(unlist(tmp[[s]][t]),collapse = " "))
          }
        }
      }
      rm(tmp)
      gc()
      
    } else if (sliceSize == "sentence"){
      # sentences <- unlist(spacyr::spacy_tokenize(processedText,what="sentence"))
      # words300B <- list()
      # for(idx in 1:length(sentences)){
      #   words300B[[length(words300B)+1]] <- sentences[idx]
      # }
      
      #coreNLP::initCoreNLP(type = c("english"),mem="6g")
      #annotation <- coreNLP::annotateString(xx)
      
      sent_token_annot <- openNLP::Maxent_Sent_Token_Annotator()
      NLP_text <- NLP::as.String(processedText)
      sent_annotate <- NLP::annotate(NLP_text, sent_token_annot)
      sentences <- NLP_text[sent_annotate]
      words300B <- sentences
      
      write.csv(words300B, paste0("resources/output/", sliceSize,"/", theSource,"_sentences.csv"),  col.names = F, row.names = F, sep = ";")
    }
    
    
    #character list
    # random word lists -> allwords <- TRUE
    #trait_words <- tolower(readLines('resources/random-500.txt'))
    #trait_words <- tolower(readLines(paste0("resources/output/",theSource,"_random.csv")))
    
    #trait_words <- gsub("\\s*", "", tolower(readLines('resources/Personal Traits.txt')))
    #trait_words <- tolower(readLines('resources/pda500.txt'))
    #trait_words <- tolower(read.table('resources/combinedTraitDict.txt',header = T)$x)
    trait_words <- tolower(read.table('resources/combinedWordDict.txt',header = T)$x)
    #trait_words <- unique(tolower(readLines('resources/pda1710_no_abbreviation.csv')))
    
    pers_state <- list()
    
    outer_lapply <- lapply(words300B, function(xx){
      result = tryCatch({
        
        #if(stemTrait){
        #  tm <- tm::Corpus(tm::VectorSource(xx))
        #  tm <- tm::tm_map(tm, tm::stemDocument)
        #  xx <- toString(tm[[1]]$content)
        #}
        
        #annotation <- coreNLP::annotateString(xx)
        
        if(personCentric){
          annotation <- spacyr::spacy_parse(xx,tag=T,dependency=T)
          negs <- annotation[annotation$dep_rel == "neg",]
          neg_deps <- c()
          for(ne in 1:nrow(negs)){
            neg_deps <- c(neg_deps, annotation[which(annotation$sentence_id==negs[ne,2] & annotation$token_id==negs[ne,8]),4])
          }
          #tokens <- coreNLP::getToken(annotation)
          #Deps <- coreNLP::getDependency(annotation, type = "basic")
          #Coref <- coreNLP::getCoreference(annotation)
          #neg_deps <- Deps[Deps$type == "neg",]$governor
          #advs <- tokens[tokens$POS == "JJ" | tokens$POS == "JJS" | tokens$POS == "JJR",c("lemma","token")]
          advs <- annotation[annotation$pos == "ADJ",c("lemma","token")]
          
          peps <- annotation[(annotation$entity == "PERSON_B" | annotation$entity == "PERSON_I"),]
          peps$token_id <- as.numeric(peps$token_id)
          
          compeps <- which(diff(peps$token_id)==1)
          
          if(nrow(peps)>0){
            peplist <- unique(unlist(lapply(c(1:nrow(peps)),function(x){
              if((x-1) %in% compeps){
                
              } else if(x %in% compeps){
                full_pers <- peps$token[x]
                k<-0
                while((x+k) %in% compeps){
                  k<-k+1
                  full_pers <- paste0(full_pers,"_",peps$token[x+k])
                }
                full_pers
              }  else{
                paste0(peps$token[x])
              }
            })))
          } else{
            peplist <- list()
          }
          
          if(length(peplist) == 0 & length(which(annotation$lemma == "-PRON-" & (annotation$tag == "PRP$" | annotation$tag == "PRP"))) > 0){
            # no new people, so check whether we have he or she
            peplist <- pers_state
          } else{
            # new peplist into state
            pers_state <<- peplist
          }
          
          if(allwords){
            xx <- gsub(",|\\.|;|:|\\'\\\\\"",'', xx)
            xx <- unlist(strsplit(xx, split = ' '))
          } else{
            xx <- tolower(advs$token)
            xx_lem <- advs$lemma
          }
          if(stemTrait){
            matched <- pmatch(trait_words,xx)
            traitwords_out <- xx[matched[which(!is.na(matched))]]
          } else{
            matched <- match(xx,trait_words)
            traitwords_out <- xx[which(!is.na(matched))]
            if(!allwords) {traitwords_out_lem <- xx_lem[which(!is.na(matched))]}
          }
          
          ## Replacing lemmatised word with token word to allow for matching of negative dependencies
          #traitwords_out[which(traitwords_out == tokens$lemma[which(traitwords_out == tokens$lemma)])] <- tolower(tokens$token[which(tokens$lemma == traitwords_out)])
          traitwords_out[which(traitwords_out %in% neg_deps)] <- tolower(paste0("#-",traitwords_out[which(traitwords_out %in% neg_deps)]))
          
          if(length(traitwords_out)>0 & length(peplist)>0){ 
            traitwords_out <- unlist(lapply(traitwords_out,function(x){tolower(paste0(peplist,"::",x))}))
          } else {
            traitwords_out <- c()
          }
          
        } else{
          annotation <- spacyr::spacy_parse(xx,tag=T,dependency=T)
          negs <- annotation[annotation$dep_rel == "neg",]
          neg_deps <- c()
          for(ne in 1:nrow(negs)){
            neg_deps <- c(neg_deps, annotation[which(annotation$sentence_id==negs[ne,2] & annotation$token_id==negs[ne,8]),4])
          }
          #tokens <- coreNLP::getToken(annotation)
          #Deps <- coreNLP::getDependency(annotation, type = "basic")
          #Coref <- coreNLP::getCoreference(annotation)
          #neg_deps <- Deps[Deps$type == "neg",]$governor
          #advs <- tokens[tokens$POS == "JJ" | tokens$POS == "JJS" | tokens$POS == "JJR",c("lemma","token")]
          advs <- annotation[annotation$pos == "ADJ",c("lemma","token")]
          if(allwords){
            xx <- gsub(",|\\.|;|:|\\'\\\\\"",'', xx)
            xx <- unlist(strsplit(xx, split = ' '))
          } else{
            xx <- tolower(advs$token)
            xx_lem <- advs$lemma
          }
          if(stemTrait){
            matched <- pmatch(trait_words,xx)
            traitwords_out <- xx[matched[which(!is.na(matched))]]
          } else{
            matched <- match(xx,trait_words)
            traitwords_out <- xx[which(!is.na(matched))]
            if(!allwords) {traitwords_out_lem <- xx_lem[which(!is.na(matched))]}
          }
          
          ## Replacing lemmatised word with token word to allow for matching of negative dependencies
          #traitwords_out[which(traitwords_out == tokens$lemma[which(traitwords_out == tokens$lemma)])] <- tolower(tokens$token[which(tokens$lemma == traitwords_out)])
          traitwords_out[which(traitwords_out %in% neg_deps)] <- tolower(paste0("#-",traitwords_out[which(traitwords_out %in% neg_deps)]))
        }
        
        rm(annotation)
        #rm(tokens)
        #rm(Deps)
        rm(neg_deps)
        rm(advs)
        gc()
        return(traitwords_out)
      }, warning = function(warning_condition) {}, error = function(error_condition) {}, finally={})
    })
    spacyr::spacy_finalize()
    
    unique_trait <- lapply(outer_lapply, function(tt){
      paste(sort(unique(unlist(tt))), collapse = ", ")
    })
    
    charDS <- data.frame(x = c(1:length(unique_trait)),
                         y = unlist(unique_trait),
                         stringsAsFactors = FALSE)
    
    #garbage collection  
    rm(words300B)
    rm(sentences)
    rm(outer_lapply)
    rm(unique_trait)
    gc()
    
    tic <- tic_generate(charDS)
    
    rm(charDS)
    gc()
    
    ## Extracting nodes, links, roots from network model computed in tic_generate.
    nodes <- data.frame(node_id = unlist(tic[[1]][,1]), tags = unlist(tic[[1]][,2]),
                        dpub = unlist(tic[[1]][,3]), stringsAsFactors = F)
    links <- data.frame(source = unlist(tic[[2]][,1]), target = unlist(tic[[2]][,2]),
                        tag = unlist(tic[[2]][,3]), stringsAsFactors = F)
    roots <- data.frame(root_node_id = unlist(tic[[3]][,1]),
                        tag = unlist(tic[[3]][,2]), stringsAsFactors = F)
    
    # TIC Statistics ----------------------------------------------------------
    ## Can we replace this with a lapply call for a list, saves two lines
    write.table(nodes,file=paste0("resources/output/", sliceSize,"/",
                                  theSource,"_nodes.csv"), row.names = F)
    write.table(links,file=paste0("resources/output/", sliceSize,"/",
                                  theSource,"_links.csv"), row.names = F)
    write.table(roots,file=paste0("resources/output/", sliceSize,"/",
                                  theSource,"_roots.csv"), row.names = F)
    print(paste0(theSource,"1"))
    
    uniqueLinks <- data.frame(id1 = character(0),
                              id2=character(0),
                              label = character(0))
    convLinks <- as.data.frame(links, stringsAsFactors = F)
    for(z in 1:nrow(convLinks)){
      uniqueLinks <- rbind(uniqueLinks,
                           data.frame(id1=unlist(convLinks[z,1]),
                                      id2=unlist(convLinks[z,2]),
                                      label=paste(convLinks[which(unlist(convLinks[,1])==unlist(convLinks[z,1]) & unlist(convLinks[,2])==unlist(convLinks[z,2])),3],collapse=', '),stringsAsFactors = F))
    }
    uniqueLinks <- unique(uniqueLinks)
    colnames(uniqueLinks) <- c("id1","id2","label")
    g <- igraph::graph.data.frame(uniqueLinks,directed = TRUE)
    
    nLabels <- c()
    for(z in igraph::V(g)$name){
      nLabels <- c(nLabels,paste(unique(unlist(strsplit(paste(uniqueLinks[which(uniqueLinks$id1==z | uniqueLinks$id2==z),3],collapse = ', '),', '))),collapse = ', '))
    }
    print(paste0(theSource,"2"))
    
    deg <-  igraph::degree(g, mode="all")
    
    degd <- degree.distribution(g)
    wtc <- igraph::cluster_walktrap(g)
    gstat <- c(igraph::diameter(g), min(degd), max(degd), mean(degd), igraph::edge_density(g), igraph::modularity(wtc))
    write.table(c(theSource,gstat), paste0("resources/output/", sliceSize, "/", theSource,"_netstat_combined.csv"), append = T, col.names = F, row.names = F, sep = ";")
    write.table(gstat, paste0("resources/output/", sliceSize,"/", theSource,"_netstat.csv"),  col.names = F, row.names = F, sep = ";")
    
    nodes <- as.data.frame(nodes, stringsAsFactors=F)
    colnames(nodes) <- c('id','title','label')
    print(paste0(theSource,"3"))
    
    #### create the character network from the cascade
    socN1 <- c()
    counter_lin <- 0
    for(lin in 1:nrow(nodes)){
      nex <- unlist(strsplit(nodes$title[lin], ", "))
      if(length(nex) > 1){
        socN1 <- rbind(socN1, paste(nex, collapse = ', '))
        
        
        socEdges <- lapply(socN1, function(lin){
          templin <- unlist(strsplit(lin, ', '))
          gtools::combinations(length(templin),
                               2, templin)
        })
      }
    }
    socEdges <- do.call("rbind", socEdges)
    
    write.table(unique(socEdges), file = paste0("resources/output/",
                                                sliceSize,"/", theSource,
                                                "_socnet_edgelist.csv"), sep = ';',
                row.names = F, col.names = F)
    socEdges <- plyr::count(socEdges)
    colnames(socEdges) <- c("id1", "id2", "label")
    h <- igraph::graph.data.frame(socEdges, directed = F)
    print(paste0(theSource,"3.5"))
    igraph::E(h)$weight <- igraph::E(h)$label
    netm <- igraph::as_adjacency_matrix(h, attr = "weight", sparse = F)
    print(paste0(theSource,"3.7"))
    colnames(netm) <- igraph::V(h)$name
    rownames(netm) <- igraph::V(h)$name
    write.table(netm, paste0("resources/output/",
                             sliceSize,"/",theSource,"_network_matrix.csv"), 
                col.names = NA, row.names = T,
                fileEncoding = "UTF-8",
                sep = " ")
    print(paste0(theSource,"4"))
    
    #garbage collection  
    rm(socEdges)
    rm(links)
    rm(nodes)
    rm(roots)
    rm(convLinks)
    rm(g)
    rm(h)
    rm(netm)
    rm(SocN1)
    rm(tic)
    rm(uniqueLinks)
    rm(wtc)
    gc()
  }  
}

#Rprof()
#summaryRprof(profile_out)
