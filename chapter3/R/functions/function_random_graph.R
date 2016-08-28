
#to prove the nature of the network

random_graph <- function(netgraph, trials = 10000){
  
  ntrials <- trials
  
  nv <- vcount(netgraph)
  ne <- ecount(netgraph)
  
  cl.rg <- numeric(ntrials) # cluster coefficient of random graph
  apl.rg <- numeric(ntrials) # average path of randomgraph
  
  for(i in 1:ntrials){
    g.rg <- erdos.renyi.game(nv, ne, type = "gnm", directed = FALSE)
    cl.rg[i] <- igraph::transitivity(g.rg, type = "global")
    apl.rg[i] <- average.path.length(g.rg)
  }
  
  random <- data_frame(cl.rg, apl.rg)
  
  names(random) <- c("cluster_coef", "average_path")

  return(random)
  }


#smallworldness(netgraph)

#' A network can be said "smallworld" if its smallworldness is higher than one 
#' (a stricter rule is smallworldness>=3; Humphries & Gurney, 2008). 
#' To consider a network as "smallworld", it is also suggested to inspect 
#' that the network has a transitivity substantially higher than comparable random networks and that 
#' its average shortest path length is similar or higher (but not many times higher) than that computed on random networks. 
#' Edge weights, signs and directions are ignored in the computation of the indices.

#scaleFreeFitIndex(abs(E(netgraph)$weight))

#summary(hist(degree(netgraph)))

#degree_distribution(netgraph)