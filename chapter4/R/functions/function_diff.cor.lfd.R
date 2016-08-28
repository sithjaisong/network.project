
diff.corr   <- function(data1, r.data2, p.adjust.methods = "local", threshold = 0.05){
  
  if(nrow(data1) > nrow(data2)){
    r.data1 = data1
    r.data2 = data2
  } else {
    r.data1 = data2
    r.data2 = data1
  }
  
  ccc1 <- as.vector(r.data1[lower.tri(r.data1)])
  
  ccc2 <- as.vector(r.data2[lower.tri(r.data2)])
  
  n1 <- ncol(r.data1)
  n2 <- ncol(r.data2)
  n <- nrow(r.data1)
  
  N <- n * (n - 1)/2
  
  p1 <- rep(1, N)
  
  p2 <- rep(1, N)
  
  pdiff <- rep(1, N)
  
  diff <- rep(1, N)
  
  mol.names <- rownames(r.data1)
  
  p1 <- cor2.test(n1, ccc1)
  
  p2 <- cor2.test(n2, ccc2)
  
  pdiff <- compcorr(n1, ccc1, n2, ccc2)$pval
  diff <- ccc1 - ccc2
  
  pdiff[(is.na(pdiff)) == TRUE] <- 1
  
  if (p.adjust.methods == "local") {
    p1.lfdr <- get.lfdr(p1)$lfdr
    p2.lfdr <- get.lfdr(p2)$lfdr
    pdiff.lfdr <- get.lfdr(pdiff)$lfdr
  }
  else if (p.adjust.methods == "BH" | p.adjust.methods == "bh") {
    p1.lfdr <- p.adjust(p1, method = p.adjust.methods)
    p2.lfdr <- p.adjust(p2, method = p.adjust.methods)
    pdiff.lfdr <- p.adjust(pdiff, method = p.adjust.methods)
  }
  else {
    p1.lfdr <- rep("not adjusted", N)
    p2.lfdr <- rep("not adjusted", N)
    pdiff.lfdr <- rep("not adjusted", N)
  }
  
  fin.ind <- pdiff.lfdr < threshold
  myindex <- which((lower.tri(r.data1)) == TRUE, arr.ind = TRUE)
  mol.names1 <- mol.names[myindex[, 2]]
  mol.names2 <- mol.names[myindex[, 1]]
  fin.ind <- pdiff < 0.05
  
  # combine data
  res <- cbind(mol.names1[fin.ind], mol.names2[fin.ind], ccc1[fin.ind], p1[fin.ind], ccc2[fin.ind], p2[fin.ind], pdiff[fin.ind], diff[fin.ind])
  
  # correct format the data structure
  
  res <- as.data.frame(res)
  names(res) <- c("var1", "var2", "r1", "p1", "r2", "p2", "p.difference", "difr")
  res$var1 <- as.character(res$var1)
  res$var2 <- as.character(res$var2)
  res$r1 <- as.numeric(as.character(res$r1))
  res$p1 <- as.numeric(as.character(res$p1))
  res$r2 <- as.numeric(as.character(res$r2))
  res$p2  <- as.numeric(as.character(res$p2))
  res$p.difference <- as.numeric(as.character(res$p.difference))
  res$difr <- as.numeric(as.character(res$difr))
  return(res)
}