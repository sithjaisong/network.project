library(ltm)

cooc_table <- function(temp){
  
  results <- matrix(nrow = 0, ncol = 6)
  
  for (b in 1:(length(names(temp)) - 1)) {
    
    # every species will be compared to every other species, so there has
    # to be another loop that iterates down the rest of the columns
    
    for (c in (b + 1):length(names(temp))){
      
      # summing the abundances of species of the columns that will be
      # compared
      
      var1.sum <- sum(temp[, b], na.rm = TRUE)
      
      var2.sum <- sum(temp[, c], na.rm = TRUE)
      
      # if the column is all 0's no co-occurrence will be performed
      
      if (var1.sum > 1 & var2.sum > 1){
        
        test <- cor.test(temp[, b], temp[, c], method = "spearman",  na.action = na.rm, exact = FALSE)
        
        # There are warnings when setting exact = TRUE because of ties from the
        # output of Spearman's correlation
        # stackoverflow.com/questions/10711395/spear-man-correlation and ties
        # It would be still valid if the data is not normally distributed.
        
        rho <- test$estimate
        
        p.value <- test$p.value
      }
      
      if (var1.sum <= 1 | var2.sum <= 1) {
        
        rho <- 0
        
        p.value <- 1
      }
      
      new.row <- c(names(temp)[b], names(temp)[c], rho, p.value, var1.sum, var2.sum)
      
      results <- rbind(results, new.row)
      
    }
    
  }
  # change the class
  results <- as.data.frame(results) 
  
  #computer the p.adjusted using rcorr.test by FDR-BH method 
  p.adjust <- as.data.frame(rcor.test(temp, method = "spearman", p.adjust = TRUE, p.adjust.method = "BH")[2])[3] 
  
  new.results <- cbind(results, p.adjust)
  
  row.names(new.results) <- NULL
  
  names(new.results) <- c("var1", "var2", "rho", "p.value", "var1.sum", "var2.sum", "p.adjusted")
  
  new.results$rho <- as.numeric(as.character(new.results$rho))
  
  new.results$p.value <- as.numeric(as.character(new.results$p.value))
  
  new.results$p.adjust <- as.numeric(new.results$p.adjust)
  
  # reorder the column names following this
  new.results <- new.results[c("var1", "var2", "rho", "p.value", "p.adjusted", "var1.sum", "var2.sum")] 
  
  return(new.results)
}