#############################################################################
# titile : function.audpc.R; 
# purpose : generate AUDPC; 
# producer : A. H. Sparks et. al 2007; 
# last update : In Los Baños, 1 Jul. 2014;
# inputs : disease severity measurements over time;
# outputs : data frame named sheet1, sheet2, sheet3 and sheet4 
# remarks 1 : based on AUDPC script found in: : Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D. Silwal, S. Tolos, and K.A. Garrett, 2008.  : Ecology and Epidemiology in R: Disease Progress over Time. The Plant Health Instructor. DOI:10.1094/PHI-A-2008-0129-02.  : URL: https://www.apsnet.org/EDCENTER/ADVANCED/TOPICS/ECOLOGYANDEPIDEMIOLOGYINR/DISEASEPROGRESS/Pages/AUDPC.aspx
# Licence: : GPL2;
#############################################################################

audpc <- function(disease.severity, time.period) {
  # n is the length of time.period, or the total number of sample dates
  n <- length(time.period)
  
  # meanvec is the vector (matrix with one dimension) that will contain the mean percent infection it is initialized
  # containing -1 for all entries this sort of initialization is sometimes useful for debugging
  meanvec <- matrix(-1, (n - 1))
  
  # intvec is the vector that will contain the length of time between sampling dates
  intvec <- matrix(-1, (n - 1))
  
  # the loop goes from the first to the penultimate entry the left curly bracket indicates the beginning of commands
  # in the loop
  for (i in 1:(n - 1)) {
    # the ith entry in meanvec is replaced with the mean percent infection between sample time i and sample time i+1
    meanvec[i] <- mean(c(disease.severity[i], disease.severity[i + 1]))
    
    # the ith entry in intvec is replaced with the length of the time interval between time i and time i+1
    intvec[i] <- time.period[i + 1] - time.period[i]
  }  # the right curly bracket ends the loop
  # the two vectors are multiplied together one entry at a time
  infprod <- meanvec * intvec
  
  # the sum of the entries in the resulting vector gives the AUDPC
  sum(infprod)
}

# eos 