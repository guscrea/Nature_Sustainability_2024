# function for creating similarity matrix between two vectors of words
# (e.g., plaintiff name and appellant name)

# inputs are v1, v2
# both are vectors of words to be compared (e.g. plaintiff name and appellant
# name)

sim_mat_f <- function(v1,v2){
  mat_of_scores <- vector("list",length(v1))
  m=1
  while(m <= length(v1)){
    list_of_scores <- vector("list", length(v2))
    n = 1
    while(n <= length(v2)){
      list_of_scores[n] <- stringsim(
        v1[m],
        v2[n]
      )
      n = n + 1
    }
    mat_of_scores[[m]] <- list_of_scores
    m = m + 1
  }
  mat_of_scores <- do.call("cbind",mat_of_scores)
  return(mat_of_scores)
}