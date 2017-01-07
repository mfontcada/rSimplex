simplexIter <- function(prob) {
  
  # Store all info from each iteration
  simplexData <- list()
  
  # Start counting iterations
  iterations <- c()
  iter <- 0
  
  # Store initial info
  simplexData[["init"]] <- prob 

  while(simplexCheck(prob) != "Optimal") {
    
    # New iteration
    iter <- iter + 1
    iterations <- c(iterations, paste0("iter", iter))
    
    # Select pivot
    pivot <- simplexPivot(prob)
    
    # Update tableau
    prob <- simplexUpdate(prob, pivot$row, pivot$col)
    
    # Store data
    simplexData[[iterations[iter]]] <- prob
    
  }
  
  return(simplexData)
  
}