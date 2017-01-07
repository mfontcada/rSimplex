# simplexPhase1 ---------------------------------------------------------------
simplexPhase1 <- function(prob) {
  
  # Store all info from each iteration
  phase1Data <- list()
  
  # Start counting iterations
  iterations <- c()
  iter <- 0
  
  # Init
  prob <- simplexBFS(prob)
  phase1Data[["init"]] <- prob
  
  while(simplexCheck(prob) != "Optimal") {
    
    # New iteration
    iter <- iter + 1
    iterations <- c(iterations, paste0("iter", iter))
    
    # Select pivot
    pivot <- simplexPivot(prob)
    
    # Update tableau
    prob <- simplexUpdate(prob, pivot$row, pivot$col)
    
    # Store data
    phase1Data[[iterations[iter]]] <- prob
    
  }
  
  if (prob$Z != 0) {
    
    stop("no solution")
    
  } else {
    
    # Check coefficients from basic artificial variables
    prob$cB <- prob$coefficients[match(prob$basic, prob$variables[-which(grepl("A", prob$variables))])]; prob$cB
    prob$cB[is.na(prob$cB)] <- -1
    # Remove columns of artificial variables
    prob$coefficients <- prob$coefficients[-which(grepl("A", prob$variables))]
    # Return to original coefficients of objective function
    prob$cT <- prob$coefficients
    prob$A <- prob$A[, -which(grepl("A", prob$variables))]
    prob$variables <- prob$variables[-which(grepl("A", prob$variables))]
    prob$nonbasic <- prob$nonbasic[-which(grepl("A", prob$nonbasic))]
    # Recompute Z row
    prob$zT <- colSums(prob$cB * prob$A)
    prob$zTcT <- prob$zT - prob$cT
    prob$Z <- sum(prob$cB * prob$b)
    
    phase1Data[["result"]] <- prob
    
  }
  
  return(phase1Data)
  
}
#-----------------------------------------------------------------------------#