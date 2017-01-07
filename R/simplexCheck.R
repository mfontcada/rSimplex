# simplexCheck ----------------------------------------------------------------
simplexCheck <- function(prob) {
  
  # If no aiq > 0, stop: the problem is unbounded.
  if (all(!(prob$A > 0))) {
    stop("the problem is unbounded")
  }
  
  # If each cj ≥ 0, stop; the current basic feasible solution is optimal.
  if (all(prob$zTcT >= 0)) {
    return("Optimal")
  } else {
    return("Not optimal")
  }
  
}
#-----------------------------------------------------------------------------#