# simplexPivot ----------------------------------------------------------------
simplexPivot <- function(prob) {
  
  # Assign objects to this environment
  #mapply(assign, names(prob), prob, MoreArgs=list(envir = environment())))
  
  # Select column with min(zT - cT)
  pivot.col <- which.min(prob$zTcT); pivot.col
  
  # Calculate the ratios
  ratio <- rep(NA, nrow(prob$A)); ratio
  positives <- prob$A[, pivot.col] > 0; positives
  ratio[positives] <- prob$b[positives] / prob$A[, pivot.col][positives]
  # Se escoge la fila cuyo resultado haya resultado mínimo.
  pivot.row <- which.min(ratio[ratio >= 0]); pivot.row
  
  return(list(ratio = ratio, row = pivot.row, col = pivot.col))
  
}

#iStep <- selectSimplex(prob); istep
#-----------------------------------------------------------------------------#