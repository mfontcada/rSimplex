# simplexUpdate ---------------------------------------------------------------
simplexUpdate <- function(prob, pivot.row, pivot.col) {
  
  # Determine which nonbasic variable is to become basic.
  nonbasic2basic <- prob$variables[pivot.col]; nonbasic2basic
  # Determine which basic variable is to become nonbasic.
  basic2nonbasic <- prob$basic[pivot.row]; basic2nonbasic
  # Update basic and nonbasic variables
  prob$basic[prob$basic == basic2nonbasic] <- nonbasic2basic
  prob$nonbasic[prob$nonbasic == nonbasic2basic] <- basic2nonbasic
  prob$basic; prob$nonbasic
  
  # Get pivot value
  pivot <- prob$A[pivot.row, pivot.col]; pivot
  
  # Pivot on the pq-th element, updating all rows, including the z-row.
  # En la fila del elemento pivote cada nuevo elemento se calcula como:
  # Nuevo Elemento Fila Pivote = Anterior Elemento Fila Pivote / Pivote
  prob$A[pivot.row, ] <- prob$A[pivot.row, ] / pivot; prob$A
  prob$b[pivot.row] <- prob$b[pivot.row] / pivot; prob$b
  
  # En el resto de las filas cada elemento se calcula:
  nonpivot.rows <- (1:nrow(prob$A))[-pivot.row]; nonpivot.rows
  # Nuevo Elemento Fila = Anterior Elemento Fila - (Anterior Elemento Fila en Columna Pivote * Nuevo Elemento Fila Pivote)
  for (i in nonpivot.rows) {
    A.previous.row <- prob$A[i, ]; A.previous.row
    A.pivot.col <- rep(prob$A[i, pivot.col], ncol(prob$A)); A.pivot.col
    A.pivot.row <- prob$A[pivot.row, ]; A.pivot.row
    prob$A[i, ] <- A.previous.row - (A.pivot.col * A.pivot.row)
    
    b.previous.row <- prob$b[i]; b.previous.row
    b.pivot.col <- A.pivot.col[1]; b.pivot.col
    b.pivot.row <- prob$b[pivot.row]; b.pivot.row
    prob$b[i] <- b.previous.row - (b.pivot.col * b.pivot.row)
  }
  prob$A
  prob$b
  
  #  Z = Sum(cb[i] * Pj) - cT
  prob$cB <- prob$cT[match(prob$basic, prob$variables)]; prob$cB
  prob$cB[is.na(prob$cB)] <- -1; prob$cB
  prob$zT <- colSums(prob$cB * prob$A); prob$zT
  prob$zTcT <- prob$zT - prob$cT
  prob$Z <- sum(prob$cB * prob$b); prob$Z
  
  return(prob)
  
}
#-----------------------------------------------------------------------------#