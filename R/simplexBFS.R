# simplexBFS ------------------------------------------------------------------
simplexBFS <- function(prob) {
  
  # Problem size
  n <- length(prob$variables); n
  m <- nrow(prob$constraints); m
  
  # Decission variables
  x <- prob$variables; x
  # Basic and nonbasic variables
  if (length(prob$artificials) > 0) {  # two phases method
    basic <- c(prob$artificials, prob$slacks); basic
    nonbasic <- prob$variables[-which(prob$variables %in% basic)]; nonbasic
  } else {  # simplex method
    basic <- c(prob$slacks, prob$surplus); basic
    nonbasic <- prob$variables[-which(prob$variables %in% basic)]; nonbasic
  }
  
  # Coefficients of objective function
  cT <- prob$coefficients
  if (length(prob$artificials) > 0) {  # two phases method
    cT[!grepl("A", prob$variables)] <- 0 # all coefficients equal to zero
    cT[grepl("A", prob$variables)] <- -1  # except artificial variables (= -1)
  }
  cT
  
  # Matrix with constraints coefficients
  A <- prob$constraints; A
  
  # Independent terms: entries in the right hand side (RHS)
  b <- prob$rhs; b
  
  #  Z = Sum(cb[i] * Pj) - cT
  cB <- cT[match(basic, x)]; cB
  zT <- colSums(cB * A); zT
  Z <- sum(cB * b); Z
  zTcT <- zT - cT
  
  return(list(coefficients = prob$coefficients, variables = x,
              basic = basic, nonbasic = nonbasic,
              cT = cT, cB = cB, A = A, b = b, zT = zT, zTcT = zTcT, Z = Z))
  
}
#-----------------------------------------------------------------------------#