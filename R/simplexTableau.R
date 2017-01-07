# simplexTableau ---------------------------------------------------------------------
simplexTableau <- function(prob) {
  tableau <- data.frame(round(prob$A, 2), b = round(prob$b, 2)); tableau
  tableau <- rbind(tableau, c(prob$zT, prob$Z)); tableau
  tableau <- rbind(tableau, c(prob$zTcT, NA)); tableau
  row.names(tableau) <- c(prob$basic, "Z", "Z-C")
  colnames(tableau) <- c(prob$variables, "b")
  tableau
}
#-----------------------------------------------------------------------------#