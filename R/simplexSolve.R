# simplex ---------------------------------------------------------------------
simplex <- function(variables, coefficients, constraints, rhs, sense,
                    type = "min") {

  # Store all info from each iteration
  simplexData <- list()

  # Convert to standard form
  prob <- simplexNorm(variables, coefficients, constraints, rhs, sense, type)
  simplexData[["norm"]] <- prob

  # Select method
  if (length(prob$artificials) > 0) {
    # Two phases

    # Phase 1
    phase1 <- simplexPhase1(prob)
    # Store all data from phase 1 except result
    simplexData[["phase1"]] <- phase1[-(length(phase1))]
    # Recover result of phase 1
    prob <- phase1[[length(phase1)]]

    # Phase 2
    # Init
    prob
    # Compute iterations
    iterations <- simplexIter(prob)
    # Store infor from all iterations
    simplexData[["phase2"]] <- iterations
    # Recover info from last iteration
    prob <- iterations[[length(iterations)]]

  } else {

    # Init
    prob <- simplexBFS(prob)
    # Compute iterations
    iterations <- simplexIter(prob)
    # Store infor from all iterations
    simplexData <- c(simplexData, iterations)
    # Recover info from last iteration
    prob <- iterations[[length(iterations)]]

  }

  #print(simplexTableau(prob))

  if (type == "min" || type == "minimize") {
    cat(paste0("Min Z = ", -prob$Z))
  } else {
    cat(paste0("Max Z = ", prob$Z))
  }

  invisible(simplexData)

}
#-----------------------------------------------------------------------------#
