#-----------------------------------------------------------------------------#
# veRySimplex
# Manuel Fontenla Cadavid
#-----------------------------------------------------------------------------#

# NormSimplex -----------------------------------------------------------------
#' Normalize simplex
#' 
#' @examples
#' # Data
#' type <- "maximize"
#' variables <- c("X", "Y")
#' coefficients <- c(1, 3)
#' constraints <- matrix(c(1, 1, 2, 1, -1, -1), ncol = length(variables), byrow = TRUE)
#' rhs <- c(40, 10, 10)
#' sense <- c("<=", ">=", ">=")
#' # Normalize
#' simplexNorm(variables, coefficients, constraints, rhs, sense, type)

simplexNorm <- function(variables, coefficients, constraints, rhs, sense, type) {
  
  # Optimization objective: maximize or minimize
  if (type == "minimize") {
    # Convert the minimize objective function F to maximize objective F * (-1)
    coefficients <- -coefficients
  }
  
  # Variables
  if (length(variables) == 1 & is.numeric(variables)) {
    # If "variables" is a number, then automatically creates variable names
    variables <- paste0("X", 1:variables)
  }
  if (!(length(variables) == length(coefficients))) {
    stop("variables and coefficients need to have the same length")
  }
  
  
  # All the independent terms need to be not negatives
  if (any(rhs < 0)) {
    constraints[rhs < 0, ] <- constraints[rhs < 0, ] * -1
    rhs[rhs < 0] <- rhs[rhs < 0] * -1
  }
  
  # All constraints must be equal
  # Inequality to equality by adding slack, surplus and/or artificial variables
  slacks <- c(); surplus <- c(); artificials <- c()
  
  for (i in 1:nrow(constraints)) {
  
    if (sense[i] == ">=") {
      
      # - surplus
      variables <- c(variables, paste0("E", length(surplus) + 1))
      surplus <- c(surplus, variables[length(variables)])
      coefficients <- c(coefficients, 0)
      constraints <- cbind(constraints, 0)
      constraints[i, ncol(constraints)] <- -1
      sense[i] <- "="
      # + artificial
      variables <- c(variables, paste0("A", length(artificials) + 1))
      artificials <- c(artificials, variables[length(variables)])
      coefficients <- c(coefficients, 0)
      constraints <- cbind(constraints, 0)
      constraints[i, ncol(constraints)] <- 1
      sense[i] <- "="
    
    } else if (sense[i] == "=") {
      
      # + artificial
      variables <- c(variables, paste0("A", length(artificials) + 1))
      artificials <- c(artificials, variables[length(variables)])
      coefficients <- c(coefficients, 0)
      constraints <- cbind(constraints, 0)
      constraints[i, ncol(constraints)] <- 1
      sense[i] <- "="
      
    } else if (sense[i] == "<=") {
      
      # + slack
      variables <- c(variables, paste0("S", length(slacks) + 1))
      slacks <- c(slacks, variables[length(variables)])
      coefficients <- c(coefficients, 0)
      constraints <- cbind(constraints, 0)
      constraints[i, ncol(constraints)] <- 1
      sense[i] <- "="
    
    } else {
    
      stop("unknown constraint sense")
    
    }
    
  }
  
  return(list(type = type,
              coefficients = coefficients,
              variables = variables,
              constraints = constraints,
              sense = sense,
              rhs = rhs,
              slacks = slacks,
              surplus = surplus,
              artificials = artificials))
  
}
#-----------------------------------------------------------------------------#