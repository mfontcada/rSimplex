# rSimplex

A very basic implementation of the simplex algorithm to solve linear programming problems in R.

```r
devtools::install_github("mfontcada/rSimplex")
library(rSimplex)
```

### Example

$$
\begin{equation}
\begin{matrix}
\max & Z = 5x_1 + 4x_2 \\[15pt]
\textrm{s.t.} & 6x_1 + 4x_2 \leq 24 \\[10pt]
& x_1 + 2x_2 \leq 6 \\[10pt]
& -x_1 + x_2 \leq 1 \\[10pt]
& x_2 \leq 2 \\[10pt]
& x_1, x_2 \geq 0 \\[10pt]
\end{matrix}
\end{equation}
$$

```r
variables <- c("X1", "X2")
coefficients <- c(5, 4)
constraints <- matrix(c(6, 4,
                        1, 2,
                        -1, 1,
                        0, 1),
                      ncol = length(variables), byrow = TRUE)
rhs <- c(24, 6, 1, 2)
sense <- c("<=", "<=", "<=", "<=")
type <- "maximize"

sol <- simplex(variables, coefficients, constraints, rhs, sense, type)
```
