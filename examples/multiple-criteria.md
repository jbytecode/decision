Example for multiple-criteria decision making using Topsis:
---------------
```
> decision.matrix <- matrix(c(6,5,7,5,6,7,8,4,3,4,5,6), nrow = 3, byrow = TRUE)

> weights <- c(0.5, 0.20, 0.15, 0.15)

> result <- topsis(decision.matrix, weights)

> print(result$best)

[1] "Alternative 2"

> result
$decision.matrix
              Criteria 1 Criteria 2 Criteria 3 Criteria 4
Alternative 1          6          5          7          5
Alternative 2          6          7          8          4
Alternative 3          3          4          5          6

$normalized.decision.matrix
              Criteria 1 Criteria 2 Criteria 3 Criteria 4
Alternative 1  0.6666667  0.5270463  0.5958796  0.5698029
Alternative 2  0.6666667  0.7378648  0.6810052  0.4558423
Alternative 3  0.3333333  0.4216370  0.4256283  0.6837635

$weighted.normalized.decision.matrix
              Criteria 1 Criteria 2 Criteria 3 Criteria 4
Alternative 1  0.3333333  0.1054093 0.08938194 0.08547043
Alternative 2  0.3333333  0.1475730 0.10215078 0.06837635
Alternative 3  0.1666667  0.0843274 0.06384424 0.10256452

$ideal.vector
Criteria 1 Criteria 2 Criteria 3 Criteria 4 
 0.3333333  0.1475730  0.1021508  0.1025645 

$worst.vector
Criteria 1 Criteria 2 Criteria 3 Criteria 4 
0.16666667 0.08432740 0.06384424 0.06837635 

$weights
[1] 0.50 0.20 0.15 0.15

$scores
[1] 0.7832713 0.8421021 0.1578979

$best
[1] "Alternative 2"

$best.index
[1] 2

```
