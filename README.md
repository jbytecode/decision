# decisionR: An R package for decision science

Initially, this package includes some multiple-criterion decision making tools such as Topsis and Vikor.
The future plan is to add several methods and algorithms in the research areas of Operations Research and Decision Science.

Package content (The methods signed with &#10004; are implemented):

### Multiple-criteria decision making (MCDA) tools 
* Implement Dematel &#10004; 
* Implement Topsis &#10004;
* Implement Vikor &#10004;
* Implement Moora &#10004;
* Implement Electre &#10004;
* Implement AHP &#10004;
* Implement ANP
* Implement Promethee

### Project management
* CPM (Critical Path Method) &#10004;
* PERT &#10004;

### Decision making under uncertainity and risk
* Laplace &#10004;
* Maximin &#10004;
* Minimax &#10004;
* Maximax &#10004;
* Minimin &#10004;
* Savage &#10004;
* Hurwicz &#10004;
* Maximum Likelihood &#10004;
* Expected Regret &#10004;
* Expected value of perfect information &#10004;

### Game Theory
* Solution of m x n game matrix using linear programming &#10004;


### Installation:
-------------

devtools::install_github(repo = "https://github.com/jbytecode/decision", subdir = "package/decisionR")

### Updating the package:

devtools::update_packages(packages = "decisionR")


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


## Example for PERT: ##
____________________
```
dt <- data.frame(
    Activity = c("A", "B", "C", "D", "E", "F", "G"),
    Dependency = c("-", "-", "A, B", "B,C", "C", "D", "A,B, D "),
    O = c(9, 1, 5, 2, 2, 12, 5),
    M = c(10, 5, 7, 3, 5, 18, 5),
    P = c(11, 10, 9, 4, 6, 28, 9),
    stringsAsFactors =  FALSE
  )

print(dt)


> dt
  Activity Dependency  O  M  P
1        A          -  9 10 11
2        B          -  1  5 10
3        C       A, B  5  7  9
4        D        B,C  2  3  4
5        E          C  2  5  6
6        F          D 12 18 28
7        G    A,B, D   5  5  9


> PERT(dt)
$data
  Activity Dependency  O  M  P Start       End  Duration  Variance
1        A          -  9 10 11     0 10.000000 10.000000 0.1111111
2        B          -  1  5 10     0  5.166667  5.166667 2.2500000
3        C       A, B  5  7  9    10 17.000000  7.000000 0.4444444
4        D        B,C  2  3  4    17 20.000000  3.000000 0.1111111
5        E          C  2  5  6    17 21.666667  4.666667 0.4444444
6        F          D 12 18 28    20 38.666667 18.666667 7.1111111
7        G    A,B, D   5  5  9    20 25.666667  5.666667 0.4444444

$critical.path
[1] "F" "D" "C" "A"

$critical.path.length
[1] 38.66667

$means
[1] 10.00000  7.00000  3.00000 18.66667

$variances
[1] 0.1111111 0.4444444 0.1111111 7.1111111

$confidence95
[1] 23.42250 53.91083

$confidence99
[1] 18.63244 58.70089
```


### Example of game solution ###
```
> a <- matrix(c(0, -1, 1, 1, 0, -1, -1, 1, 0), nrow = 3, byrow = TRUE)
> result <- game(a + 2)
> a
     [,1] [,2] [,3]
[1,]    0   -1    1
[2,]    1    0   -1
[3,]   -1    1    0
> result
$g1
[1] 2

$p.row
[1] 0.3333333 0.3333333 0.3333333

$g2
[1] 2

$p.col
[1] 0.3333333 0.3333333 0.3333333
```
