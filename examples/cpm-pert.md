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
