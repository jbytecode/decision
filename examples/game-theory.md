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
