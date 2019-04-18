An R package for decision science.

Initially, this package includes some multiple-criterion decision making tools such as Topsis and Vikor.

Future work:
* Implement Dematel &#10004; 
* Implement Topsis &#10004;
* Implement Vikor &#10004;
* Implement Moora &#10004;
* Implement Electre &#10004;
* Implement AHP &#10004;
* Implement ANP
* Implement Promethee

The package also includes functions for other Operations Research and Decision Science subjects:
* CPM (Critical Path Method) &#10004;
* PERT &#10004;



Installation:
-------------

library(devtools)

devtools::install_github(repo = "https://github.com/jbytecode/decision", subdir = "package/decisionR")



Examples:
---------------
decision.matrix <- matrix(c(6,5,7,5,6,7,8,4,3,4,5,6), nrow = 3, byrow = TRUE)

weights <- c(0.5, 0.20, 0.15, 0.15)

result <- topsis(decision.matrix, weights)

print(result$best)

[1] "Alternative 2"
