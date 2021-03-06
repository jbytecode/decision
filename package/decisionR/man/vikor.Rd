% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiple-criteria.R
\name{vikor}
\alias{vikor}
\title{Vikor Method}
\usage{
vikor(decisionMatrix, weights, v = 0.5)
}
\arguments{
\item{weights}{Weights for criteria (Measure of how important the criteria are.)}

\item{v}{Another weight parameter which is defined in range 0 < v < 1 and set to 0.5 by default.}

\item{A}{Decision matrix}
}
\value{
A list of results.
}
\description{
This function implements the Vikor method for the multiple-criteria decision problem.
}
\examples{
A <- matrix(c(9,7,6,7,8,7,9,6,7,8,6,6), nrow = 3, byrow = TRUE)
W <- c(4, 2, 6, 8)
w <- W / sum(W)
result <- vikor(A, w)
print(result)
}
\references{
Celikbilek Yakup, Cok Kriterli Karar Verme Yontemleri, Aciklamali ve Karsilastirmali
Saglik Bilimleri Uygulamalari ile. Editor: Muhlis Ozdemir, Nobel Kitabevi, Ankara, 2018
}
\author{
Mehmet Hakan Satman - mhsatman@istanbul.edu.tr
}
