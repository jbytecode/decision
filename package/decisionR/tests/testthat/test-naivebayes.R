library("testthat")

testStatus <- function(strMessage){
  cat("* Doing test: ", strMessage, "\n")  
}

test_that("Naive Bayes - Golf Data",{
  result <- naivebayes(formula = karar ~ gorunum + sicaklik + nem + ruzgar, data = golfdata, 
                  input = data.frame(gorunum="Gunesli", sicaklik = "Ilik", nem = "Normal", ruzgar = "Zayif"))
  expect_equal(result$Hayir, 0.006857143)
  expect_equal(result$Evet, 0.02821869)
})


