library("testthat")

test_that("Naive Bayes - Golf Data", {
  result <- naivebayes(
    formula = karar ~ gorunum + sicaklik + nem + ruzgar, data = golfdata,
    input = data.frame(gorunum = "Gunesli", sicaklik = "Ilik", nem = "Normal", ruzgar = "Zayif")
  )
  expect_equal(result$Hayir, 0.006857143)
  expect_equal(result$Evet, 0.02821869)
})


test_that("Gaussian Naive Bayes", {
  kilo <- c(50, 45, 40, 46, 50, 70, 75, 76, 77, 90)
  boy <- c(160, 165, 166, 167, 168, 175, 176, 177, 178, 190)
  cinsiyet <- as.factor(c(rep("Kadin", 5), rep("Erkek", 5)))
  mydata <- data.frame(boy, kilo, cinsiyet)
  result <- gaussianNaiveBayes(
    formula = cinsiyet ~ boy + kilo,
    data = mydata,
    input = data.frame(boy = 185, kilo = 80)
  )
  expect_equal(result$Kadin < result$Erkek, TRUE)
})