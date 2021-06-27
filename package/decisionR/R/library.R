geometricmean <- function(x, na.rm = TRUE) {
    exp(mean(log(x[x > 0]), na.rm = na.rm))
}