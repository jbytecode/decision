library("testthat")

test_that("get nodes", {
    conns <- c(
        Connection$new("A", "B", 10),
        Connection$new("A", "C", 20),
        Connection$new("B", "D", 1),
        Connection$new("C", "D", 10)
    )
    nodes <- sort(get_nodes(conns))
    expect_equal(length(nodes), 4)
    expect_equal(nodes, c("A", "B", "C", "D"))
})