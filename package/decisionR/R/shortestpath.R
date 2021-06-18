Connection <- setRefClass(
    Class = "Connection",
    fields = c("from", "to", "length"),
    methods = list(
        initialize = function(from, to, length) {
            .self$from <- from
            .self$to <- to
            .self$length <- length
        }
    )
)

get_nodes <- function(conns) {
    node_list <- c()
    for (conn in conns) {
        node_list <- append(node_list, c(conn$from, conn$to))
    }
    return(unique(node_list))
}

shortestpath <- function(conns) {
    nodes <- get_nodes(conns)
}