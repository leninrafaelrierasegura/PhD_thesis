

library(MetricGraph)
library(plotly)

edge1 <- rbind(c(0,0),c(1,0))
theta <- seq(from=-pi,to=pi,length.out = 100)
edge2 <- cbind(1+1/pi+cos(theta)/pi,sin(theta)/pi)
edges <- list(edge1, edge2)
graph <- metric_graph$new(edges = edges, verbose = 0)
graph$set_manual_edge_lengths(edge_lengths = c(1,2))

graph$build_mesh(h=0.1)

f = graph$mesh$VtE[,2]

graph$plot(type = "plotly")

graph$plot_function(f, type = "plotly")
