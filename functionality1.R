## ---------------------------------------------------------------------------
# # Create a clipboard button on the rendered HTML page
# source(here::here("clipboard.R")); clipboard
# # Set seed for reproducibility
# set.seed(1982)
# # Set global options for all code chunks
# knitr::opts_chunk$set(
#   # Disable messages printed by R code chunks
#   message = FALSE,
#   # Disable warnings printed by R code chunks
#   warning = FALSE,
#   # Show R code within code chunks in output
#   echo = TRUE,
#   # Include both R code and its results in output
#   include = TRUE,
#   # Evaluate R code chunks
#   eval = TRUE,
#   # Enable caching of R code chunks for faster rendering
#   cache = FALSE,
#   # Align figures in the center of the output
#   fig.align = "center",
#   # Enable retina display for high-resolution figures
#   retina = 2,
#   # Show errors in the output instead of stopping rendering
#   error = TRUE,
#   # Do not collapse code and output into a single block
#   collapse = FALSE
# )
# # Start the figure counter
# fig_count <- 0
# # Define the captioner function
# captioner <- function(caption) {
#   fig_count <<- fig_count + 1
#   paste0("Figure ", fig_count, ": ", caption)
# }
# 


## ---------------------------------------------------------------------------
# library(MetricGraph)
# library(ggplot2)
# library(reshape2)
# library(dplyr)
# library(viridis)
# library(plotly)
# library(patchwork)
# library(slackr)
# source("keys.R")
# slackr_setup(token = token) # token comes from keys.R


## ---------------------------------------------------------------------------
# gets.graph.interval <- function(n){
#   edge <- rbind(c(0,0),c(1,0))
#   edges = list(edge)
#   graph <- metric_graph$new(edges = edges)
#   graph$build_mesh(n = n)
#   return(graph)
# }


## ---------------------------------------------------------------------------
# gets.graph.circle <- function(n){
#   r = 1/(pi)
#   theta <- seq(from=-pi,to=pi,length.out = 100)
#   edge <- cbind(1+r+r*cos(theta),r*sin(theta))
#   edges = list(edge)
#   graph <- metric_graph$new(edges = edges)
#   graph$build_mesh(n = n)
#   return(graph)
# }


## ---------------------------------------------------------------------------
# # Function to build a tadpole graph and create a mesh
# gets.graph.tadpole <- function(h){
#   edge1 <- rbind(c(0,0),c(1,0))
#   theta <- seq(from=-pi,to=pi,length.out = 100)
#   edge2 <- cbind(1+1/pi+cos(theta)/pi,sin(theta)/pi)
#   edges <- list(edge1, edge2)
#   graph <- metric_graph$new(edges = edges, verbose = 0)
#   graph$set_manual_edge_lengths(edge_lengths = c(1,2))
#   graph$build_mesh(h = h)
#   return(graph)
# }


## ---------------------------------------------------------------------------
# # Function to order the vertices for plotting
# plotting.order <- function(v, graph){
#   edge_number <- graph$mesh$VtE[, 1]
#   pos <- sum(edge_number == 1)+1
#   return(c(v[1], v[3:pos], v[2], v[(pos+1):length(v)], v[2]))
# }


## ---------------------------------------------------------------------------
# tadpole.layout <- function(x_range, y_range, z_range){
#   return(list(xaxis = list(title = "x", range = x_range),
#               yaxis = list(title = "y", range = y_range),
#               zaxis = list(title = "z", range = z_range),
#               aspectratio = list(x = 2*(1+2/pi),
#                                  y = 2*(2/pi),
#                                  z = 1*(2/pi)),
#               camera = list(eye = list(x = 5,
#                                        y = 3,
#                                        z = 4),
#                             center = list(x = (1+2/pi)/2,
#                                           y = 0,
#                                           z = 0))))
# }

