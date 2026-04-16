## -------------------------------------------------------------------------------------------------------------------
# Create a clipboard button on the rendered HTML page
source(here::here("clipboard.R")); clipboard
# Set seed for reproducibility
#set.seed(1982) 
# Set global options for all code chunks
knitr::opts_chunk$set(
  # Disable messages printed by R code chunks
  message = FALSE,    
  # Disable warnings printed by R code chunks
  warning = FALSE,    
  # Show R code within code chunks in output
  echo = TRUE,        
  # Include both R code and its results in output
  include = TRUE,     
  # Evaluate R code chunks
  eval = TRUE,       
  # Enable caching of R code chunks for faster rendering
  cache = FALSE,      
  # Align figures in the center of the output
  fig.align = "center",
  # Enable retina display for high-resolution figures
  retina = 2,
  # Show errors in the output instead of stopping rendering
  error = TRUE,
  # Do not collapse code and output into a single block
  collapse = FALSE
)
# Start the figure counter
fig_count <- 0
# Define the captioner function
captioner <- function(caption) {
  fig_count <<- fig_count + 1
  paste0("Figure ", fig_count, ": ", caption)
}



## -------------------------------------------------------------------------------------------------------------------
library(MetricGraph)
library(ggplot2)
library(reshape2)
library(dplyr)
library(viridis)
library(plotly)
library(patchwork)

library(slackr)
source("keys.R")
slackr_setup(token = token) # token comes from keys.R


## -------------------------------------------------------------------------------------------------------------------
# Color for axis name and axis numbers
colaxnn <- "gray"
# Global font size
gfsize <- 16
# Dark blue color
mydarkblue <- "#0000C8"
# Global size or widht for objects
gsw <- 7


## -------------------------------------------------------------------------------------------------------------------
gets.graph.interval <- function(n){
  edge <- rbind(c(0,0),c(1,0))
  edges = list(edge)
  graph <- metric_graph$new(edges = edges)
  graph$build_mesh(n = n)
  return(graph)
}


## -------------------------------------------------------------------------------------------------------------------
gets.graph.circle <- function(n){
  r = 1/(pi)
  theta <- seq(from=-pi,to=pi,length.out = 100)
  edge <- cbind(1+r+r*cos(theta),r*sin(theta))
  edges = list(edge)
  graph <- metric_graph$new(edges = edges)
  graph$build_mesh(n = n)
  return(graph)
}


## -------------------------------------------------------------------------------------------------------------------
# Function to build a tadpole graph and create a mesh
gets.graph.tadpole <- function(h){
  edge1 <- rbind(c(0,0),c(1,0))
  theta <- seq(from=-pi,to=pi,length.out = 100)
  edge2 <- cbind(1+1/pi+cos(theta)/pi,sin(theta)/pi)
  edges <- list(edge1, edge2)
  graph <- metric_graph$new(edges = edges, verbose = 0)
  graph$set_manual_edge_lengths(edge_lengths = c(1,2))
  graph$build_mesh(h = h)
  return(graph)
}


## -------------------------------------------------------------------------------------------------------------------
tadpole.eig <- function(k,graph){
x1 <- c(0,graph$get_edge_lengths()[1]*graph$mesh$PtE[graph$mesh$PtE[,1]==1,2]) 
x2 <- c(0,graph$get_edge_lengths()[2]*graph$mesh$PtE[graph$mesh$PtE[,1]==2,2]) 

if(k==0){ 
  f.e1 <- rep(1,length(x1)) 
  f.e2 <- rep(1,length(x2)) 
  f1 = c(f.e1[1],f.e2[1],f.e1[-1], f.e2[-1]) 
  f = list(phi=f1/sqrt(3)) 
  
} else {
  f.e1 <- -2*sin(pi*k*1/2)*cos(pi*k*x1/2) 
  f.e2 <- sin(pi*k*x2/2)                  
  
  f1 = c(f.e1[1],f.e2[1],f.e1[-1], f.e2[-1]) 
  
  if((k %% 2)==1){ 
    f = list(phi=f1/sqrt(3)) 
  } else { 
    f.e1 <- (-1)^{k/2}*cos(pi*k*x1/2)
    f.e2 <- cos(pi*k*x2/2)
    f2 = c(f.e1[1],f.e2[1],f.e1[-1],f.e2[-1]) 
    f <- list(phi=f1,psi=f2/sqrt(3/2))
  }
}
return(f)
}


## -------------------------------------------------------------------------------------------------------------------
# Function to compute the eigenpairs of the tadpole graph
gets.eigen.params <- function(N_finite = 4, kappa = 1, alpha = 0.5, graph){
  EIGENVAL <- NULL
  EIGENVAL_ALPHA <- NULL
  EIGENVAL_MINUS_ALPHA <- NULL
  EIGENFUN <- NULL
  INDEX <- NULL
  for (j in 0:N_finite) {
    lambda_j <- kappa^2 + (j*pi/2)^2
    lambda_j_alpha_half <- lambda_j^(alpha/2)
    lambda_j_minus_alpha_half <- lambda_j^(-alpha/2)
    e_j <- tadpole.eig(j,graph)$phi
    EIGENVAL <- c(EIGENVAL, lambda_j)
    EIGENVAL_ALPHA <- c(EIGENVAL_ALPHA, lambda_j_alpha_half)  
    EIGENVAL_MINUS_ALPHA <- c(EIGENVAL_MINUS_ALPHA, lambda_j_minus_alpha_half)
    EIGENFUN <- cbind(EIGENFUN, e_j)
    INDEX <- c(INDEX, j)
    if (j>0 && (j %% 2 == 0)) {
      lambda_j <- kappa^2 + (j*pi/2)^2
      lambda_j_alpha_half <- lambda_j^(alpha/2)
      lambda_j_minus_alpha_half <- lambda_j^(-alpha/2)
      e_j <- tadpole.eig(j,graph)$psi
      EIGENVAL <- c(EIGENVAL, lambda_j)
      EIGENVAL_ALPHA <- c(EIGENVAL_ALPHA, lambda_j_alpha_half)    
      EIGENVAL_MINUS_ALPHA <- c(EIGENVAL_MINUS_ALPHA, lambda_j_minus_alpha_half)
      EIGENFUN <- cbind(EIGENFUN, e_j)
      INDEX <- c(INDEX, j+0.1)
      }
    }
  return(list(EIGENVAL = EIGENVAL,
              EIGENVAL_ALPHA = EIGENVAL_ALPHA, 
              EIGENVAL_MINUS_ALPHA = EIGENVAL_MINUS_ALPHA,
              EIGENFUN = EIGENFUN,
              INDEX = INDEX))
}


## -------------------------------------------------------------------------------------------------------------------
# Function to order the vertices for plotting
plotting.order <- function(v, graph){
  edge_number <- graph$mesh$VtE[, 1]
  pos <- sum(edge_number == 1)+1
  return(c(v[1], v[3:pos], v[2], v[(pos+1):length(v)], v[2]))
}


## -------------------------------------------------------------------------------------------------------------------
# Original camera
eye <- list(x = 5, y = 3, z = 4)
center <- list(x = (1+2/pi)/2, y = 0, z = 0)

# Fraction to move toward center (zoom in)
f <- 0  # 50% closer

new_eye <- list(
  x = eye$x + f * (center$x - eye$x),
  y = eye$y + f * (center$y - eye$y),
  z = eye$z + f * (center$z - eye$z)
)

tadpole.layout <- function(x_range, y_range, z_range){
  return(list(xaxis = list(title = list(text = "x", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = x_range),
              yaxis = list(title = list(text = "y", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = y_range),
              zaxis = list(title = list(text = "z", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = z_range),
              aspectratio = list(x = 2*(1+2/pi), 
                                 y = 2*(2/pi), 
                                 z = 1*(2/pi)),
              camera = list(eye = list(x = 5, y = 3, z = 4),
                            center = list(x = (1+2/pi)/2, y = 0, z = 0))))
}

tadpole.layout.with.zoom <- function(x_range, y_range, z_range){
  return(list(xaxis = list(title = list(text = "x", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = x_range),
              yaxis = list(title = list(text = "y", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = y_range),
              zaxis = list(title = list(text = "z", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = z_range),
              aspectratio = list(x = 2*(1+2/pi), 
                                 y = 2*(2/pi), 
                                 z = 1*(2/pi)),
              camera = list(eye = new_eye,
                            center = center)))
}


## -------------------------------------------------------------------------------------------------------------------
myggsave <- function(plot, width = 9.22, height = 7.05) {
  
  dir_to_save <- here::here("data_files/tikzpic")
  obj_name <- deparse(substitute(plot))
  tex_name <- file.path(dir_to_save, paste0(obj_name, ".tex"))
  
  # Create directory if it doesn't exist
  if (!dir.exists(dir_to_save)) dir.create(dir_to_save, recursive = TRUE)
  
  # Save TikZ plot
  tikzDevice::tikz(tex_name, standAlone = TRUE, width = width, height = height)
  print(plot)
  dev.off()
  
  # Compile to PDF
  system(paste0("pdflatex -output-directory=", dir_to_save, " ", tex_name))
  
  # Remove auxiliary files
  aux_ext <- c(".aux", ".log", ".tex")
  for (ext in aux_ext) {
    f <- file.path(dir_to_save, paste0(obj_name, ext))
    if (file.exists(f)) file.remove(f)
  }
  
  # Remove any temporary raster images generated by tikzDevice
  ras_files <- list.files(
    dir_to_save,
    pattern = paste0(obj_name, "_ras[0-9]+\\.png$"),
    full.names = TRUE
  )
  if (length(ras_files) > 0) file.remove(ras_files)
  
  message("PDF saved at: ", file.path(dir_to_save, paste0(obj_name, ".pdf")))
}


## -------------------------------------------------------------------------------------------------------------------

library(plotly)
library(reticulate)
library(jsonlite)
library(glue)

combine_plotly_grid_pdf <- function(plots, output_pdf = "combined_grid.pdf", 
                                    ncol = 2, width = 1400, height = 1000, scale = 2, 
                                    resolution = 300, spacing = 50) {
  py_config() # Check Python configuration
  if (!is.list(plots) || length(plots) < 2) {
    stop("plots must be a list of at least two plotly objects")
  }
  
  # Temporary PNG files
  tmp_files <- sapply(seq_along(plots), function(i) tempfile(fileext = ".png"))
  
  # Save all plots as PNGs
  for (i in seq_along(plots)) {
    plotly::save_image(plots[[i]], tmp_files[i], width = width, height = height, scale = scale)
  }
  
  # Convert temporary files to JSON array for Python
  py_tmp_files <- toJSON(tmp_files, auto_unbox = TRUE)
  
  # Python code using glue (variables directly substituted)
  py_code <- glue("
from PIL import Image
import math

files = {py_tmp_files}
images = [Image.open(f) for f in files]

ncol = {ncol}
nrow = math.ceil(len(images)/ncol)
spacing = {spacing}

cell_width = max(img.width for img in images)
cell_height = max(img.height for img in images)

combined_width = cell_width * ncol + spacing * (ncol - 1)
combined_height = cell_height * nrow + spacing * (nrow - 1)

combined = Image.new('RGB', (combined_width, combined_height), (255, 255, 255))

for idx, img in enumerate(images):
    row = idx // ncol
    col = idx % ncol
    x = col * (cell_width + spacing)
    y = row * (cell_height + spacing)
    combined.paste(img, (x, y))

combined.save(r'{output_pdf}', 'PDF', resolution={resolution})
  ")
  
  # Run Python code
  py_run_string(py_code)
  
  # Clean up temporary files
  file.remove(tmp_files)
  
  message("Combined PDF saved to: ", output_pdf)
}

# p1 <- plot_ly(z = ~volcano, type = "surface") %>% layout(title = "Plot 1")
# p2 <- plot_ly(z = ~volcano + 5, type = "surface") %>% layout(title = "Plot 2")
# p3 <- plot_ly(z = ~volcano + 10, type = "surface") %>% layout(title = "Plot 3")
# p4 <- plot_ly(z = ~volcano + 15, type = "surface") %>% layout(title = "Plot 4")
# 
# combine_plotly_grid_pdf(list(p1, p2, p3, p4), output_pdf = "plots_2x2.pdf", ncol = 4)


## -------------------------------------------------------------------------------------------------------------------
library(plotly)
library(reticulate)


combine_plotly_pdf <- function(p1, p2, output_pdf = "combined_side_by_side.pdf", 
                               width = 1400, height = 1000, scale = 2, resolution = 300,
                               spacing = 50) {
  py_config() # Check Python configuration
  # Temporary PNG paths
  tmp1 <- tempfile(fileext = ".png")
  tmp2 <- tempfile(fileext = ".png")
  
  # Save the plotly plots as PNGs
  plotly::save_image(p1, tmp1, width = width, height = height, scale = scale)
  plotly::save_image(p2, tmp2, width = width, height = height, scale = scale)
  
  # Python code to combine PNGs side by side
  py_run_string(sprintf("
from PIL import Image

img1 = Image.open(r'%s')
img2 = Image.open(r'%s')

# Compute dimensions
combined_width = img1.width + img2.width + %d
combined_height = max(img1.height, img2.height)

combined = Image.new('RGB', (combined_width, combined_height), (255, 255, 255))
combined.paste(img1, (0, 0))
combined.paste(img2, (img1.width + %d, 0))

# Save as PDF
combined.save(r'%s', 'PDF', resolution=%d)
", tmp1, tmp2, spacing, spacing, output_pdf, resolution))
  
  # Remove temporary PNGs
  file.remove(tmp1, tmp2)
  
  message("Combined PDF saved to: ", output_pdf)
}


# # Create example 3D plotly plots
# p1 <- plot_ly(z = ~volcano, type = "surface") %>% layout(title = "Plot 1")
# p2 <- plot_ly(z = ~volcano + 10, type = "surface") %>% layout(title = "Plot 2")
# 
# # Combine into one PDF
# combine_plotly_pdf(p1, p2, output_pdf = "my_plots.pdf")


## -------------------------------------------------------------------------------------------------------------------
library(plotly)
library(reticulate)

combine_plotly_pdf_single <- function(p, output_pdf = "single_plot.pdf", 
                                      width = 1400, height = 1000, scale = 2, 
                                      resolution = 300) {
  py_config()  # Check Python configuration
  
  # Temporary PNG path
  tmp_png <- tempfile(fileext = ".png")
  
  # Save the Plotly plot as PNG
  plotly::save_image(p, tmp_png, width = width, height = height, scale = scale)
  
  # Python code: open PNG, convert to RGB, save as PDF
  py_run_string(sprintf("
from PIL import Image

img = Image.open(r'%s')
img_rgb = img.convert('RGB')
img_rgb.save(r'%s', 'PDF', resolution=%d)
", tmp_png, output_pdf, resolution))
  
  # Clean up temporary PNG
  file.remove(tmp_png)
  
  message("PDF saved to: ", normalizePath(output_pdf))
}
# combine_plotly_pdf_single(p3, output_pdf = "single_plot.pdf")
# # Example plot
# p1 <- plot_ly(z = ~volcano, type = "surface") %>% layout(title = "Single Plot")
# 
# # Save as PDF
# combine_plotly_pdf_single(p1, output_pdf = "single_plot.pdf")


## -------------------------------------------------------------------------------------------------------------------

loglog_line_equation <- function(x1, y1, slope) {
  b <- log10(y1 / (x1 ^ slope))
  
  function(x) {
    (x ^ slope) * (10 ^ b)
  }
}
exp_line_equation <- function(x1, y1, slope) {
  lnC <- log(y1) - slope * x1
  
  function(x) {
    exp(lnC + slope * x)
  }
}
compute_guiding_lines <- function(x_axis_vector, errors, theoretical_rates, line_equation_fun) {
  guiding_lines <- matrix(NA, nrow = length(x_axis_vector), ncol = length(theoretical_rates))
  
  for (j in seq_along(theoretical_rates)) {
    guiding_lines_aux <- matrix(NA, nrow = length(x_axis_vector), ncol = length(x_axis_vector))
    
    for (k in seq_along(x_axis_vector)) {
      point_x1 <- x_axis_vector[k]
      point_y1 <- errors[k, j]
      slope <- theoretical_rates[j]
      
      line <- line_equation_fun(x1 = point_x1, y1 = point_y1, slope = slope)
      guiding_lines_aux[, k] <- line(x_axis_vector)
    }
    
    guiding_lines[, j] <- rowMeans(guiding_lines_aux)
  }
  
  return(guiding_lines)
}

error.convergence.plotter <- function(x_axis_vector, 
                                      alpha_vector, 
                                      errors, 
                                      theoretical_rates, 
                                      observed_rates,
                                      line_equation_fun,
                                      fig_title,
                                      x_axis_label,
                                      y_axis_label,
                                      color_label,
                                      apply_sqrt = FALSE) {
  
  relative_per_error <- 100 * abs(theoretical_rates - observed_rates) / abs(theoretical_rates)
  
  x_vec <- if (apply_sqrt) sqrt(x_axis_vector) else x_axis_vector
  
  guiding_lines <- compute_guiding_lines(x_axis_vector = x_vec, 
                                         errors = errors, 
                                         theoretical_rates = theoretical_rates, 
                                         line_equation_fun = line_equation_fun)
  
  default_colors <- scales::hue_pal()(length(alpha_vector))
  
  plot_lines <- lapply(1:ncol(guiding_lines), function(i) {
    geom_line(
      data = data.frame(x = x_vec, y = guiding_lines[, i]),
      aes(x = x, y = y),
      color = default_colors[i],
      linetype = "dashed",
      show.legend = FALSE
    )
  })
  
  df <- as.data.frame(cbind(x_vec, errors))
  colnames(df) <- c("x_axis_vector", alpha_vector)
  df_melted <- melt(df, id.vars = "x_axis_vector", variable.name = "column", value.name = "value")
  
  custom_labels <- sprintf("$%s \\; | \\; %s \\; | \\; %s \\; | \\; %s$",
                           formatC(alpha_vector, format = "f", digits = 2),
                           formatC(theoretical_rates, format = "f", digits = 2),
                           formatC(observed_rates, format = "f", digits = 2),
                           formatC(relative_per_error, format = "f", digits = 2))
  
  df_melted$column <- factor(df_melted$column, levels = alpha_vector, labels = custom_labels)

  p <- ggplot() +
    geom_line(data = df_melted, aes(x = x_axis_vector, y = value, color = column)) +
    geom_point(data = df_melted, aes(x = x_axis_vector, y = value, color = column)) +
    plot_lines +
    labs(
      title = fig_title,
      x = x_axis_label,
      y = y_axis_label,
      color = color_label
    ) +
    (if (apply_sqrt) {
      scale_x_continuous(breaks = x_vec, labels = round(x_axis_vector, 4))
    } else {
      scale_x_log10(breaks = x_axis_vector, labels = round(x_axis_vector, 4))
    }) +
    (if (apply_sqrt) {
      #scale_y_continuous(trans = "log", labels = scales::scientific_format())
      scale_y_continuous(trans = scales::log_trans(base = exp(1)), labels = scales::scientific_format())
    } else {
      scale_y_log10(labels = scales::scientific_format())
    }) +
    theme_minimal() +
    theme(text = element_text(family = "Palatino"),
          legend.position = "right",
          legend.direction = "vertical",
          #plot.margin = margin(0, 0, 0, 0),
          plot.title = element_text(hjust = 0.5, size = 18, face = "bold"))
  
  return(p)
}



## -------------------------------------------------------------------------------------------------------------------
generate_graph_on_sphere <- function(n,
                                     k = 6,   # polar circles
                                     m = 12,  # tropics
                                     M = 24){
  # ---- latitude structure (symmetric) ----
  z_levels <- c(1,  0.75, 0.35, 0, -0.35, -0.75, -1)
  counts   <- c(1,  k,    m,    M,  m,     k,    1)
  
  # ---- check consistency ----
  if (sum(counts) != n) {
    stop(sprintf("n must be %d for given k, m, M", sum(counts)))
  }
  
  nodes_list <- list()
  idx <- 1
  
  for (b in seq_along(z_levels)) {
    
    z <- z_levels[b]
    nb <- counts[b]
    
    if (nb == 1) {
      # poles
      nodes_list[[idx]] <- matrix(c(0, 0, z), ncol = 3)
      idx <- idx + 1
      next
    }
    
    # ---- uniform circle ----
    theta <- seq(0, 2*pi, length.out = nb + 1)[- (nb + 1)]
    
    r <- sqrt(1 - z^2)
    
    circle <- cbind(
      r * cos(theta),
      r * sin(theta),
      rep(z, nb)
    )
    
    nodes_list[[idx]] <- circle
    idx <- idx + 1
  }
  
  nodes <- do.call(rbind, nodes_list)
  nodes2d <- nodes[, 1:2]
  
  # ---- fully connected edges ----
  edges <- list()
  edges2d <- list()
  k <- 1
  
  for (i in 1:(nrow(nodes) - 1)) {
    for (j in (i + 1):nrow(nodes)) {
      edges[[k]] <- rbind(nodes[i, ], nodes[j, ])
      edges2d[[k]] <- rbind(nodes2d[i, ], nodes2d[j, ])
      k <- k + 1
    }
  }
  return(
  list(
    nodes = nodes,
    edges = edges,
    edges2d = edges2d,
    structure = list(
      z_levels = z_levels,
      counts = counts
    )
  ))
}


## -------------------------------------------------------------------------------------------------------------------
plot_sparse_pattern <- function(A) {
  s <- summary(A)
  
  p <- ggplot(s, aes(x = j, y = i)) +
    geom_point(size = 1) +
    scale_y_reverse() +
    scale_x_continuous(position = "top") +  # move x-axis to top
    coord_fixed() +
    labs(x = "Column", y = "Row") +
    theme_bw() +
    theme(
      panel.grid = element_blank()
    )
  return(p)
}

