library(ggplot2)
library(tikzDevice)

df <- data.frame(x = 1:10, y = (1:10)^2)

# Create TikZ file in old/
tikz("old/ggplot.tex", standAlone = TRUE)
print(
  ggplot(df, aes(x, y)) +
    geom_line() +
    labs(title = "$f(x)$") +
    theme_minimal()
)
dev.off()

# Compile LaTeX with output in old/
system("pdflatex -output-directory=old old/ggplot.tex")
browseURL("old/ggplot.pdf")



TAU <- logo_graph$plot_function(X = model_for_tau, vertex_size = 0) +
  ggtitle("$\\tau(s) = e^{0.05\\cdot(x(s)-y(s))}$") +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"),
        plot.title = element_text(hjust = 0.5, size = 12))



tikz("old/ggplot.tex", standAlone = TRUE)
print(
  r1
)
dev.off()

# Compile LaTeX with output in old/
system("pdflatex -output-directory=old old/ggplot.tex")
browseURL("old/ggplot.pdf")


r1 <- logo_graph$plot_function(X = est_range, vertex_size = 0) +
  ggtitle("$\\rho(s)$") +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"),
        plot.title = element_text(hjust = 0.5, size = 12))  +
  annotate("point", x = xypoints[m1,1], y = xypoints[m1,2], size = 2, color = "#0000C8") +
  annotate("point", x = xypoints[m2,1], y = xypoints[m2,2], size = 2, color = "red") +
  annotate("point", x = xypoints[m3,1], y = xypoints[m3,2], size = 2, color = "darkgreen") +
  annotate("text", x = xypoints[m1,1] + 0.1, y = xypoints[m1,2] - 0.4, label = "$s_1$",  size = 4, hjust = 0, color = "black") +
  annotate("text", x = xypoints[m2,1] + 0.4, y = xypoints[m2,2], label = "$s_2$",  size = 4, hjust = 0, color = "black") +
  annotate("text", x = xypoints[m3,1] - 0.8, y = xypoints[m3,2], label = "$s_3$",  size = 4, hjust = 0, color = "black")






# Create a plot of the model for tau
TAU <- logo_graph$plot_function(X = model_for_tau, vertex_size = 0, edge_width = 2) +
  ggtitle("$\\tau(s) = e^{0.05\\cdot(x(s)-y(s))}$") +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"),
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height = unit(1, "cm")) 
# Create a plot of the model for kappa
KAPPA <- logo_graph$plot_function(X = model_for_kappa, vertex_size = 0, edge_width = 2) +
  ggtitle("$\\kappa(s) = e^{0.1\\cdot(x(s)-y(s))}$") +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"),
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height = unit(1, "cm")) 
# Create a plot for the practical correlation range
r1 <- logo_graph$plot_function(X = est_range, vertex_size = 0, edge_width = 2) +
  ggtitle("$\\rho(s)$") +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"),
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height = unit(1, "cm")) +
  annotate("point", x = xypoints[m1,1], y = xypoints[m1,2], size = 2, color = "#0000C8") +
  annotate("point", x = xypoints[m2,1], y = xypoints[m2,2], size = 2, color = "red") +
  annotate("point", x = xypoints[m3,1], y = xypoints[m3,2], size = 2, color = "darkgreen") +
  annotate("text", x = xypoints[m1,1] + 0.1, y = xypoints[m1,2] - 0.4, label = "$s_1$", size = 4, hjust = 0, color = "black") +
  annotate("text", x = xypoints[m2,1] + 0.4, y = xypoints[m2,2], label = "$s_2$", size = 4, hjust = 0, color = "black") +
  annotate("text", x = xypoints[m3,1] - 0.8, y = xypoints[m3,2], label = "$s_3$", size = 4, hjust = 0, color = "black")
# Create a plot for the standard deviation
s1 <- logo_graph$plot_function(X = est_sigma, vertex_size = 0, edge_width = 2) +
  ggtitle("$\\sigma(s)$") +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"),
        plot.title = element_text(hjust = 0.5, size = 12),
        legend.key.width = unit(0.2, "cm"),
        legend.key.height = unit(1, "cm"))  +
  annotate("point", x = xypoints[m1,1], y = xypoints[m1,2], size = 2, color = "#0000C8") +
  annotate("point", x = xypoints[m2,1], y = xypoints[m2,2], size = 2, color = "red") +
  annotate("point", x = xypoints[m3,1], y = xypoints[m3,2], size = 2, color = "darkgreen") +
  annotate("text", x = xypoints[m1,1] + 0.1, y = xypoints[m1,2] - 0.4, label = "$s_1$", size = 4, hjust = 0, color = "black") +
  annotate("text", x = xypoints[m2,1] + 0.4, y = xypoints[m2,2], label = "$s_2$", size = 4, hjust = 0, color = "black") +
  annotate("text", x = xypoints[m3,1] - 0.8, y = xypoints[m3,2], label = "$s_3$", size = 4, hjust = 0, color = "black")
# Combine the four plots
four_plots <- (TAU + KAPPA) / (s1 + r1)

tikz("old/ggplot.tex", standAlone = TRUE , width = 9.22, height = 7.05)
print(
  four_plots
)
dev.off()

# Compile LaTeX with output in old/
system("pdflatex -output-directory=old old/ggplot.tex")
browseURL("old/ggplot.pdf")

myggsave <- function(plot, width = 9.22, height = 7.05) {
  dir_to_save <- here::here("data_files/tikzpic")
  obj_name <- deparse(substitute(plot))
  tex_name <- paste0(dir_to_save, "/", obj_name, ".tex")
  tikz(tex_name, standAlone = TRUE, width = width, height = height)
  print(plot)
  dev.off()
  system(paste0("pdflatex -output-directory=", dir_to_save, " ", tex_name))
}


myggsave <- function(plot, width = 9.22, height = 7.05) {
  library(here)
  dir_to_save <- here("data_files/tikzpic")
  obj_name <- deparse(substitute(plot))
  tex_name <- file.path(dir_to_save, paste0(obj_name, ".tex"))
  
  # Create directory if it doesn't exist
  if (!dir.exists(dir_to_save)) dir.create(dir_to_save, recursive = TRUE)
  
  # Save TikZ plot
  tikz(tex_name, standAlone = TRUE, width = width, height = height)
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

myggsave(TAU, width = 9.22, height = 7.05)







