```{r, eval = TRUE}
library(reticulate)
#use_python("/home/rierasl/miniconda3/envs/phdenv/bin/python", required = TRUE)
py_config()
library(plotly)

p <- plot_ly(z = ~volcano, type = "surface")
# Save static images in figures folder
plotly::save_image(p, "plot3d.png", width = 1400, height = 1000)
plotly::save_image(p, "plot3d.pdf")
plotly::save_image(p, "plot3d.svg")
```


```{r, eval = TRUE}
library(plotly)

# Create plots
p1 <- plot_ly(z = ~volcano, type = "surface") %>% layout(title = "Plot 1")
p2 <- plot_ly(z = ~volcano + 10, type = "surface") %>% layout(title = "Plot 2")

# Save each separately in high resolution
plotly::save_image(p1, "p1.png", width = 1400, height = 1000, scale = 2)
plotly::save_image(p2, "p2.png", width = 1400, height = 1000, scale = 2)
```

```{python}
from PIL import Image

img1 = Image.open("p1.png")
img2 = Image.open("p2.png")

combined = Image.new('RGB', (img1.width + img2.width, img1.height))
combined.paste(img1, (0,0))
combined.paste(img2, (img1.width,0))
combined.save("combined.png")
```

```{python}
from PIL import Image

img1 = Image.open("p1.png")
img2 = Image.open("p2.png")

combined = Image.new('RGB', (img1.width + img2.width, max(img1.height, img2.height)))
combined.paste(img1, (0, 0))
combined.paste(img2, (img1.width, 0))
combined.save("combined_side_by_side.pdf", "PDF", resolution=300.0)
```


```{r}
library(plotly)

# Your 3D plots
p1 <- plot_ly(z = ~volcano, type = "surface") %>% layout(title = "Plot 1")
p2 <- plot_ly(z = ~volcano + 10, type = "surface") %>% layout(title = "Plot 2")

# Save each plot as PDF
plotly::save_image(p1, "p1.pdf", width = 1400, height = 1000, scale = 2)
plotly::save_image(p2, "p2.pdf", width = 1400, height = 1000, scale = 2)
```


```{python}
from PyPDF2 import PdfReader, PdfWriter
from PyPDF2 import Transformation

# Load PDFs
pdf1 = PdfReader("p1.pdf")
pdf2 = PdfReader("p2.pdf")

writer = PdfWriter()

page1 = pdf1.pages[0]
page2 = pdf2.pages[0]

# Get dimensions
w1 = float(page1.mediabox.width)
h1 = float(page1.mediabox.height)
w2 = float(page2.mediabox.width)
h2 = float(page2.mediabox.height)

# Enlarge page1 to fit both PDFs side by side
page1.mediabox.upper_right = (w1 + w2, max(h1, h2))

# Translate page2 and merge into page1
page2.add_transformation(Transformation().translate(tx=w1, ty=0))
page1.merge_page(page2)

# Add the combined page to writer
writer.add_page(page1)

# Save
with open("combined_side_by_side.pdf", "wb") as f:
  writer.write(f)
```






library(plotly)
library(htmlwidgets)
library(webshot2)  # lightweight, no sudo needed
library(rsvg)

# Your Plotly plot

p2 <- plot_ly(z = ~volcano + 10, type = "surface") %>% layout(title = "Plot 2")

# Save as self-contained HTML
htmlwidgets::saveWidget(p2, "temp_plot.html", selfcontained = TRUE)

# Take SVG screenshot with webshot2
webshot2::webshot("temp_plot.html", "temp_plot.svg", vwidth = 800, vheight = 600)








# from graphs 6


```
{r, eval  = FALSE}
library(plotly)

# Parameters
a <- 1
b <- 0.5

# Total arc-length
L <- sqrt(a^2 + b^2) * 6*pi

# Arc-length parametrization (helix around x-axis)
alpha_tilde <- function(s){
  t <- s / sqrt(a^2 + b^2)
  x <- b*t
  y <- a*cos(t)
  z <- a*sin(t)
  data.frame(x=x, y=y, z=z)
}

max_x <- b * (L / sqrt(a^2 + b^2))

half_max_x <- max_x / 2

# Smooth helix
n_smooth <- 500
s_smooth <- seq(0, L, length.out = n_smooth)
curve_smooth <- alpha_tilde(s_smooth)

# Points for mapping lines
n_map <- 50
s_map <- seq(0, L, length.out = n_map)
curve_map <- alpha_tilde(s_map)

# Interval along x-axis
int_map <- data.frame(
  x = s_map * max_x / L,
  y = rep(0, n_map),
  z = rep(0, n_map)
)

# Build mapping lines
rows <- lapply(1:n_map, function(i) {
  list(int_map[i, ], curve_map[i, ], data.frame(x = NA, y = NA, z = NA))
})

result <- do.call(rbind, unlist(rows, recursive = FALSE))

# Plot
p <- plot_ly() |>
  
  # Smooth helix
  add_trace(
    data = curve_smooth,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "#0000C8", width = 7),
    showlegend = FALSE
  ) |>
  
  # Sample helix points
  add_trace(
    data = curve_map,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "markers",
    marker = list(color = "black", size = 4),
    showlegend = FALSE
  ) |>
  
  # Interval [0,L]
  add_trace(
    data = int_map,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "#0000C8", width = 7),
    showlegend = FALSE
  ) |>
  
  # Interval points
  add_trace(
    data = int_map,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "markers",
    marker = list(color = "black", size = 4),
    showlegend = FALSE
  ) |>
  
  # Mapping lines
  add_trace(
    data = result,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "gray", width = 1),
    showlegend = FALSE
  )

p1 <- layout(p,
             margin = list(l = 0, r = 0, b = 0, t = 0),
             scene = list(
               xaxis = list(title = "x",  range = c(0, max_x)),
               yaxis = list(title = "y", range = c(-1, 1)),
               zaxis = list(title = "z", range = c(-1, 1)),
               #aspectmode="data",
               aspectratio = list(x = max_x/2, y = 1, z = 1),
               camera = list(eye = list(x = 0, y = -4, z = 4),
                             center = list(x = 0, y = 0, z = 0))
             )
)

save(p1, file = here::here("data_files/graphs6p1.Rdata"))
load(here::here("data_files/graphs6p1.Rdata"))
p1
```


```
{r}
library(plotly)

# Parameters
a <- 1
b <- 0.5
TT <- 6*pi
# Total arc-length
L <- sqrt(a^2 + b^2) * TT

half_L <- L / 2


max_x <- b * (L / sqrt(a^2 + b^2))

half_max_x <- max_x / 2

dist_to_move <- half_L - half_max_x

# Arc-length parametrization (helix around x-axis)
alpha_tilde <- function(s){
  t <- s / sqrt(a^2 + b^2)
  x_shift <- TT*(sqrt(a^2 + b^2) - b) / 2
  x <- b * t + x_shift
  y <- a * cos(t)
  z <- a * sin(t)
  data.frame(x=x, y=y, z=z)
}

a0 = alpha_tilde(0)
aL = alpha_tilde(L)
aLh = alpha_tilde(half_L)

# Smooth helix
n_smooth <- 500
s_smooth <- seq(0, L, length.out = n_smooth)
curve_smooth <- alpha_tilde(s_smooth)

# Points for mapping lines
n_map <- 50
s_map <- seq(0, L, length.out = n_map)
curve_map <- alpha_tilde(s_map)

# Interval along x-axis
int_map <- data.frame(
  x = s_map,
  y = rep(0, n_map)+4,
  z = rep(0, n_map)
)

# Build mapping lines
rows <- lapply(1:n_map, function(i) {
  list(int_map[i, ], curve_map[i, ], data.frame(x = NA, y = NA, z = NA))
})

result <- do.call(rbind, unlist(rows, recursive = FALSE))

# Plot
p <- plot_ly() |>
  
  # Smooth helix
  add_trace(
    data = curve_smooth,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "darkgreen", width = 7),
    showlegend = FALSE
  ) |>
  
  # Sample helix points
  add_trace(
    data = curve_map,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "markers",
    marker = list(color = "black", size = 3),
    showlegend = FALSE
  ) |>
  
  # Interval [0,L]
  add_trace(
    data = int_map,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "#0000C8", width = 7),
    showlegend = FALSE
  ) |>
  
  # Interval points
  add_trace(
    data = int_map,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "markers",
    marker = list(color = "black", size = 3),
    showlegend = FALSE
  ) |>
  
  # Mapping lines
  add_trace(
    data = result,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "gray", width = 1),
    showlegend = FALSE
  )


p2 <- p |> config(mathjax = 'cdn') |>
  layout(p,
         font = list(family = "Palatino"),
         margin = list(l = 0, r = 0, b = 0, t = 0),
         scene = list(xaxis = list(title = list(text = "x", font = list(color = colaxnn)),  tickfont = list(color = colaxnn),  range = c(0, L)),
                      yaxis = list(title = list(text = "y", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = c(-1, 4)),
                      zaxis = list(title = list(text = "z", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = c(-1, 1)),
                      #aspectmode="data",
                      aspectratio = list(x = L/3, y = 1, z = 1),
                      camera = list(eye = list(x = 0, y = -4, z = 4),
                                    center = list(x = 0, y = 0, z = 0)),
                      annotations = list(
                        list(
                          x = 0, y = 0, z = 0,
                          text = TeX("0"),
                          textangle = 0, ax = 0, ay = 35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = L, y = 0, z = 0,
                          text = TeX("\\ell"),
                          textangle = 0, ax = 0, ay = 35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),   
                        list(
                          x = aLh$x, y = aLh$y, z = aLh$z,
                          text = TeX("e"),
                          textangle = 0, ax = 0, ay = 35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = aL$x, y = aL$y, z = aL$z,
                          text = TeX("v_2"),
                          textangle = 0, ax = 0, ay = 35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),      
                        list(
                          x = a0$x, y = a0$y, z = a0$z,
                          text = TeX("v_1"),
                          textangle = 0, ax = 0, ay = 35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1))
         )
  )

```




```
{r}
library(plotly)
n_smooth <- 500

TT <- 3*pi
# parameter
t <- seq(0, TT, length.out = n_smooth)

# speed
speed <- sqrt(1 + cos(t)^2)

# arc-length function
s <- pracma::cumtrapz(t, speed)
L <- max(s)
# s <- cumsum(c(0, diff(t) * (head(speed,-1) + tail(speed,-1))/2))
f1 <- function(t) exp(t/4)


f1_on_curve <- f1(s)
y_up_range <- max(f1_on_curve)

f2 <- function(t) sin(3*pi*t/L) + y_up_range
f1df_int <- data.frame(x = s, y = rep(0, n_smooth), z = f1_on_curve)

half_L <- L / 2
half_TT <- TT / 2
dist_to_move <- abs(half_L - half_TT)

y_shift <- 4
# curve alpha(t)
x <- t + dist_to_move
y <- sin(t) + y_shift
z <- rep(0,length(t))

curve_smooth <- data.frame(x = x, y = y, z = z)
f1_on_curve_smooth <- data.frame(x = x, y = y, z = f1_on_curve)

n_map <- 25
# points where we draw connectors
idx <- seq(1, length(t), length.out = n_map)

f1df_int_map <- f1df_int[idx,]
f1_on_curve_smooth_map <- f1_on_curve_smooth[idx,]

# Build mapping lines
rows <- lapply(1:n_map, function(i) {
  list(f1df_int_map[i, ], f1_on_curve_smooth_map[i, ], data.frame(x = NA, y = NA, z = NA))
})

resultee1 <- do.call(rbind, unlist(rows, recursive = FALSE))





curve_map <- data.frame(x = x[idx], y = y[idx], z = z[idx])
int_map <- data.frame(x = s[idx], y = rep(0, n_map), z = rep(0, n_map))


# Build mapping lines
rows <- lapply(1:n_map, function(i) {
  list(int_map[i, ], curve_map[i, ], data.frame(x = NA, y = NA, z = NA))
})

result <- do.call(rbind, unlist(rows, recursive = FALSE))

s_circ <- L
radius <- s_circ / (2*pi)
x_shift_for_circle <- radius + TT + dist_to_move

theta <- seq(from=-pi,to=pi,length.out = n_smooth)
circle_curve <- data.frame(x = radius*cos(theta) + x_shift_for_circle, 
                           y = radius*sin(theta) + y_shift, 
                           z = rep(0, length(theta)))

arclength_circle <- radius * (theta + pi)

dist_to_move_circle <- abs(half_L - y_shift)

f2_on_curve <- f2(arclength_circle)
f2df_int <- data.frame(x = rep(x_shift_for_circle +2*radius, n_smooth),
                       y = arclength_circle-dist_to_move_circle, 
                       z = f2_on_curve)

f2_on_curve_smooth <- data.frame(x = radius*cos(theta) + x_shift_for_circle, 
                                 y = radius*sin(theta) + y_shift,
                                 z = f2_on_curve)

f2df_int_map <- f2df_int[idx,]
f2_on_curve_smooth_map <- f2_on_curve_smooth[idx,]

# Build mapping lines
rows <- lapply(1:n_map, function(i) {
  list(f2df_int_map[i, ], f2_on_curve_smooth_map[i, ], data.frame(x = NA, y = NA, z = NA))
})

resultee2 <- do.call(rbind, unlist(rows, recursive = FALSE))



circle_map <- circle_curve[idx, ]
int_map_circle <- data.frame(x = rep(x_shift_for_circle +2*radius, n_map), 
                             y = s[idx]-dist_to_move_circle, 
                             z = rep(0, n_map))

# Build mapping lines for circle
rows <- lapply(1:n_map, function(i) {
  list(int_map_circle[i, ], circle_map[i, ], data.frame(x = NA, y = NA, z = NA))
})

result_for_circle <- do.call(rbind, unlist(rows, recursive = FALSE))

v1 <- curve_map[1, ]
v2 <- curve_map[n_map, ]
e1 <- curve_map[ceiling(n_map/2), ]
e2 <- circle_map[ceiling(n_map/2), ]
zero1 <- int_map[1, ]
le1 <- int_map[n_map, ]

zero2 <- int_map_circle[1, ]
le2 <- int_map_circle[n_map, ]

vertex <- rbind(v1, v2, zero1, zero2, le1, le2)

p <- plot_ly() |>
  # smooth sin curve
  add_trace(data = curve_smooth,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "black"),
            showlegend = FALSE
  ) |>
  # smooth circle curve
  add_trace(data = circle_curve,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "black"),
            showlegend = FALSE
  ) |>  
  # interval [0,L]
  add_trace(data = int_map,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "black"),
            showlegend = FALSE
  ) |>
  # interval for circle
  add_trace(data = int_map_circle,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "black"),
            showlegend = FALSE
  ) |>
  # plot on int for sin
  add_trace(data = f1df_int,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "blue"),
            showlegend = FALSE
  ) |>
  add_trace(x = rep(f1df_int$x, each = 3), 
            y = rep(f1df_int$y, each = 3), 
            z = unlist(lapply(f1df_int$z, function(zj) c(0, zj, NA))),
            type = "scatter3d", 
            mode = "lines",
            line = list(color = "lightgray", width = 0.5),
            showlegend = FALSE) |>
  # plot on sin for sin
  add_trace(data = f1_on_curve_smooth,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "blue"),
            showlegend = FALSE
  ) |>
  add_trace(x = rep(f1_on_curve_smooth$x, each = 3), 
            y = rep(f1_on_curve_smooth$y, each = 3), 
            z = unlist(lapply(f1_on_curve_smooth$z, function(zj) c(0, zj, NA))),
            type = "scatter3d", 
            mode = "lines",
            line = list(color = "lightgray", width = 0.5),
            showlegend = FALSE) |>
  # plot on int for cir
  add_trace(data = f2df_int,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "blue"),
            showlegend = FALSE
  ) |>
  add_trace(x = rep(f2df_int$x, each = 3), 
            y = rep(f2df_int$y, each = 3), 
            z = unlist(lapply(f2df_int$z, function(zj) c(0, zj, NA))),
            type = "scatter3d", 
            mode = "lines",
            line = list(color = "lightgray", width = 0.5),
            showlegend = FALSE) |>
  # plot on cir for cir
  add_trace(data = f2_on_curve_smooth,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "blue"),
            showlegend = FALSE
  ) |>
  add_trace(x = rep(f2_on_curve_smooth$x, each = 3), 
            y = rep(f2_on_curve_smooth$y, each = 3), 
            z = unlist(lapply(f2_on_curve_smooth$z, function(zj) c(0, zj, NA))),
            type = "scatter3d", 
            mode = "lines",
            line = list(color = "lightgray", width = 0.5),
            showlegend = FALSE) |>
  # Mapping lines
  add_trace(
    data = rbind(result, result_for_circle, resultee1, resultee2),
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "gray", width = 1),
    showlegend = FALSE
  )|>
  add_trace(data = vertex,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 5, color = "black"),
            showlegend = FALSE
  ) |> add_trace(
    type = "cone",
    x = e1$x,
    y = e1$y,
    z = e1$z,
    u = 1,
    v = 0,
    w = 0,
    sizemode = "absolute",
    sizeref = 0.4,
    colorscale = list(c(0, 1), c("green", "green")),
    showscale = FALSE
  ) |> add_trace(
    type = "cone",
    x = e2$x,
    y = e2$y,
    z = e2$z,
    u = 0,
    v = 1,
    w = 0,
    sizemode = "absolute",
    sizeref = 0.4,
    colorscale = list(c(0, 1), c("green", "green")),
    showscale = FALSE
  )

dx <- int_map$x - curve_map$x
dy <- int_map$y - curve_map$y
dz <- int_map$z - curve_map$z

norm <- sqrt(dx^2 + dy^2 + dz^2)

u <- dx / norm
v <- dy / norm
w <- dz / norm

dxc <- int_map_circle$x - circle_map$x
dyc <- int_map_circle$y - circle_map$y
dzc <- int_map_circle$z - circle_map$z

normc <- sqrt(dxc^2 + dyc^2 + dzc^2)

uc <- dxc / normc
vc <- dyc / normc
wc <- dzc / normc

p <- p |>
  add_trace(
    type = "cone",
    x = int_map$x,
    y = int_map$y,
    z = int_map$z,
    u = u,
    v = v,
    w = w,
    sizemode = "absolute",
    sizeref = 0.7,
    anchor = "tip",
    colorscale = list(c(0, "gray"), c(1, "gray")),
    showscale = FALSE
  ) |>
  add_trace(
    type = "cone",
    x = f1df_int_map$x,
    y = f1df_int_map$y,
    z = f1df_int_map$z,
    u = u,
    v = v,
    w = w,
    sizemode = "absolute",
    sizeref = 0.7,
    anchor = "tip",
    colorscale = list(c(0, "gray"), c(1, "gray")),
    showscale = FALSE
  ) |>
  add_trace(
    type = "cone",
    x = int_map_circle$x,
    y = int_map_circle$y,
    z = int_map_circle$z,
    u = uc,
    v = vc,
    w = wc,
    sizemode = "absolute",
    sizeref = 0.4,
    anchor = "tip",
    colorscale = list(c(0, "gray"), c(1, "gray")),
    showscale = FALSE
  ) |>
  add_trace(
    type = "cone",
    x = f2df_int_map$x,
    y = f2df_int_map$y,
    z = f2df_int_map$z,
    u = uc,
    v = vc,
    w = wc,
    sizemode = "absolute",
    sizeref = 0.4,
    anchor = "tip",
    colorscale = list(c(0, "gray"), c(1, "gray")),
    showscale = FALSE
  ) 

p4tadpole_arclength <- p |>
  config(mathjax = 'cdn') |> 
  layout(p,
         font = list(family = "Palatino"),
         margin = list(l = 0, r = 0, b = 0, t = 0),
         scene = list(xaxis = list(title = list(text = "x", font = list(color = colaxnn)),  tickfont = list(color = colaxnn),  range = c(0, x_shift_for_circle +2*radius)*1.01),
                      yaxis = list(title = list(text = "y", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = range(int_map_circle$y)),
                      zaxis = list(title = list(text = "z", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = c(0, y_up_range+1)),
                      aspectratio = list(x = x_shift_for_circle +2*radius, y = L, z = 4),
                      camera = list(eye = list(x = 0, y = -L*1.5, z = L*1.5),
                                    center = list(x = 0, y = 0, z = 0)),
                      annotations = list(
                        list(
                          x = zero1$x, y = zero1$y, z = zero1$z,
                          text = TeX("0"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = zero2$x, y = zero2$y, z = zero2$z,
                          text = TeX("0"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = le1$x, y = le1$y, z = le1$z,
                          text = TeX("\\ell_{e_1}"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = le2$x, y = le2$y, z = le2$z,
                          text = TeX("\\ell_{e_2}"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = v1$x, y = v1$y, z = v1$z,
                          text = TeX("v_1"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = v2$x, y = v2$y, z = v2$z,
                          text = TeX("v_2"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = e1$x, y = e1$y, z = e1$z,
                          text = TeX("e_1"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = e2$x, y = e2$y, z = e2$z,
                          text = TeX("e_2"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1))
                      
         )
  )

save(p4tadpole_arclength, file = here::here("data_files/graphs6p4tadpole_arclength.Rdata"))
```

# WITH GRADIENT

```
{r}
library(plotly)
n_smooth <- 1000

TT <- 3*pi
# parameter
t <- seq(0, TT, length.out = n_smooth)

# speed
speed <- sqrt(1 + cos(t)^2)

# arc-length function
s <- pracma::cumtrapz(t, speed)
# s <- cumsum(c(0, diff(t) * (head(speed,-1) + tail(speed,-1))/2))

L <- max(s)

half_L <- L / 2
half_TT <- TT / 2
dist_to_move <- abs(half_L - half_TT)

y_shift <- 4
# curve alpha(t)
x <- t + dist_to_move
y <- sin(t) + y_shift
z <- rep(0,length(t))

curve_smooth <- data.frame(x = x, y = y, z = z)


n_map <- 25
# points where we draw connectors
idx <- seq(1, length(t), length.out = n_map)


curve_map <- data.frame(x = x[idx], y = y[idx], z = z[idx])
int_map <- data.frame(x = s[idx], y = rep(0, n_map), z = rep(0, n_map))



# Build mapping lines
rows <- lapply(1:n_map, function(i) {
  list(int_map[i, ], curve_map[i, ], data.frame(x = NA, y = NA, z = NA))
})

result <- do.call(rbind, unlist(rows, recursive = FALSE))

s_circ <- L
radius <- s_circ / (2*pi)
x_shift_for_circle <- radius + TT + dist_to_move

theta <- seq(from=-pi,to=pi,length.out = n_smooth)
circle_curve <- data.frame(x = radius*cos(theta) + x_shift_for_circle, 
                           y = radius*sin(theta) + y_shift, 
                           z = rep(0, length(theta)))


circle_map <- circle_curve[idx, ]
dist_to_move_circle <- abs(half_L - y_shift)
int_map_circle <- data.frame(x = rep(x_shift_for_circle +2*radius, n_map), y = s[idx]-dist_to_move_circle, z = rep(0, n_map))

# Build mapping lines for circle
rows <- lapply(1:n_map, function(i) {
  list(int_map_circle[i, ], circle_map[i, ], data.frame(x = NA, y = NA, z = NA))
})

result_for_circle <- do.call(rbind, unlist(rows, recursive = FALSE))

v1 <- curve_map[1, ]
v2 <- curve_map[n_map, ]
e1 <- curve_map[ceiling(n_map/2), ]
e2 <- circle_map[ceiling(n_map/2), ]
zero1 <- int_map[1, ]
le1 <- int_map[n_map, ]

zero2 <- int_map_circle[1, ]
le2 <- int_map_circle[n_map, ]

vertex <- rbind(v1, v2, zero1, zero2, le1, le2)

p <- plot_ly() |>
  # smooth sin curve
  add_trace(data = curve_smooth,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "black"),
            showlegend = FALSE
  ) |>
  # sin curve
  add_trace(data = curve_map,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 3, color = "rgba(0,0,0,0)"),
            showlegend = FALSE
  ) |>
  # smooth circle curve
  add_trace(data = circle_curve,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d",
            mode = "lines",
            line = list(width = 7, color = "black"),
            showlegend = FALSE
  ) |>  
  add_trace(data = circle_map,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 3, color = "rgba(0,0,0,0)"),
            showlegend = FALSE
  ) |>
  # interval [0,L]
  add_trace(data = int_map,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "lines+markers",
            line = list(width = 7, color = "black"),
            marker = list(size = 3, color = "rgba(0,0,0,0)"),
            showlegend = FALSE
  ) |>
  # interval for circle
  add_trace(data = int_map_circle,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "lines+markers",
            line = list(width = 7, color = "black"),
            marker = list(size = 3, color = "rgba(0,0,0,0)"),
            showlegend = FALSE
  ) |>
  add_trace(data = vertex,
            x = ~x,
            y = ~y,
            z = ~z,
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 5, color = "black"),
            showlegend = FALSE
  ) |> add_trace(
    type = "cone",
    x = e1$x,
    y = e1$y,
    z = e1$z,
    u = 1,
    v = 0,
    w = 0,
    sizemode = "absolute",
    sizeref = 0.4,
    colorscale = list(c(0, 1), c("green", "green")),
    showscale = FALSE
  ) |> add_trace(
    type = "cone",
    x = e2$x,
    y = e2$y,
    z = e2$z,
    u = 0,
    v = 1,
    w = 0,
    sizemode = "absolute",
    sizeref = 0.4,
    colorscale = list(c(0, 1), c("green", "green")),
    showscale = FALSE
  )

pal <- colorRampPalette(c("royalblue", "cyan", "yellow", "red"))(n_map)


dx <- int_map$x - curve_map$x
dy <- int_map$y - curve_map$y
dz <- int_map$z - curve_map$z

dxc <- int_map_circle$x - circle_map$x
dyc <- int_map_circle$y - circle_map$y
dzc <- int_map_circle$z - circle_map$z


for(i in 1:n_map){
  
  seg <- rbind(
    int_map[i, ],
    curve_map[i, ]
  )
  
  seg2 <- rbind(
    int_map_circle[i, ],
    circle_map[i, ]
  )
  
  p <- p |> add_trace(
    data = seg,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = pal[i], width = 1),
    showlegend = FALSE
  ) |> add_trace(
    data = seg2,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(color = pal[i], width = 1),
    showlegend = FALSE
  ) |> add_trace(
    type = "cone",
    x = int_map$x[i],
    y = int_map$y[i],
    z = int_map$z[i],
    u = dx[i],
    v = dy[i],
    w = dz[i],
    sizemode = "absolute",
    sizeref = 0.2,
    anchor = "tip",
    colorscale = list(c(0, pal[i]), c(1, pal[i])),
    showscale = FALSE
  ) |> add_trace(
    type = "cone",
    x = int_map_circle$x[i],
    y = int_map_circle$y[i],
    z = int_map_circle$z[i],
    u = dxc[i],
    v = dyc[i],
    w = dzc[i],
    sizemode = "absolute",
    sizeref = 0.2,
    anchor = "tip",
    colorscale = list(c(0, pal[i]), c(1, pal[i])),
    showscale = FALSE
  )
}



p3 <- p |>
  config(mathjax = 'cdn') |> 
  layout(p,
         font = list(family = "Palatino"),
         margin = list(l = 0, r = 0, b = 0, t = 0),
         scene = list(xaxis = list(title = list(text = "x", font = list(color = colaxnn)),  tickfont = list(color = colaxnn),  range = c(0, x_shift_for_circle +2*radius)*1.01),
                      yaxis = list(title = list(text = "y", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = range(int_map_circle$y)),
                      zaxis = list(title = list(text = "z", font = list(color = colaxnn)),  tickfont = list(color = colaxnn), range = c(-1, 1)),
                      aspectratio = list(x = x_shift_for_circle +2*radius, y = L, z = L),
                      camera = list(eye = list(x = 0, y = -L*1.5, z = L*1.5),
                                    center = list(x = 0, y = 0, z = 0)),
                      annotations = list(
                        list(
                          x = zero1$x, y = zero1$y, z = zero1$z,
                          text = TeX("0"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = zero2$x, y = zero2$y, z = zero2$z,
                          text = TeX("0"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = le1$x, y = le1$y, z = le1$z,
                          text = TeX("\\ell_{e_1}"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = le2$x, y = le2$y, z = le2$z,
                          text = TeX("\\ell_{e_2}"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = v1$x, y = v1$y, z = v1$z,
                          text = TeX("v_1"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = v2$x, y = v2$y, z = v2$z,
                          text = TeX("v_2"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = e1$x, y = e1$y, z = e1$z,
                          text = TeX("e_1"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
                        list(
                          x = e2$x, y = e2$y, z = e2$z,
                          text = TeX("e_2"),
                          textangle = 0, ax = 0, ay = -35,
                          font = list(color = "black", size = gfsize),
                          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1))
         )
  )

save(p3, file = here::here("data_files/graphs6p3.Rdata"))
```


```
{r, eval =TRUE, fig.height = 7, out.width = "100%"}
load(here::here("data_files/graphs6p3.Rdata"))
p3
```





```{r}
library(plotly)
library(pracma)

Tt <- 2

# 1. Define the curve
phi <- seq(0, Tt, length.out = 100)
x_curve <- phi
y_curve <- rep(0, length(phi))
z_curve <- exp(phi)

# 2. Arc-length parametrization
arc_length_fun <- function(t) sqrt(1 + (exp(t))^2)
s_vals <- cumtrapz(phi, arc_length_fun(phi))  # cumulative arc length
ell <- max(s_vals)  # total arc length

# 3. Define uniform points along [0, ell]
n_points <- 19
s_uniform <- seq(0, ell, length.out = n_points)

# 4. Find corresponding phi values (invert arc-length mapping)
phi_from_s <- sapply(s_uniform, function(s_target) {
  # Find phi such that integral from 0 to phi of ds/dt equals s_target
  f <- function(t) cumtrapz(phi[phi <= t], arc_length_fun(phi[phi <= t])) - s_target
  # Use uniroot to solve for t
  # To simplify, use linear interpolation on precomputed s_vals
  approx(s_vals, phi, xout = s_target)$y
})

# 5. Corresponding points on the curve
x_curve_points <- phi_from_s
y_curve_points <- rep(0, n_points)
z_curve_points <- exp(phi_from_s)

# 6. Plot
fig <- plot_ly()

# Curve
fig <- fig %>% add_trace(x = x_curve, y = y_curve, z = z_curve,
                         type='scatter3d', mode='lines', line=list(width=4,color='blue'),
                         name='Curve')

# Uniform arc-length interval along y-axis
fig <- fig %>% add_trace(x = rep(0, n_points), y = s_uniform, z = rep(0, n_points),
                         type='scatter3d', mode='markers', marker=list(size=4,color='green'),
                         name='Interval [0, ell]')

# Lines from arc-length interval to curve
for (i in 1:n_points) {
  fig <- fig %>% add_trace(x = c(0, x_curve_points[i]),
                           y = c(s_uniform[i], 0),
                           z = c(0, z_curve_points[i]),
                           type='scatter3d', mode='lines',
                           line=list(dash='dot', color='green'), showlegend=FALSE)
}

# Interval [0,4] points (original phi values) along x-axis
fig <- fig %>% add_trace(x = x_curve_points, y = rep(0, n_points), z = z_curve_points,
                         type='scatter3d', mode='markers', marker=list(size=4,color='red'),
                         name='Interval [0,4]')

# Lines from phi-interval to curve
for (i in 1:n_points) {
  fig <- fig %>% add_trace(x = c(x_curve_points[i], x_curve_points[i]),
                           y = c(0,0),
                           z = c(0, z_curve_points[i]),
                           type='scatter3d', mode='lines',
                           line=list(dash='dash', color='red'), showlegend=FALSE)
}

# Layout
fig <- fig %>% layout(scene=list(
  xaxis=list(title='phi / x-axis'),
  yaxis=list(title='arc length s'),
  zaxis=list(title='z = exp(phi)'),
  aspectratio = list(x = Tt/4, y = ell/4, z = max(z_curve)/4)
))

fig
```


generate_graph_on_sphere <- function(n, steps = 200, step_size = 0.01) {
  # --- Random initial points on sphere ---
  X <- matrix(rnorm(3 * n), n, 3)
  X <- X / sqrt(rowSums(X^2))  # normalize to unit sphere
  
  # --- Repulsion (Coulomb-like optimization) ---
  for (s in 1:steps) {
    F <- matrix(0, n, 3)
    
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        diff <- X[i, ] - X[j, ]
        d <- sqrt(sum(diff^2)) + 1e-8
        
        force <- diff / d^3   # repulsive force
        
        F[i, ] <- F[i, ] + force
        F[j, ] <- F[j, ] - force
      }
    }
    
    # update + reproject to sphere
    X <- X + step_size * F
    X <- X / sqrt(rowSums(X^2))
  }
  
  nodes <- X
  nodes2d <- nodes[, 1:2]
  
  # --- Fully connected edges ---
  edges <- list()
  edges2d <- list()
  k <- 1
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      edges[[k]] <- rbind(nodes[i, ], nodes[j, ])
      edges2d[[k]] <- rbind(nodes2d[i, ], nodes2d[j, ])
      k <- k + 1
    }
  }
  
  return(list(
    nodes = nodes,
    edges = edges,
    edges2d = edges2d
  ))
}




# check this arrangement, may become in handy

create_data <- function(alpha, m){
  res <- rSPDE:::interp_rational_coefficients(
    order = m, 
    type_rational_approx = "brasil", 
    type_interp = "spline", 
    alpha = alpha)
  
  partFraction <- function(x) {
    term_matrix <- outer(x, res$p, "-") # matrix of (x - p[i])
    term_values <- sweep(1 / term_matrix, 2, res$r, "*") # multiply columns by r[i]
    return(rowSums(term_values) + res$k)
  }
  
  powerFun <- function(x){
    return(x^(-(alpha-floor(alpha))))
  }
  
  delta <- 10^(-(5+m)/2)
  
  x <- seq(1, 1/delta, length.out = 1000)
  pf <- partFraction(x)
  po <- powerFun(x)
  Linferror <- max(abs(partFraction(x) - powerFun(x)))
  df <- rbind(data.frame(x = x, val = pf, method = "partFraction", alpha = alpha, m = m, Linferror = Linferror),
              data.frame(x = x, val =  po, method = "powerFun", alpha = alpha, m = m, Linferror = Linferror))
  return(df)
}

alpha <- c(0.4, 1.6, 2.4)
m <- c(2,6,12)
delta_aux <- 10^(-(5+m)/2)
min_reciprocal_delta <- min(1/delta_aux)
min_m <- min(m)

df <- do.call(rbind, lapply(alpha, function(a) {
  do.call(rbind, lapply(m, function(mm) {
    create_data(a, mm)
  }))
}))


df <- df |>
  mutate(
    m = {
      m_vals <- sort(unique(m))
      factor(
        m,
        levels = m_vals,
        labels = sprintf("$m = %g$", m_vals))
    },
    alpha = {
      alpha_vals <- sort(unique(alpha))
      factor(
        alpha,
        levels = alpha_vals,
        labels = sprintf("$\\alpha = %g$", alpha_vals)
      )
    },
    method = factor(
      method,
      levels = c("partFraction", "powerFun")[2:1],
      labels = c("$r_m(x^{-1}) = kx^{-\\lfloor\\alpha\\rfloor}+x^{-\\lfloor\\alpha\\rfloor}\\sum_{i=1}^mr_i(x-p_i)^{-1}$", "$f(x^{-1}) = x^{-\\alpha}$")[2:1]
    )
  )


format_scientific <- function(x, digits = 2) {
  ifelse(
    x == 0,
    "0",
    {
      e <- floor(log10(abs(x)))
      m <- round(x / 10^e, digits)
      sprintf("%.2f \\times 10^{%d}", m, e)
    }
  )
}

df_lab <- df |>
  group_by(m, alpha) |>
  summarise(
    Linferror = first(Linferror),
    x_mid = mean(range(x, na.rm = TRUE)),
    y_mid = exp(mean(log(range(val, na.rm = TRUE)))),  # because of log scale
    .groups = "drop"
  )

p_RAT22 <- ggplot(df, aes(x, val, color = method, linetype = method)) + 
  facet_wrap(alpha~ m, scales = "free_x", nrow = length(unique(df$alpha)))+#facet_grid(alpha ~ m, scales = "free_x") +
  geom_line(linewidth = 2) +
  scale_y_log10(n.breaks = 5) +
  scale_x_log10(n.breaks = 5) +
  scale_colour_manual(name = "Function", values = c("$r_m(x^{-1}) = kx^{-\\lfloor\\alpha\\rfloor}+x^{-\\lfloor\\alpha\\rfloor}\\sum_{i=1}^mr_i(x-p_i)^{-1}$" = "red", "$f(x^{-1}) = x^{-\\alpha}$" = "blue")) + 
  scale_linetype_manual(name = "Function", values = c("$r_m(x^{-1}) = kx^{-\\lfloor\\alpha\\rfloor}+x^{-\\lfloor\\alpha\\rfloor}\\sum_{i=1}^mr_i(x-p_i)^{-1}$" = "dashed", "$f(x^{-1}) = x^{-\\alpha}$" = "solid")[2:1]) + 
  theme_bw() +
  theme(panel.spacing = unit(0.3, "cm"), 
        text = element_text(family = "Palatino"),
        plot.title = element_text(hjust = 0.5, size = 12),
        strip.text.x = element_text(size = 12),  # alpha (top)
        strip.text.y = element_text(size = 12),   # m (left)
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),  # x-axis numbers
        axis.text.y = element_text(size = 12),   # y-axis numbers
        legend.title = element_text(size = 12),
        #plot.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 12)) +
  labs(x = sprintf("$x\\in[1,1/\\delta_{%d}]\\quad(\\log_{10}\\mbox{ scale})$", min_m),
       y = "$(\\log_{10}\\mbox{ scale})$" # $r_m(x^{-1}),\\;f(x^{-1})$"
  ) + 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.width  = unit(2, "cm"),
    legend.key.height = unit(0, "cm")
  ) +
  geom_text(
    data = df_lab,
    aes(
      #label = sprintf("$\\|f-r_m\\|_{L_{\\infty}([1,1/\\delta_{%d}])} = %s$", min_m, format_scientific(Linferror))
      label = sprintf("$\\|f-r_m\\|_{L_{\\infty}([1,1/\\delta_m])} = %s$", format_scientific(Linferror))
    ),
    x = 0,
    y = -2.1,
    inherit.aes = FALSE,
    parse = FALSE,
    size = 3.5,
    hjust = 0,
    vjust = 0
  ) 



generate_graph_on_sphere <- function(n) {
  # --- Generate n points on the unit sphere (Fibonacci sphere) ---
  i <- 0:(n-1)
  phi <- pi * (3 - sqrt(5))  # golden angle
  
  z <- 1 - 2 * i / (n - 1)   # z from 1 to -1
  theta <- phi * i           # angle
  
  r <- sqrt(1 - z^2)
  x <- r * cos(theta)
  y <- r * sin(theta)
  
  nodes <- cbind(x, y, z)
  nodes2d <- cbind(x, y)
  
  # --- Build fully connected edge list ---
  edges <- list()
  edges2d <- list()
  k <- 1
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      edges[[k]] <- rbind(nodes[i, ], nodes[j, ])
      edges2d[[k]] <- rbind(nodes2d[i, ], nodes2d[j, ])
      k <- k + 1
    }
  }
  
  return(list(nodes = nodes,
              edges2d = edges2d,
              edges = edges))
}








