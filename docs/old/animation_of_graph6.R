library(plotly)
library(pracma)

n_smooth <- 250
TT <- 3*pi
t <- seq(0, TT, length.out = n_smooth)

speed <- sqrt(1 + cos(t)^2)
s <- pracma::cumtrapz(t, speed)
L <- max(s)

half_L <- L / 2
half_TT <- TT / 2
dist_to_move <- abs(half_L - half_TT)

y_shift <- 4

# smooth curve
x <- t + dist_to_move
y <- sin(t) + y_shift
z <- rep(0,length(t))

curve_smooth <- data.frame(x = x, y = y, z = z)

# interval mapping
n_map <- 50
idx <- seq(1, length(t), length.out = n_map)

curve_map <- data.frame(x = x[idx], y = y[idx], z = z[idx])
int_map <- data.frame(x = s[idx], y = rep(0, n_map), z = 0)

# circle
s_circ <- L
radius <- s_circ / (2*pi)
x_shift_for_circle <- radius + TT + dist_to_move

y_up_range <- max(exp(s/4))

theta <- seq(from=-pi,to=pi,length.out = n_smooth)

circle_curve <- data.frame(
  x = radius*cos(theta) + x_shift_for_circle,
  y = radius*sin(theta) + y_shift,
  z = 0
)

circle_map <- circle_curve[idx, ]
dist_to_move_circle <- abs(half_L - y_shift)

int_map_circle <- data.frame(
  x = rep(x_shift_for_circle +2*radius, n_map),
  y = s[idx]-dist_to_move_circle,
  z = 0
)

# ---- vertices (STATIC)
v1 <- curve_map[1, ]
v2 <- curve_map[n_map, ]
e1 <- curve_map[ceiling(n_map/2), ]
e2 <- circle_map[ceiling(n_map/2), ]
zero1 <- int_map[1, ]
le1 <- int_map[n_map, ]
zero2 <- int_map_circle[1, ]
le2 <- int_map_circle[n_map, ]

vertex <- rbind(v1, v2, zero1, zero2, le1, le2)

# ---- animation parameter
n_frames <- 40
lambda_vals <- seq(0,1,length.out = n_frames)

frames_curves <- list()
frames_connectors <- list()

for(k in seq_along(lambda_vals)){
  
  lambda <- lambda_vals[k]
  
  # moving blue curves
  x_interp <- (1-lambda)*curve_map$x + lambda*int_map$x
  y_interp <- (1-lambda)*curve_map$y + lambda*int_map$y
  
  x_interp_c <- (1-lambda)*circle_map$x + lambda*int_map_circle$x
  y_interp_c <- (1-lambda)*circle_map$y + lambda*int_map_circle$y
  
  df_curve <- rbind(
    data.frame(x = x_interp, y = y_interp, z = 0, frame = k, group = "sin"),
    data.frame(x = x_interp_c, y = y_interp_c, z = 0, frame = k, group = "circle")
  )
  
  frames_curves[[k]] <- df_curve
  
  # ---- connectors (gray)
  conn_list <- list()
  
  for(i in 1:n_map){
    
    # sine connector
    seg1 <- data.frame(
      x = c(curve_map$x[i], x_interp[i], NA),
      y = c(curve_map$y[i], y_interp[i], NA),
      z = c(0, 0, NA),
      frame = k
    )
    
    # circle connector
    seg2 <- data.frame(
      x = c(circle_map$x[i], x_interp_c[i], NA),
      y = c(circle_map$y[i], y_interp_c[i], NA),
      z = c(0, 0, NA),
      frame = k
    )
    
    conn_list[[2*i-1]] <- seg1
    conn_list[[2*i]]   <- seg2
  }
  
  frames_connectors[[k]] <- do.call(rbind, conn_list)
}

df_all <- do.call(rbind, frames_curves)
df_conn <- do.call(rbind, frames_connectors)

# ---- plot
p <- plot_ly()

# 🔴 static smooth curves
p <- p |>
  add_trace(
    data = rbind(
      curve_smooth,
      data.frame(x = NA, y = NA, z = NA),
      circle_curve
    ),
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "lines",
    line = list(width = 6, color = "red"),
    showlegend = FALSE
  )

# ⚪ animated connectors
p <- p |>
  add_trace(
    data = df_conn,
    x = ~x, y = ~y, z = ~z,
    frame = ~frame,
    type = "scatter3d",
    mode = "lines",
    line = list(color = "gray", width = 2),
    showlegend = FALSE
  )

# 🔵 animated curves
p <- p |>
  add_trace(
    data = df_all,
    x = ~x, y = ~y, z = ~z,
    frame = ~frame,
    split = ~group,
    type = "scatter3d",
    mode = "lines",
    line = list(width = 8, color = "blue"),
    showlegend = FALSE
  )

# ⚫ vertices
p <- p |>
  add_trace(
    data = vertex,
    x = ~x, y = ~y, z = ~z,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 5, color = "black"),
    showlegend = FALSE
  )

# ---- layout
rv <- 2
p <- p |>
  config(mathjax = 'cdn') |> 
  layout(
    font = list(family = "Palatino"),
    margin = list(l = 0, r = 0, b = 0, t = 0),
    scene = list(
      xaxis = list(
        title = list(text = "x", font = list(color = "gray", size = 16)),
        tickfont = list(color = "gray", size = 13),
        range = c(0, x_shift_for_circle +2*radius)*1.01
      ),
      yaxis = list(
        title = list(text = "y", font = list(color = "gray", size = 16)),
        tickfont = list(color = "gray", size = 13),
        range = range(int_map_circle$y)
      ),
      zaxis = list(
        title = list(text = "z", font = list(color = "gray", size = 16)),
        tickfont = list(color = "gray", size = 13),
        range = c(0, y_up_range+1)
      ),
      aspectratio = list(x = x_shift_for_circle +2*radius, y = L, z = 4),
      camera = list(
        eye = list(x = 7*rv, y = -15*rv, z = 6*rv),
        center = list(x = 0, y = 0, z = 0)
      ),
      annotations = list(
        list(
          x = zero1$x, y = zero1$y, z = zero1$z,
          text = TeX("0"),
          textangle = 0, ax = 0, ay = -35,
          font = list(color = "black", size = 16),
          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
        list(
          x = zero2$x, y = zero2$y, z = zero2$z,
          text = TeX("0"),
          textangle = 0, ax = 0, ay = -35,
          font = list(color = "black", size = 16),
          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
        list(
          x = le1$x, y = le1$y, z = le1$z,
          text = TeX("\\ell_{e_1}"),
          textangle = 0, ax = 0, ay = -35,
          font = list(color = "black", size = 16),
          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
        list(
          x = le2$x, y = le2$y, z = le2$z,
          text = TeX("\\ell_{e_2}"),
          textangle = 0, ax = 0, ay = -35,
          font = list(color = "black", size = 16),
          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
        list(
          x = v1$x, y = v1$y, z = v1$z,
          text = TeX("v_1"),
          textangle = 0, ax = 0, ay = -35,
          font = list(color = "black", size = 16),
          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
        list(
          x = v2$x, y = v2$y, z = v2$z,
          text = TeX("v_2"),
          textangle = 0, ax = 0, ay = -35,
          font = list(color = "black", size = 16),
          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
        list(
          x = e1$x, y = e1$y, z = e1$z,
          text = TeX("e_1"),
          textangle = 0, ax = 0, ay = -35,
          font = list(color = "black", size = 16),
          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1),
        list(
          x = e2$x, y = e2$y, z = e2$z,
          text = TeX("e_2"),
          textangle = 0, ax = 0, ay = -35,
          font = list(color = "black", size = 16),
          arrowcolor = "gray", arrowsize = 1, arrowwidth = 0.5, arrowhead = 1))
    )
  )

# ---- animation
p <- p |> animation_opts(
  frame = 60,
  transition = 0,
  redraw = TRUE
)

p