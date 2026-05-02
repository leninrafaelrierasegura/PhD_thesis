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

# interval
n_map <- 50
idx <- seq(1, length(t), length.out = n_map)

curve_map <- data.frame(x = x[idx], y = y[idx], z = z[idx])
int_map <- data.frame(x = s[idx], y = rep(0, n_map), z = rep(0, n_map))

# circle
s_circ <- L
radius <- s_circ / (2*pi)
x_shift_for_circle <- radius + TT + dist_to_move

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

# ---- animation parameter
n_frames <- 40
lambda_vals <- seq(0,1,length.out = n_frames)

frames_data <- list()

for(k in seq_along(lambda_vals)){
  
  lambda <- lambda_vals[k]
  
  # interpolate sin curve -> interval
  x_interp <- (1-lambda)*curve_map$x + lambda*int_map$x
  y_interp <- (1-lambda)*curve_map$y + lambda*int_map$y
  
  # interpolate circle -> vertical interval
  x_interp_c <- (1-lambda)*circle_map$x + lambda*int_map_circle$x
  y_interp_c <- (1-lambda)*circle_map$y + lambda*int_map_circle$y
  
  df1 <- data.frame(x = x_interp, y = y_interp, z = 0,
                    frame = k, group = "sin")
  
  df2 <- data.frame(x = x_interp_c, y = y_interp_c, z = 0,
                    frame = k, group = "circle")
  
  frames_data[[k]] <- rbind(df1, df2)
}

df_all <- do.call(rbind, frames_data)

# ---- base smooth curves (fade out)
curve_all <- do.call(rbind, lapply(1:n_frames, function(k) {
  
  lambda <- lambda_vals[k]
  
  rbind(
    data.frame(
      x = curve_smooth$x,
      y = curve_smooth$y,
      z = curve_smooth$z,
      frame = k,
      opacity = 1 - lambda
    ),
    data.frame(
      x = circle_curve$x,
      y = circle_curve$y,
      z = circle_curve$z,
      frame = k,
      opacity = 1 - lambda
    )
  )
}))

# ---- plot
p <- plot_ly()

# moving curves
p <- p |>
  add_trace(
    data = df_all,
    x = ~x, y = ~y, z = ~z,
    frame = ~frame,
    split = ~group,
    type = "scatter3d",
    mode = "lines",
    line = list(width = 8, color = "#0000C8"),
    showlegend = FALSE
  )

# original smooth curves (fade effect)
p <- p |>
  add_trace(
    data = curve_all,
    x = ~x, y = ~y, z = ~z,
    frame = ~frame,
    type = "scatter3d",
    mode = "lines",
    line = list(width = 6, color = "darkred"),
    opacity = ~opacity,
    inherit = FALSE,
    showlegend = FALSE
  )

# layout


p <- p |> animation_opts(
  frame = 60,
  transition = 0,
  redraw = TRUE
)




p