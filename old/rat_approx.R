library(ggplot2)
alpha <- 0.6
m <- 10
delta <- 10^(-(5+m)/2)
# to check if the rational approximation is correct
res <- rSPDE:::interp_rational_coefficients(order = m, 
                                            type_rational_approx = "brasil", 
                                            type_interp = "spline", 
                                            alpha = alpha)
res
pol <- gsignal::rresidue(res$r, res$p, res$k, tol = 0.00001)
pol

g_x <- function(x) {
  term_matrix <- outer(x, res$p, "-") # matrix of (x - p[i])
  term_values <- sweep(1 / term_matrix, 2, res$r, "*") # multiply columns by r[i]
  return(rowSums(term_values) + res$k)
}

f_x <- function(x) {
  up <- sapply(x, function(xx) sum(pol$b * xx^(rev(seq_along(pol$b))-1)))
  down <- sapply(x, function(xx) sum(pol$a * xx^(rev(seq_along(pol$a))-1)))
  up / down
}

h_x <- function(x){
  x^(-(alpha-floor(alpha)))
}

x <- seq(delta, 1, length.out = 100)
x <- 1/x
df <- data.frame(x = x, f = f_x(x), g = g_x(x), h = h_x(x))

p <- ggplot(df, aes(x = x)) +
  geom_line(aes(y = f, color = "p/q"), size = 1) +
  #geom_point(aes(y = f, color = "p/q"), size = 1.5) +
  geom_line(aes(y = h, color = "x^{-alpha}"), size = 1, linetype = "dotted") +
  #geom_point(aes(y = h, color = "x^{-alpha}"), size = 1.5) +
  geom_line(aes(y = g, color = "partialfraction p/q"), size = 1, linetype = "dashed") +
  #geom_point(aes(y = g, color = "partialfraction p/q"), size = 1.5) +
  labs(
    title = "Polynomials Comparison",
    x = "x",
    y = "f(x) and g(x)",
    color = "Function"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
plotly::ggplotly(p)

max(abs(g_x(x) - h_x(x)))  # Check if the two polynomials are equal
