library(ggplot2)


capture.output(
  knitr::purl(here::here("functionality1.Rmd"), output = here::here("functionality1.R")),
  file = here::here("old/purl_log.txt")
)
source(here::here("functionality1.R"))


alpha <- 1.6
m <- 10


res <- rSPDE:::interp_rational_coefficients(
  order = m, 
  type_rational_approx = "brasil", 
  type_interp = "spline", 
  alpha = alpha)

pol <- gsignal::rresidue(
  res$r, 
  res$p, 
  res$k, 
  tol = 0.00001)

partFraction <- function(x) {
  term_matrix <- outer(x, res$p, "-") # matrix of (x - p[i])
  term_values <- sweep(1 / term_matrix, 2, res$r, "*") # multiply columns by r[i]
  return(rowSums(term_values) + res$k)
}

pOverq <- function(x) {
  up <- sapply(x, function(xx) sum(pol$b * xx^(rev(seq_along(pol$b))-1)))
  down <- sapply(x, function(xx) sum(pol$a * xx^(rev(seq_along(pol$a))-1)))
  return(up / down)
}
powerFun <- function(x){
  return(x^(-(alpha-floor(alpha))))
}

delta <- 10^(-(5+m)/2)
x <- seq(1, 1/delta, length.out = 100)
#x <- 1/x
df <- data.frame(x = x, pq = pOverq(x), pf = partFraction(x), po = powerFun(x))

p <- ggplot(df, aes(x = x)) +
  geom_line(aes(y = pq, color = "p/q"), size = 1) +
  #geom_point(aes(y = pq, color = "p/q"), size = 1.5) +
  geom_line(aes(y = pf, color = "partialfraction p/q"), size = 1, linetype = "dashed") +
  #geom_point(aes(y = pf, color = "partialfraction p/q"), size = 1.5) +
  geom_line(aes(y = po, color = "x^{-alpha}"), size = 1, linetype = "dotted") +
  #geom_point(aes(y = po, color = "x^{-alpha}"), size = 1.5) +
  labs(
    title = "",
    x = "x",
    y = "y",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

plotly::ggplotly(p)

max(abs(partFraction(x) - powerFun(x)))  # Check if the two polynomials are equal
