Bernoulli <- function(data, p = 0.5, rng = "mt19937_64") {
  gf <- GF(BernoulliSample, p = p, rng = rng)
  Filter(data, gf)
}
