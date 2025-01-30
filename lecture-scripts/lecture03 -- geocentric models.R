library(rethinking)


# 29:26 -- height and weight ----------------------------------------------

# function to simulate height and weight of individuals

sim_weight <- function(H,b,sd){
  U <- rnorm( length(H), 0, sd )
  W <- b*H + U
  return(W)
}

H <- runif(200, min=130, max=170)
W <- sim_weight(H, b=.5, sd=5)
plot(W ~ H, col = 2, lwd = 3)


# 48:13 -- quadratic approximation ----------------------------------------

d <- Howell1
d <- d[d$age >= 18, ]

m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*H,
    a ~ dnorm(0, 10),
    b ~ dunif(0, 1),
    sigma ~ dunif(0,10)
    ), data = list( W=W, H=H)
  )



# 55:24 -- prior predictive distribution ----------------------------------

n <- 1e3
a <- rnorm(n, 0,10)
b <- runif(n, 0,1)
plot(NULL, xlim = c(130,170), ylim = c(50,90) ,
     xlab = "height", ylab = "weight")
for(j in 1:50) abline( a = a[j], b = b[j], lwd = 2, col = "#1C5253")


# 1:01:42 -- test model ---------------------------------------------------


# simulate sample of 10 people
set.seed(93)
H <- runif( 3, 130, 170)
W <- sim_weight( H=H, b=.7, sd=5)

# run the model

m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*H,
    a ~ dnorm(0, 10),
    b ~ dunif(0, 1),
    sigma ~ dunif(0,10)
  ), data = list( W=W, H=H)
)
# does this recover our known parameters? b = .5 and sigma = 5?
precis(m3.1)


# 1:04:18 -- use on real data ---------------------------------------------

dat <- list(W = d$weight, H = d$height)
m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma),
    mu <- a + b*H,
    a ~ dnorm(0, 10),
    b ~ dunif(0, 1),
    sigma ~ dunif(0,10)
  ), data = dat
)
precis(m3.1)

sample_post = extract.samples(m3.1)

plot(
  d$height, d$weight, col = "#1C5253", lwd = 3, xlab = "height", ylab = "weight"
)
for(j in 1:20) abline(a = sample_post$a[j], b = sample_post$b[j])
