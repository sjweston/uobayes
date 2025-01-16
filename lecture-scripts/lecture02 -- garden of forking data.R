library(rethinking)


# 35:56 -- probability distribution ---------------------------------------

## here, we visualise the likelihood of a particular sample under 5 possible hypotheses. 
## the sample
sample <- c("W", "L", "W", "W", "W", "L", "W", "L", "W")

W <- sum(sample == "W") #number of W observed
L <- sum(sample == "L") #number of L observed

p <- c(0, .25, .5, .75, 1) #hypothetical proportions of W

ways <- sapply(p, function(q) (q*4)^W * ((1-q)*4)^L )
prob = ways/sum(ways)
cbind( p, ways, prob )

barplot(
  height = prob,
  names.arg = p,
  col = "black",
  xlab = "proportion water",
  ylab = "probability",
  ylim = c(0, max(prob) * 1.2) # Add some space above the bars
)


# 38:37 -- generative simulation ------------------------------------------

# function to toss a globe covered p by water N times
sim_globe = function( p=0.7 , N=9 ){
  sample(
    x = c("W", "L"),  # possible values
    size = N,         # how many draws
    prob = c(p, 1-p), # probability of each possibility
    replace = TRUE    # the same value can be drawn multiple times
  )
}

# try it
sim_globe()

# try it many times -- each column is a new try
replicate( sim_globe( p=0.5, N=9 ), n = 10)

# test on extreme settings
sim_globe( p = 1 )  # no land
sum(sim_globe(N=1e4) == "W")/1e4

# 41:42 -- code the estimator ---------------------------------------------

# function to compute posterior distribution
compute_posterior = function( the_sample, poss = c(0, .25, .50, .75, 1) ){
  
  W = sum(the_sample == "W") # number of W observed
  L = sum(the_sample == "L") # number of L observed
  ways = sapply(
    poss, 
    function(q) (q*4)^W * ((1-q)*4)^L
  )
  post = ways/sum(ways)
  bars = sapply(post, function(q) make_bar(q)) # this is from the rethinking package
  data.frame(poss, ways, post = round(post, 3), bars)
}

# do it a couple times
compute_posterior(sim_globe())
compute_posterior(sim_globe())
compute_posterior(sample)


# 1:05:53 -- sampling the posterior ---------------------------------------

post_samples <- rbeta(1000, 6+1, 3+1)
dens( post_samples, lwd=4)