library(rethinking)


# 35:56 -- probability distribution ---------------------------------------

## here, we visualize the likelihood of a particular sample under 5 possible hypotheses. 
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
dens( post_samples, lwd=4, col="#1C525380", xlab = "proportion water", adj = .1)
curve( dbeta(x, 6+1, 3+1), add = T, lty = 2, lwd = 3)


# 1:11:46 -- sampling for prediction --------------------------------------

# simulate posterior PREDICTIVE distribution
post_samples <- rbeta(1000, 6+1, 3+1) # posterior distribution for p
pred_post <- sapply(post_samples, 
                    function(p) sum( sim_globe(p, 10) == "W") )
tab_post = table(pred_post)
as.data.frame(tab_post) %>% 
  ggplot(aes(x = pred_post, y = Freq)) +
  geom_segment(aes(x = pred_post, xend = pred_post,
                   y = 0, yend = Freq),
               linewidth = 3,
               color = "#1c5253") +
  labs(x = "Number of W's",
       y = "Count") +
  theme_minimal()


# 1:24:00 -- bonus misclassification --------------------------------------

sim_globe2 = function( p=0.7 , N=9, x=0.1 ){
  true_sample <- sample(x=c("W", "L"), size=N, prob=c(p, 1-p), replace=TRUE)
  obs_sample <- ifelse( runif(N) < x, 
                        ifelse( true_sample == "W", "L", "W"), #error
                        true_sample) # no error
  return(obs_sample)
}
