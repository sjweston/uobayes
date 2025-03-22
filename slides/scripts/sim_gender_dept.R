# generative model, basic mediator scenario
set.seed(0319)
N <- 1000 # number of applicants
# even gender distribution
G <- sample( 1:2, size=N, replace=TRUE )
# gender 1 tends to apply to department 1, 2 to 2
D <- rbinom( n=N, size=1, prob=ifelse( G==1 , 0.3 , 0.8 ) ) + 1
# matrix of acceptance rates
accept_rate <- matrix( c(0.5, 0.2, 0.1, 0.3), nrow=2)
# simulate acceptance
A <- rbinom( n=N, size=1, accept_rate[D,G])
