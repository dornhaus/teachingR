## CODE TO GO WITH BOOKCLUB
## ON Richard McElreath's 'Statistical Rethinking'

########### PART Va #####################
# LIBRARIES  -----------------------------
library(rethinking)
library(scales)
library(viridis)
library(dagitty) # for drawing DAGs
#dev.off()
# Resetting graphics parameters ------------------
par(mar=c(5,5,1,1), oma=c(0,0,0,0), mfrow=c(1,1)) #b,l,t,r
colorscale <- viridis(100)
# OVERALL TOPIC: MULTIPLE REGRESSION ---------------------------

## CHAPTER 5 & VIDEO 4 already talk about this, but
## here I am joining with 
## CHAPTER 6 and VIDEO 5 material, which introduces 
## Confounds, e.g. multicollinearity, post-treatment bias, 
## and collider bias (selection-distortion effect)

# Drawing the owl: 
# 1 Define what we want: the effect of M on D in this example
# 2 Scientific model: the DAGs, i.e. alternative hypotheses,
#          are M<-A->D,
#          or M->D, M<-A->D
# 3 Statistical models representing different hypotheses fit to data
# 4 Simulating and testing that the model works
# 5 a: Comparing the fitted parameters for different models: 
#    which model variations change the results? 
#   b: "do(M)", calculating the difference between D with M=0 and M=1,
#      or some other artificially determined Ms, keeping the coefficients
#      and distributions of other variables the same as originally fitted
#      by the model to the data. 

# Note: here he is very much arguing for a regular hypothesis-testing
# scientific method approach. 
# - You have two (or more) real hypotheses, from the research context. 
#      In terms of numbered steps below: 1 is defining the question, 
#      2 are the conceptual hypotheses, 3 is translating the conceptual hypotheses
#      into a statistical, i.e. quantitative, model
# - You think about their implications (before data).
#      4 is just logistics for 3 and 5, but 5 is making the actual predictions;
#      predictions are always counterfactual (or at least all but one of them)
# - You derive a unique measure (the conditional independence here) that
#      the hypotheses make contradictory predictions about. 
#      Still part of step 5!
# - Then you statistically estimate which of the two predictions the 
#      estimated measure is more like. 
#      In the first example, essentially we are saying that if a factor is causal,
#      the model should not change the corresponding parameter based on whether
#      something else is included.
#      In the second example, we fit a full model with all possible associations,
#      and then simulate 'COUNTERFACTUAL' data in which we artificially break 
#      the association between A and M by using artificial A or artificial M.
#      Then we use the parameter fits from the full model to determine D - 
#      if D still depends on A or M even when these are manipulated, then the 
#      model has determined that they are causal in the original data (i.e. the
#      parameter solution calculated is such that this factor affects D. 

# Also, all linear models are 'linear' because they use an
# additive combination of parameters multiplied by respective variables.
# The parameters are independent and simply added (weighted by variables).

# Drawing DAGs ---------------------------
# Here is where we really want to draw DAGs:
# Directed acyclic graphs
dag5.1 <- dagitty("dag {
A -> D
A -> M
M -> D
}")
coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) )
drawdag(dag5.1)

# The package can not only draw DAGs, but
# find the conditional independencies...
DMA_dag2 <- dagitty('dag{ D <- A -> M }')
impliedConditionalIndependencies(DMA_dag2)
# This means all the DAGs with an indistinguishable structure.

# OK - FIRST DOING MULTIPLE REGRESSION AT ALL -------------------------
# - load actual data
# - code generative model, i.e. simdata
# - bivariate and multivariate models for comparison
# - plotting uncertainty around parameters
# - plotting residuals
# - posterior prediction plots, i.e. predicted against observed

## Loading and simulating data --------------------------
# From CHAPTER 5
# Load data and copy
data(WaffleDivorce)

# Actual data 
d <- WaffleDivorce
# Standardize variables
dat <- data.frame(matrix(,nrow=nrow(d),ncol=0))
dat$A <- scale(d$MedianAgeMarriage)
dat$D <- scale(d$Divorce)
dat$M <- scale(d$Marriage)
dat$Loc <- d$Loc

# Simulate data with different relationships
N <- 50 # number of simulated States
age <- rnorm(N) # sim A
mar <- rnorm(N, -age) # sim A -> M 
div <- rnorm(N, age) # sim A -> D
# or:
# div <- rnorm(N, age + mar) # sim A->D<-M
simdata <- data.frame(A=age, D=div, M=mar)

# define whether we want real or simulated data
# (just run one of these)
d <- simdata 
d <- dat

# HYPOTHESES = three models -------------------------
# We want to know whether M affects D even independently of 
# the fact that A is (or may be) a common cause of M and D.

# Model on median age at marriage across US States
# and divorce rates (code direct from book chapter 5)
# THREE MODELS:
# one each for the relationship of A on D and M on D, and then
# a multivariate model with both A and M affecting (potentially) D.

# First, bivariate model just of A and D
m_justA <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) , data = d)
# PRIORS -----------------
# We'll also initially fiddle with the prior a bit, 
# so here a way to plot the implied relationships of the priors
prior <- extract.prior(m_justA)
# link() gives some expected mu from the model given the 'post=' data
mu <- link(m_justA, post=prior, data=list(A=c(-2,2)))
# Plot the relationships implied by priors
par(mar=c(5,5,1,1), oma=c(0,0,0,0), mfrow=c(1,1)) #b,l,t,r
plot(NULL 
     , xlim=c(-2,2) , ylim=c(-2,2)
     , xlab="A [std]", ylab="D [std]"
     )
for ( i in 1:50 ) 
  lines( c(-2,2), mu[i,], col=col.alpha("black",0.4))
# Since all are standardized, we expect the intercept a to be 0.

# Compute percentile interval of mean
A_seq <- seq(from=-2.5, to=2.5, length.out=30)
# Calculate some predicted mu for each A, this time from posterior
mu <- link(m_justA, data=list(A=A_seq))
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
# Plot it all:
# - Data
plot(D ~ A , data=d , col=rangi2 )
# - Expected mu for each A
lines(A_seq, mu.mean, lwd=2)
# - Percentile interval of mu from posterior
shade(mu.PI, A_seq)

# Now the other two models:
m_justM <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )
m_multipleReg <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d)
precis(m_multipleReg)
#par(mar=c(3,0,0.5,0.5), oma=c(1,0,0,0.5)) #b,l,t,r
plot(coeftab(m_multipleReg, m_justM, m_justA), par=c("bA","bM", "sigma", "a") )
# Return graphics to normal:
par(mar=c(5,5,1,1), oma=c(0,0,0,0)) #b,l,t,r

# His point here is that looking at which coefficients change when other
# parameters are included tells you something about whether these 
# are 'confounded'. E.g. the effect of M disappears when A is included. 

# RESIDUALS -------------------

# Now, to understand/illustrate this, we might also want to plot 
# effects against residuals of A or M. By that we mean we first
# regress M on A, then calculate the difference between predicted and actual M.
m_MonA_for_residuals <- quap(
  alist(
    M ~ dnorm( mu , sigma ) ,
    mu <- a + bAM * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bAM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )
mu <- link(m_MonA_for_residuals)
mu_mean <- apply(mu, 2, mean)
mu_residMonA <- d$M - mu_mean

m_AonM_for_residuals <- quap(
  alist(
    A ~ dnorm( mu , sigma ) ,
    mu <- a + bMA * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bMA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

mu <- link(m_AonM_for_residuals)
mu_mean <- apply(mu, 2, mean)
mu_residAonM <- d$A - mu_mean

# Plotting the residuals against the response to illustrate
# better the effects found in the multivariate regression model.
# Note that McElreath strongly argues against using the residuals 
# themselves as data. This is because they are parameters
# estimated from the (first) model, and have uncertainty associated
# with them. 
# If there are multiple factors, they should all be included in the first
# model with an appropriate DAG. 
plot(D~mu_residMonA
     , data = d
     , ylab = "Divorce rate [std]"
     , xlab = "Marriage rate residuals on A"
     , col = rangi2
)
abline(lm(d$D~mu_residMonA))
offset <- 0.15
text(mu_residMonA+offset, d$D+offset, labels=d$Loc, cex= 0.5)

plot(D~mu_residAonM
     , data = d
     , ylab = "Divorce rate [std]"
     , xlab = "Age at marriage residuals on M"
     , col = rangi2
)
abline(lm(d$D~mu_residAonM))
text(mu_residAonM+offset, d$D+offset, labels=d$Loc, cex= 0.5)

# The residuals though contain essentially the same information as
# the posterior parameter estimates, just better illustrated.
# They don't prevent the confound problems from colliders etc.

# POSTERIOR PREDICTION PLOTS -----------------

# We simulated data completely artificially to check that our 
# programmed model works in detecting an effect we know is there,
# or shows that there is no effect when we know there is none.

# McElreath suggests another level of checking, which is to do plots
# of predicted data derived from the estimated posterior.
# Essentially this would be useful if we actually wanted predictions/
# forecasts; but it is also useful to see in what ways they are similar
# to the actual data used to derive the posterior (whether simulated or
# real). 
# 
# Ways in which they are not similar show the ways the model 
# fails. 

mu <- link(m_multipleReg)
# Reminder: link() generates samples from the posterior from the given model. 
# Automatic parameter is setting is to have n=1000: 1000 samples
# There are 50 columns because there are 50 original data points; for
# each data point('s x-value), a mu is estimated 1000 times. 

mu_mean <- apply(mu, 2, mean)
# Reminder: the arguments for apply() are X = data, 
# MARGIN = 1 for row or 2 for column, and FUN = function to be applied

mu_PI <- apply(mu, 2, PI)
# Also remember that for PI he always gives the 5%-94% interval, to use
# 89% as the range instead of 90% or 95%, just to be contrary.

# Plotting original data again
plot(D~A
     , data = d
     , col = rangi2)
# Then the mus on top
points(mu_mean~d$A
       , pch = 19
       , cex = 0.3)
# Here we see the resulting means (mu) for each data point from the original
# dataset. 

# Now we will simulate 'predictions' from the same model
D_sim <- sim(m_multipleReg, n=1e4 )
D_PI <- apply(D_sim, 2, PI)

# Now plotting these predictions (mus) over the actual data
# (since they are in the same order and for same x values)
plot(mu_mean ~ d$D
     , col=rangi2 
     , ylim=range(mu_PI)
     , xlab="Observed divorce"
     , ylab="Predicted divorce")
# A perfect fit would look like this:
abline(a=0, b=1, lty=2)
# Code for plotting error bars on points!
for (i in 1:nrow(d)) 
  lines(rep(d$D[i],2), mu_PI[,i], col=rangi2)
# For just labeling some points: (although did not work well for my graphics device)
# identify( x=d$D , y=mu_mean , labels=d$Loc )
# For labeling all points:
text(d$D+offset, mu_mean+offset, labels=d$Loc, cex= 0.5)

# Box as a reminder that models don't answer 'large world' questions.
# Then he also says 'true hypotheses will pass and fail many statistical 
# "tests" on their way to acceptance'. This is most certainly correct, since
# what he certainly means is that many 'tests' will fail to detect or reject something,
# either because of error or bad design (lack of match to 'large world'), 
# and 'acceptance' in the scientific community should be a higher bar than
# a single paper or statistical test. 
# However, the careless way he says it makes it seem as if statistical 
# 'tests' were irrelevant to such acceptance, which of course they should not
# be, and aren't. 


## CONFOUNDS ----------------------------

# From VIDEO 5

# 4 types of confounds:
# The fork  X<-Z->Y
# The pipe X->Z->Y
# The collider X->Z<-Y
# The descendant X->Z->Y, Z->A

# Multiple of these can and do occur in the same DAG or 
# causal system; the names are about the specific problem
# when trying to determine the effect of X on Y.

# In the fork, it's good to stratify by Z; in the pipe and the
# collider, it is BAD to stratify by Z. 

## THE FORK X<-Z->Y
# or SPURIOUS ASSOCIATIONS
# X and Y associated because they share a common cause.
# X is a 'spurious effect' (Z is the real cause of Y).
# Solution: once we stratify by Z, there is no
# association between X and Y

# In the marriage data example, A is Z (real effect), 
# M is X (spurious effect), 
# and D is Y.

# How to stratify? 
# Version 1: My script #6 deals with categorical variables,
# and if the common cause Z is categorical, we can simply
# calculate our estimands separately for each level of Z,
# e.g. expected weight, or the weight-height relationship, for
# each sex.

# Version 2: If Z is a continuous variable, we need to 
# do essentially the same for an infinite number of Zs.
# We do this by *simulation*. I.e. we fit a model for the 
# system, then generate predicted observations that keep Z constant
# (or calculate the effect of X on Y for each Z).

N <- 50 # number of cases
x_real <- rnorm(N) # x_real as Gaussian with mean 0 and stddev 1
x_spur <- rnorm(N, x_real) # x_spur as Gaussian with mean=x_real
y <- rnorm(N, x_real) # y as Gaussian with mean=x_real
d <- data.frame(y,x_real,x_spur) # bind all together in data frame
# Plot everything against everything
pairs(d)
# Everything correlates, but actually
dag_spuriousX <- dagitty( "dag {
x_real -> x_spur
x_real -> y
}")
coordinates(dag_spuriousX) <- list( x=c(x_real=0,x_spur=1,y=2) , y=c(x_real=0,x_spur=1,y=0))
drawdag(dag_spuriousX)
# This is the fork!

# Multiple regression should detect this in the same way we treated 
# Age at marriage and Marriage rate in the last example - although
# he doesn't do this with p-values but just looks at what predictive power is 
# added by each variable in the model.

# What about conventional linear models?
Regular_MultiVar_Model <- lm(y~x_real+x_spur)
summary(Regular_MultiVar_Model)
# We get the same (correct) conclusion but with p-values, pretty significant
# ones at that (for N=50 or 100).
# For an N=10, we often don't get a significant result.

# BUT, he would prefer if we answer these questions about which 
# link the effect comes from via simulations, i.e. 


#  COUNTERFACTUAL PLOTS ------------------------------

# Or 'simulating an intervention'.
# The idea is to determine (fix) the values of one variable, 
# thus breaking their link to anything that normally causes it.
# Then looking at this variable's effect on the response. 
# He calls this 'p(D|do(M))' - we 'do', i.e. make up, M, 
# and then calculate the distribution of D.

# Choose one
d <- simdata 
d <- dat

## THE FORK AGAIN Z<-X->Y or X<-Z->Y --------------------------------------
#
# Here, in video 5, when talking about the 'fork', he suggests 
# a counterfactual comparison of the regular multivariate model but
# for a fixed M = 0 and then for a fixed M = 1, and then analyzing
# the contrasts (i.e. mean of differences, not difference of means!).

# In the book, he went right to re-creating the model 
# but including all three causal effects of the DAG
# This is like the 'full luxury Bayes' he talked about in video 4,
# (although with confounds we are now on video 5). 
m_DMA_full <- quap(
  alist(
    ## A -> D <- M
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1),
    ## A -> M
    M ~ dnorm(mu_M, sigma_M),
    mu_M <- aM + bAM*A,
    aM ~ dnorm(0, 0.2),
    bAM ~ dnorm(0, 0.5),
    sigma_M ~ dexp(1)
  ) , data = d)

# Generating a counterfactual, i.e. 'manipulated' set of A
# data
A_seq <- seq(from=-2, to=2, length.out=30)
# Simulating the effect first on M, then D
sim_dat <- data.frame(A=A_seq)
s <- sim(m_DMA_full, data=sim_dat, vars=c("M","D"))
# Display counterfactual predictions
par(mar=c(5,5,1,1), oma=c(0,0,0,0)) #b,l,t,r
plot(sim_dat$A , colMeans(s$D) , ylim=c(-2,2) , type="l" ,
      xlab="manipulated A" , ylab="counterfactual D" )
shade( apply(s$D,2,PI) , sim_dat$A )
mtext( "Total counterfactual effect of A on D" )

# The point of this is that if the effect of A on D is spurious,
# i.e. generated through some joint association with another variable,
# then manipulating A does nothing to change D. And vice versa.

# So for the real marriage data, we find that A does affect D, even breaking
# any association with M. 

# Now let's try the same with M:
M_seq <- seq(from=-2, to=2, length.out=30)
# Simulating the effect first on A, then D
sim_dat <- data.frame(M=M_seq)
s <- sim(m_DMA_full, data=sim_dat, vars=c("A","D"))
# Display counterfactual predictions
par(mar=c(5,5,1,1), oma=c(0,0,0,0)) #b,l,t,r
plot(sim_dat$M, colMeans(s$D) , ylim=c(-2,2) , type="l" ,
     xlab="manipulated M" , ylab="counterfactual D" )
shade(apply(s$D,2,PI), sim_dat$M)
mtext("Total counterfactual effect of M on D")

# Doing this for the real marriage data with M shows no (strong)
# effect of M on D when breaking the association of M with A.


## THE PIPE X->Z->Y --------------------------------------
# or 'Mediator' variables (Z in this case)
# POST-TREATMENT BIAS

# Here, if we stratify by Z, we lose all effect of X.
# Essentially what we want to do, if we know Z is a mediator,
# is to leave it out of the model. 
# Including the mediator in a controlled experiment is called
# post-treatment bias: do not include consequences of the treatment
# in the estimator (other than the focus response).

# Post-treatment bias can also occur when some other variable that
# is a consequence of the treatment is not a mediator but
# something else, eg. in a collider situation, or when 
# T -> Z,  A->Z and A->Y. We want to know if T affects Y,
# and it does not, but conditioning on Z may make it seem like it does.

N <- 50 # number of cases
x_realcause <- rnorm(N) # Gaussian with mean 0 and stddev 1
z_mediator <- rnorm(N, x_realcause) # Gaussian with mean=x_real
y <- rnorm(N, z_mediator) # y as Gaussian with mean=x_real
d <- data.frame(y,x_realcause,z_mediator) # bind all together in data frame
# Plot everything against everything
pairs(d)
# Everything correlates, but actually
dag_mediator <- dagitty( "dag {
x_realcause -> z_mediator
z_mediator -> y
}")
coordinates(dag_mediator) <- list( x=c(x_realcause=0,z_mediator=1,y=2) , y=c(x_realcause=0,z_mediator=1,y=0))
drawdag(dag_mediator)
# This is the pipe!
par(mar=c(5,5,1,1), oma=c(0,0,0,0)) #b,l,t,r

# Either the Bayesian or the regular regression method misidentify
# Z as the 'real' cause of Y.
Regular_MultiVar_Model <- lm(y~x_realcause+z_mediator)
summary(Regular_MultiVar_Model)

# Note that in the example for 'fork' above, when our interest is in 
# the effect (or not) of M, the problem was a 'fork'; when our
# interest is in the effect (or not) of A, the problem was a 
# 'pipe'!

# And the counterfactual strategy of modeling the full system and checking
# for the strength of the effect of either A or M on D when manipulating
# them worked - we found that A was the 'real' cause in either case,
# and not M. 
# So the counterfactual simulations detected the pipe correctly
# when the regular regression method did not. 


# THE COLLIDER X->Z<-Y ------------------------------
# If you stratify by Z, you can end up having a strong
# association between X and Y. 
# Might be interpreted as a fork! The point is that any connection
# between data is read in the same way by the model, whichever
# direction the arrow goes.

# Can also happen outside of your analysis itself from collider-
# based selection on the data (selection-distortion effect).
# Effectively that selection pre-stratifies data based on a collider
# (e.g. the sum of trustworthiness and newsworthiness).

# Example from primate milk from book
data(milk)
d <- milk
d$K <- scale( d$kcal.per.g )
d$F <- scale( d$perc.fat )
d$L <- scale( d$perc.lactose )

# We're interested in the energy content of milk (K), and whether it is
# predicted by fat content (F). 
# kcal.per.g regressed on perc.fat
m6.3 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bF*F,
    a ~ dnorm(0, 0.2),
    bF ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) , data=d)
# kcal.per.g regressed on perc.lactose
m6.4 <- quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu <- a + bL*L,
    a ~ dnorm(0, 0.2),
    bL ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) , data=d)
precis(m6.3)
precis(m6.4)
postF <- extract.samples(m6.3)
postL <- extract.samples(m6.4)
dens(postL$bL, col=rangi2)
dens(postF$bF, col=rangi2)
# Individually, each factor (L and F) explain a lot of the 
# energy content of milk.

# But, all the factors are also correlated
pairs(~d$L + d$F + d$K)
# And if we include both F and L in the model, *both* of their
# relationships with K are much reduced:
m6.5 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bF*F + bL*L ,
    a ~ dnorm( 0 , 0.2 ) ,
    bF ~ dnorm( 0 , 0.5 ) ,
    bL ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) ,
  data=d )
precis(m6.5)

# Quote:
# In general, there’s no guarantee that the available data contain much information about
# a parameter of interest. When that’s true, your Bayesian machine will return a posterior
# distribution very similar to the prior. Comparing the posterior to the prior can therefore
# be a good idea, a way of seeing how much information the model extracted from the data.

# Illustration of how the standard deviation of this parameter 
# (in the milk dataset specifically) goes up
# with the degree of correlation between the two colliders (factors)
d <- milk
sim.coll <- function( r=0.9 ) {
  d$x <- rnorm( nrow(d) , mean=r*d$perc.fat ,
                sd=sqrt( (1-r^2)*var(d$perc.fat) ) )
  m <- lm( kcal.per.g ~ perc.fat + x , data=d )
  sqrt( diag( vcov(m) ) )[2] # stddev of parameter
}
rep.sim.coll <- function( r=0.9 , n=100 ) {
  stddev <- replicate( n , sim.coll(r) )
  mean(stddev)
}
r.seq <- seq(from=0,to=0.99,by=0.01)
stddev <- sapply( r.seq , function(z) rep.sim.coll(r=z,n=100) )
plot( stddev ~ r.seq , type="l" , col=rangi2, lwd=2 , xlab="correlation" )

# This problem is actually similar to the one that in the book
# comes under the heading:

# MULTICOLLINEARITY -----------------------------
N <- 100 # number of individuals
set.seed(909)
height <- rnorm(N,10,2) # sim total height of each
leg_prop <- runif(N,0.4,0.5) # leg as proportion of height
leg_left <- leg_prop*height + rnorm(N , 0 , 0.02 )
# sim left leg as proportion + error
leg_right <- leg_prop*height + rnorm(N , 0 , 0.02 )
# sim right leg as proportion + error
# combine into data frame
d <- data.frame(height,leg_left,leg_right)

plot(d$leg_left ~ d$height
     , col = col.alpha("black", 0.8)
     , pch = 1)
points(d$leg_right~d$height
       , col= col.alpha(rangi2, 0.4)
       , pch = 19)
mean(d$leg_left - d$leg_right)
pairs(~d$leg_left + d$leg_right + d$height)

m6.1 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + bl*leg_left + br*leg_right ,
    a ~ dnorm( 10 , 100 ) ,
    bl ~ dnorm( 2 , 10 ) ,
    br ~ dnorm( 2 , 10 ) ,
    sigma ~ dexp( 1 )
  ) ,
  data=d )
precis(m6.1)
plot(precis(m6.1))
# Unusually high variance in the bx estimates, given how closely leg length
# tracks height. Here we can see what happens:
post <- extract.samples(m6.1)
plot( bl ~ br , post , col=col.alpha(rangi2,0.1) , pch=16 )
# The two slopes are closely associated: if we predict height from
# one leg (high slope value) then the other leg becomes irrelevant
# (low slope value). This is because either one contains all the information; 
# both slopes need not be high (and cannot, as that would lead to a higher
# total slope of average leg length with height).

# The posterior distribution (of all parameters!) reveals this though: 
# the range of either parameter given the other is narrow, it just looks
# wide when averaging over all values of the other parameter(s).

# The sum of bl and br reveals the 'correct' association:
dens(post$br + post$bl, col=rangi2)

# The problem here is in particular that we might have looked at the original
# br and bl distributions and concluded that they both overlap zero, and 
# thus that neither of them is a good predictor. 
# Key insight we should have taken from this though is that they ALSO
# overlap with pretty high values of slope.


# The DESCENDANT X->Z->Y, Z->A -------------------------------

# The descendant (A) is just another factor that follows from any
# of the others (doesn't have to be a pipe as in this example).
# The point is that any A, if included in the model, has a similar
# effect as including Z in a model. So if Z is a collider or a
# pipe, you should also not include any A in the model (of
# effect of X on Y).

# Descendants are everywhere: we often actually measure proxies
# of something we care about. 

#  MASKED RELATIONSHIPS -----------------------------------

# Primate milk
data(milk)
d <- milk
str(d)
d$K <- scale(d$kcal.per.g) # Energy in milk
d$N <- scale(d$neocortex.perc) # Encephalization
d$M <- scale(log(d$mass)) # Body mass

# It turns out some of the N vector entries are NAs. 
# The modeling approach used before does not work with that, 
# so we redefine our dataset as just the complete cases from the original
# table. 
dcc <- d[complete.cases(d$K,d$N,d$M), ]

milkmodel_draft <- quap(
  alist(
    K ~ dnorm(mu , sigma ),
    mu <- a + bN*N,
    a ~ dnorm(0, 1),
    bN ~ dnorm(0, 1),
    sigma ~ dexp(1)
    ) 
  , data=dcc 
  )

# Now consider priors. Are they reasonable? Let's plot some
prior <- extract.prior(milkmodel_draft)
xseq <- c(-2,2)
mu <- link(milkmodel_draft, post=prior, data=list(N=xseq))
plot(NULL, 
     xlim=xseq, ylim=xseq, 
     xlab="N: Encephalization [std]", ylab="K: Milk energy content [std]",
     main="Priors: a~dnorm(0,1), bN~dnorm(0,1)"
     )
for ( i in 1:50 ) 
  lines(xseq, mu[i,] 
        , col=col.alpha("black",0.3) 
        )

# These priors are all over the place. First, given the standardized
# data, if the data are even vaguely normal, the resulting relationship
# should give a 0 intercept, i.e. at the x=0 (z-score), the y z-score
# should also be (about) 0.
# Second, for vaguely normally distributed data, values more than 
# 2 standard deviations out (2 or -2 z-score) should be pretty rare.
hist(dcc$N, xlim = c(-3,3))
hist(dcc$K, xlim = c(-3,3))
# (and they are)

# So, improved priors:
milkmodel <- quap(
  alist(
    K ~ dnorm(mu , sigma ),
    mu <- a + bN*N,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) 
  , data=dcc 
)

# Let's plot again:
prior <- extract.prior(milkmodel)
xseq <- c(-2,2)
mu <- link(milkmodel, post=prior, data=list(N=xseq))
plot(NULL, 
     xlim=xseq, ylim=xseq, 
     xlab="N: Encephalization [std]", ylab="K: Milk energy content [std]",
     main="Priors: a~dnorm(0,0.2), bN~dnorm(0,0.5)"
)
for ( i in 1:50 ) 
  lines(xseq, mu[i,] 
        , col=col.alpha("black",0.3) 
  )

# What does the model say:
precis(milkmodel)
# But: "Tables of numbers are golem speak, and we are not
# golems."
# Illustrate result instead:
xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link(milkmodel, data=list(N=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc, col=rangi2)
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

# New model, only using body mass
milkmodel2 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=dcc )
precis(milkmodel2)
xseq <- seq(from=min(dcc$M)-0.15, to=max(dcc$M)+0.15, length.out=30)
mu <- link(milkmodel2, data=list(M=xseq))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot(K ~ M, data=dcc, col=rangi2)
lines(xseq, mu_mean, lwd=2)
shade(mu_PI, xseq)

# Ok now the multivariate model
milkmodel_full <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N + bM*M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=dcc )
precis(milkmodel_full)
plot(coeftab(milkmodel_full, milkmodel2, milkmodel) , pars=c("bM","bN"))
# The outcome plot of the estimated parameters shows both estimated 
# slopes (relationships) got stronger in the combined model, 
# meaning they were masking each other previously.
pairs(~K + M + N, dcc)

Regular_MultiVar_Model <- lm(K~N+M, data=dcc)
summary(Regular_MultiVar_Model)
# Same conclusion qualitatively, although estimated slopes are 
# even higher here.

# Several DAGs can produce the three-way relationship between 
# K, M, and N here. In the book he claims these are impossible to distinguish
# from the data alone, although actually we did exactly that with the 
# spurious X and the marriage data examples above. 
# Perhaps the difference is that here, neither M nor N are 'spurious':
# they do both have actual effects even independent of their correlation
# with each other. 

###  Back to COUNTERFACTUAL PLOTS ---------------------------------

# Here it is helpful to use a counterfactual plot, not to replace
# actual experiments, but to illustrate how the model sees the effects
# of each parameter independently.
xseq <- seq(from=min(dcc$M)-0.15, to=max(dcc$M)+0.15, length.out=30)
mu <- link(milkmodel_full, data=data.frame(M=xseq, N=0))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot(NULL, xlim=range(dcc$M), ylim=range(dcc$K)
     , xlab="M: Body mass [std]", ylab="K: Milk energy content [std]"
     , main="Counterfactual for N=0"
     )
lines(xseq, mu_mean, lwd=2)
shade(mu_PI, xseq)

xseq <- seq(from=min(dcc$N)-0.15, to=max(dcc$N)+0.15, length.out=30)
mu <- link(milkmodel_full, data=data.frame(N=xseq, M=0))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot(NULL, xlim=range(dcc$N), ylim=range(dcc$K)
     , xlab="N: Encephalization [std]", ylab="K: Milk energy content [std]"
     , main="Counterfactual for M=0"
)
lines(xseq, mu_mean, lwd=2)
shade(mu_PI, xseq)

# The dagitty package can draw all the DAGs that produce
# equivalent dependency patterns
dag5.7 <- dagitty( "dag{
M -> K <- N
M -> N }" )
coordinates(dag5.7) <- list( x=c(M=0,K=1,N=2) , y=c(M=0.5,K=1,N=0.5) )
MElist <- equivalentDAGs(dag5.7)
drawdag(MElist)

# Can we, or can we not, gain information about which DAG
# is correct from the data?

###  SIMULATION!  (to find out if we can - the answer turns out to be 'no') ----------------

# M -> K <- N
# M -> N
n <- 100
M <- rnorm(n )
N <- rnorm(n, M)
K <- rnorm(n, N - M)
d_simMtoN <- data.frame(K=K,N=N,M=M)
# N -> M
n <- 100
N <- rnorm(n)
M <- rnorm(n, N)
K <- rnorm(n, N - M)
d_simNtoM <- data.frame(K=K,N=N,M=M)

milkmodel_simMtoN <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N + bM*M ,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) , data=d_simMtoN)
precis(milkmodel_simMtoN)
milkmodel_simNtoM <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N + bM*M ,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) , data=d_simNtoM)
precis(milkmodel_simNtoM)
# Now all models for comparison:
d <- d_simMtoN
m_1 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )
m_2 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data=d )
m_both <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N + bM*M ,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ) , data=d)
dev.off()
plot(coeftab(m_both, m_1, m_2) , pars=c("bM","bN"))

Regular_MultiVar_Model <- lm(K~N+M, data=d)
summary(Regular_MultiVar_Model)

# Either way, M->N and N->M are indistinguishable!

# The only thing we can detect is whether either N or M
# do not have any effect on the response, and are only 
# correlated with it by virtue of a joint cause of another 
# variable.




