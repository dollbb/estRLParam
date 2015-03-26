estimate RL params
============

Functions for simulating a reinforcement learning model in a restless bandit task and estimating its parameters with a variety of methods. 

Both R code and Matlab code simulate the same task and estimate the
generative parameters with Maximum Likelihood, or Maximum A Posteriori. R code
additionally can estimate with Markov Chain Monte Carlo (requires
[stan](http://mc-stan.org/)).  R code also fits lag-1 single and
multilevel logistic regression models.

for details:
* R: see Rcode/fitData.R
* Matlab: see matlab/recoverParams.m
