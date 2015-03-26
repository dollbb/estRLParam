estimate RL params
============

functions for simulating a reinforcement learning model in a restless bandit task and estimating its parameters with a variety of methods. 

both R code and matlab code simulate the same task and estimate the
generative parameters with Maximum Likelihood, or Maximum A Posteriori
(see Rcode/fitData.R or matlab/recoverParams.m for details). R code
additionally can estimate with Markov Chain Monte Carlo (requires
[stan](http://mc-stan.org/)).  R code also fits lag-1 single and
multilevel logistic regression models.

for details:
* R: see Rcode/fitData.R
* matlab: see matlab/recoverParams.m
