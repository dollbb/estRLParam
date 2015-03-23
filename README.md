estimate RL params
============

functions for simulating and estimating model parameters in a reinforcement learning task with a variety of methods.

both R code and matlab code simulate the same task and estimate the
generative parameters with Maximum Likelihood, or Maximum A Posteriori
(see Rcode/fitData.R or matlab/recoverParams.m for details). Rcode/
additionally can estimate with Markov Chain Monte Carlo (requires [stan](http://mc-stan.org/)).
