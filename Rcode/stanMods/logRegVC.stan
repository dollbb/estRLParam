data {
	int<lower=1> N; //number obs
	int NS; // number subs
	int<lower=1, upper=NS> subId[N];
	vector[2] zero;

	int stay[N];
	real prevRew[N];
	
}

parameters{
	real intercept;
	real betaPrevRew;
	vector[2] varySub [NS]; // by sub raneff
	cov_matrix[2] sigmaSub; // cov mat for ran int and slope 			   
}

model {
        real vary[N];
	real glm[N];

	intercept ~ normal(0,100); 
	betaPrevRew ~ normal(0,100); 

	for (j in 1:NS) varySub[j] ~ multi_normal(zero, sigmaSub);
	
	for (i in 1:N) {
	    vary[i] <- varySub[subId[i],1] + varySub[subId[i],2] * prevRew[i];
	    glm[i] <- vary[i] + intercept + betaPrevRew * prevRew[i];
	}

	stay ~ bernoulli_logit(glm);

}