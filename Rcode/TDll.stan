data {
	int NS;
	int MT;	
	int NT[NS];
	int ch[NS,MT];	
	int r[NS,MT];
}

parameters {

   	real<lower=0,upper=1> am;
	real<lower=0> as;

	real bm;
	real<lower=0> bs;

	real<lower=0,upper=1> alpha[NS];
	real itemp[NS];	

}

transformed parameters {
	real aa;
	real ab;

	aa <- am * pow(as,-2);
	ab <- pow(as,-2) - aa;
}

model {
      bm ~ normal(0,100);
      bs ~ cauchy(0,2.5);

      for (s in 1:NS) {
      	  real q[2];
	  for (i in 1:2) {q[i] <- 0;}

	  alpha[s] ~ beta(aa, ab);
	  itemp[s] ~ normal(bm, bs);
	  
		for (t in 1:NT[s]) {
	      	    ch[s,t] ~ bernoulli_logit(itemp[s] * (q[2] - q[1]) );
		     q[ch[s,t]+1] <- q[ch[s,t]+1] + alpha[s] * ( r[s,t] - q[ch[s,t]+1] );

		    }		    
      }

}

generated quantities {
	  matrix[NS, MT] logLik; // will have to deal with the end trl after
	  
	  for (s in 1:NS) {
	      real q[2];
	      for (i in 1:2) {q[i] <- 0;}	      

	      for (t in 1:NT[s]) {
	       	  logLik[s,t] <- binomial_log(ch[s,t], 1,
	      	                 inv_logit(itemp[s] * (q[2]-q[1])));
	          q[ch[s,t]+1] <- q[ch[s,t]+1] + alpha[s] * ( r[s,t] - q[ch[s,t]+1] );	  
		  
	      }
	  }

}
