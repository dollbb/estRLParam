data {
	int NS;
	int MT;	
	int NT[NS];
	int ch[NS,MT];	
}

parameters {
	real bm;
	real<lower=0> bs;
	real itemp[NS];	

}


model {
      bm ~ normal(0,100);
      bs ~ cauchy(0,2.5);

      for (s in 1:NS) {

	  itemp[s] ~ normal(bm, bs);
	  
		for (t in 1:NT[s]) {
	      	    ch[s,t] ~ bernoulli_logit(itemp[s]);

		    }		    
      }

}

generated quantities {
	  matrix[NS,MT] logLik;

	  for (s in 1:NS) {
	      real q[2];
	      for (i in 1:2) {q[i] <- 0;}
 	      for (t in 1:NT[s]) {
	       logLik[s,t] <- binomial_log(ch[s,t],1,inv_logit(itemp[s]));
	       }
	  }

}