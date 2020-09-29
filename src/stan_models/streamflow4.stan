data {
//fitting data
int<lower=1> N;                   // sample size
vector<lower=0,upper=1>[N] y;     // response 

vector[N] x_i;                    // intercept
vector[N] x_a;                    // age
vector[N] x_g;                    // growing condition - opt:0 sub:1
vector[N] x_t;                    // tree type - euc:0 pin:1

}
transformed data {
vector[N] inter1;   
vector[N] inter2;                  // interaction
inter1 = x_a .* x_t;
inter2 = x_a .* x_g;
}
parameters {
vector[4] theta;                  // reg coefficients
real theta1_i;                     // interaction coeff
real theta2_i;                     // interaction coeff
real<lower=0> phi;                // dispersion parameter
}
transformed parameters{
vector[4] beta;
real beta1_i;
real beta2_i;

beta = theta * 10;               // "matt trick" if desired
beta1_i = theta1_i * 10;           // "matt trick" if desired
beta2_i = theta2_i * 10;           // "matt trick" if desired
}
model {
// model calculations
vector[N] Xbeta;                  // linear predictor
vector[N] mu;                     // transformed linear predictor
vector[N] A;             // parameter for beta distn
vector[N] B;             // parameter for beta distn

Xbeta = x_i * beta[1] + x_a * beta[2] + x_g * beta[3] + x_t * beta[4] + inter1 * beta1_i + inter2 * beta2_i;
for (i in 1:N) { 
mu[i] = inv_logit(Xbeta[i]);   
}

//refctor parmater to mean and dispersion
A = mu * phi;
B = (1.0 - mu) * phi;

// priors
theta ~ normal(0, 1); 
theta1_i ~ normal(0, 1);
theta2_i ~ normal(0, 1);
phi ~ cauchy(0, 5);               // different options for phi  
//phi ~ inv_gamma(.001, .001);
//phi ~ uniform(0, 500);          // put upper on phi if using this

// likelihood
y ~ beta(A, B); //fit streamflow reduction

}

generated quantities {
vector[N] log_lik;
vector[N] Xbetap;                  // linear predictor
vector[N] mup;                     // transformed linear predictor
vector[N] Ap;             // parameter for beta distn
vector[N] Bp;             // parameter for beta distn

Xbetap = x_i * beta[1] + x_a * beta[2] + x_g * beta[3] + x_t * beta[4] + inter1 * beta1_i + inter2 * beta2_i;
for (i in 1:N) { 
mup[i] = inv_logit(Xbetap[i]);   
Ap[i] = mup[i] * phi;
Bp[i] = (1.0 - mup[i]) * phi;
log_lik[i] = beta_lpdf(y[i] | Ap[i], Bp[i]);
}
}
