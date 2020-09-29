data {
//fitting data
int<lower=1> M;                   // predict size
int<lower=1> N;                   // sample size
vector<lower=0,upper=1>[N] y;     // response 

vector[N] x_i;                    // intercept
vector[N] x_a;                    // age
vector[N] x_g;                    // growing condition - opt:0 sub:1
vector[N] x_t;                    // tree type - euc:0 pin:1

//predict
vector[M] x_ip;                    // intercept
vector[M] x_ap;                    // age
vector[M] x_gp;                    // growing condition - opt:0 sub:1
vector[M] x_tp;                    // tree type - euc:0 pin:1

}
transformed data {
vector[N] inter;                  // interaction
vector[M] interp;                  // interaction
inter = x_a .* x_g;
interp = x_ap .* x_gp;
}
parameters {
vector[4] theta;                  // reg coefficients
real theta_i;                     // interaction coeff
real<lower=0> phi;                // dispersion parameter
}
transformed parameters{
vector[4] beta;
real beta_i;

beta = theta * 10;               // "matt trick" if desired
beta_i = theta_i * 10;           // "matt trick" if desired
}
model {
// model calculations
vector[N] Xbeta;                  // linear predictor
vector[N] mu;                     // transformed linear predictor
vector[N] A;             // parameter for beta distn
vector[N] B;             // parameter for beta distn

Xbeta = x_i * beta[1] + x_a * beta[2] + x_g * beta[3] + x_t * beta[4] + inter * beta_i;
for (i in 1:N) { 
mu[i] = inv_logit(Xbeta[i]);   
}

//refctor parmater to mean and dispersion
A = mu * phi;
B = (1.0 - mu) * phi;

// priors
theta ~ normal(0, 1); 
theta_i ~ normal(0, 1);
phi ~ cauchy(0, 5);               // different options for phi  
//phi ~ inv_gamma(.001, .001);
//phi ~ uniform(0, 500);          // put upper on phi if using this

// likelihood
y ~ beta(A, B); //fit streamflow reduction

}

generated quantities {
vector[M] predy;
vector[M] Xbetap;                  // linear predictor
vector[M] mup;                     // transformed linear predictor
vector[M] Ap;             // parameter for beta distn
vector[M] Bp;             // parameter for beta distn


//parameters for new sites
for (i in 1:M) {
  Xbetap[i] = x_ip[i] * beta[1] + x_ap[i] * beta[2] + x_gp[i] * beta[3] + x_tp[i] * beta[4] + interp[i] * beta_i;
  mup[i] = inv_logit(Xbetap[i]);   
}

//refctor parmater to mean and dispersion
Ap = mup * phi;
Bp = (1.0 - mup) * phi;

//predict
for (i in 1:M) {
   predy[i] = beta_rng( max([0.00001,Ap[i]]), max([0.00001,Bp[i]]));
}
}
