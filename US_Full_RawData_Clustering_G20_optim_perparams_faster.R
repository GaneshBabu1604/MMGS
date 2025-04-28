require(MixMatrix)
require(mclust)
require(psych)
require(ggplot2)
require(reshape2)
require(Matrix)
require(sys)
require(Rfast)
require(Rcpp)
require(RcppArmadillo)
sourceCpp('dmatnorm_faster.cpp')

sine_estimate_old <- function(t, a, b, d, e, f){
  #(a*t+b) * sin((2 * pi / c) *t+d) + (e*f^t)
  (a*t+b) * sin((2 * pi / 12) *t+d) + (e+f*t)
}

sine_fit_leastsquares_old <- function(pars, y, z_g){
  a = pars[1]
  b = pars[2]
  #c = pars[3]
  d = pars[3]
  e = pars[4]
  f = pars[5]
  r = ncol(y)
  diff = 0
  estimated_y <- sine_estimate_old(c(1:r), a, b, d, e, f)
  res_temp <- sweep(y,2,estimated_y,'-')
  res <- (res_temp)^2
  for(n in 1:nrow(y)){
    diff = diff + sum(res[n,]*z_g[n])
  }
  #print(diff)
  return(diff)
}

sine_fit_complete_log_likelihood_old <- function(pars, G, var_num, par_num, current_par_val, z, z_prob, y, mu, sigma_inv, psi_inv, logdetsigma, logdetpsi){
  p <- dim(y)[1]
  r <- dim(y)[2]
  N <- dim(y)[3]
  g = G
  z_new = z_prob
  if(par_num == 1){
	a = pars[1]
  	b = current_par_val[var_num,2,g]
  	d = current_par_val[var_num,3,g]
  	e = current_par_val[var_num,4,g]
  	f = current_par_val[var_num,5,g]	
  }
  else if(par_num == 2){
	a = current_par_val[var_num,1,g]
  	b = pars[1]
  	d = current_par_val[var_num,3,g]
  	e = current_par_val[var_num,4,g]
  	f = current_par_val[var_num,5,g]  
  }
  else if(par_num == 3){
	a = current_par_val[var_num,1,g]
  	b = current_par_val[var_num,2,g]
  	d = pars[1]
  	e = current_par_val[var_num,4,g]
  	f = current_par_val[var_num,5,g] 
  }
  else if(par_num == 4){
        a = current_par_val[var_num,1,g]
  	b = current_par_val[var_num,2,g]
  	d = current_par_val[var_num,3,g]
  	e = pars[1]
  	f = current_par_val[var_num,5,g] 
  }
  else{
        a = current_par_val[var_num,1,g]
  	b = current_par_val[var_num,2,g]
  	d = current_par_val[var_num,3,g]
  	e = current_par_val[var_num,4,g]
  	f = pars[1] 
  }
  mu[var_num,,g] = sine_estimate_old(c(1:r), a, b, d, e, f)
  tau_g = sum(z[,g])/N
  z_new[,g] = (c(log(tau_g)) + c(dmatnorm_calc(y,mean = mu[,,g],U_inv = sigma_inv[,,g], V_inv = psi_inv[,,g],p = p, r = r, N = N,logdetU = logdetsigma[,g], logdetV = logdetpsi[,g])))    
  for(gg in 1:ncol(z)){
    z_new[,gg] = z[,gg]*z_new[,gg]
  }
  #z_max = apply(z_new,1,max)
  #z_sum = log(apply(exp(z_new-z_max),1,sum)) + z_max
  log_likelihood = sum(z_new)
  #log_likelihood = sum(z_sum)
  print(c(pars[1],log_likelihood))
  return(-log_likelihood)
}

BIC_Calculator_AR1_Sine_old<-function(log_likelihood,G = 2, N = 100, p = 2, r = 2){
  n_params = (G-1)+(G*p*5)+(G)+(G*((p*(p+1))/2))
  #print(n_params)
  BIC = -2*log_likelihood + n_params*log(N)
  return(BIC)
}

my_matrix_GMM_AR1_SineSeasonality_final_old <- function(X, convergence = 1e-5, epoch = 1000, z_start = 3, init_values = NULL, G = 2, pass_loc){
  N = dim(X)[3]
  p = dim(X)[1]
  r = dim(X)[2]
  power_values = c()
  for(i in c(0:(r-1))){
    if(i == 0){
      power_values = c(power_values,c(0:(r-1)))
    }
    else if(i == (r-1)){
      power_values = c(power_values,c((r-1):0))
    }
    else{
      power_values = c(power_values,c(i:0,(1:(r-(i+1)))))
    }
  }
  power_mult = matrix(power_values,r,r)
  tau = matrix((1/G), ncol = G,nrow = 1)
  mu = array(0,dim = c(p,r,G))
  phi = array(0, dim = c(r,r,G))
  phi_inverse = array(0,dim = c(r,r,G))
  sigma = array(0, dim = c(p,p,G))
  sigma_inverse = array(0, dim = c(p,p,G))
  A1 = Matrix(1,nrow = r,ncol = r,sparse = TRUE)
  sub_matrix <- A1[2:r,1:(r-1)]
  sub_matrix[lower.tri(sub_matrix)] = 0
  A1[2:r,1:(r-1)] = sub_matrix
  sub_matrix <- A1[1:(r-1),2:r]
  sub_matrix[upper.tri(sub_matrix)] = 0
  A1[1:(r-1),2:r] = sub_matrix
  diag(A1) = 0
  A2 = Matrix(0,ncol=r,nrow=r,sparse = TRUE)
  diag(A2) = 1
  A2[1,1]<-A2[r,r] <- 0
  I = Matrix(0,ncol=r,nrow = r,sparse = TRUE)
  diag(I) = 1
  z = matrix(0,nrow = N, ncol = G)
  logdetsigma = matrix(0,nrow = 1,ncol = G)
  logdetphi = matrix(0,nrow = 1,ncol = G)
  rho_k = matrix(0,nrow = 1,ncol = G)
  sig_k = matrix(0,nrow = 1,ncol = G)
  z_hat = matrix(0,nrow = N, ncol = G)
  colnames(z_hat) = c(1:G)
  log_likelihood = c()
  init_list = NULL
  if(z_start==1){
    if(length(init_values)==dim(X)[3]){
      z_init = unmap(init_values)
      mu_temp = array(0,dim = c(p,r,G))
      init_pars = array(0,dim = c(p,5,G))
      init_pars_nls = array(0,dim = c(p,5,G))
      for(g in c(1:G)){
        mu_temp[,,g] = apply(sweep(X,MARGIN = 3,z_init[,g],FUN = '*'),MARGIN = c(1,2),sum)/sum(z_init[,g])
        for(d in c(1:p)){
          temp = mu_temp[d,,g]
          W <- nls(temp ~ sine_estimate_old(c(1:r), a, b, d, e, f), start = list(a = 1,b = 5, d = 2, e = 7, f = 1),control = list(minFactor = 1e-10,maxiter = 2000))
          init_pars_nls[d,,g] <- coef(W)
          temp_data = t(test_data[d,,])
          W <- optim(par = init_pars_nls[d,,g], fn = sine_fit_leastsquares_old, gr = NULL, y = temp_data,z_g = z_init[,g])
          init_pars[d,,g] <- W$par
          mu[d,,g] = sine_estimate_old(c(1:r),init_pars[d,1,g],init_pars[d,2,g],init_pars[d,3,g],init_pars[d,4,g],init_pars[d,5,g])
        }
        phi[,,g] = diag(r)%*%diag(r)
        sigma[,,g] = diag(p)%*%diag(p)
        tau[,g] = length(which(init_values==g))/N
        phi_inverse[,,g] = solve(phi[,,g])
      }
      init_list = list(centers = mu,U = sigma, V = phi, tau = tau)
    }
    else{
      print('Incorrect length of initial values')
    }
  }
  else if(z_start == 2){
    Vector_X = data.frame()
    for(i in c(1:N)){
      Vector_X = rbind(Vector_X,c(X[,,i]))
    }
    set.seed(123456)
    init_values = kmeans(Vector_X,G,nstart = 5)$cluster
    z_init = unmap(init_values)
    mu_temp = array(0,dim = c(p,r,G))
    init_pars = array(0,dim = c(p,5,G))
    init_pars_nls = array(1,dim = c(p,5,G))
    for(g in c(1:G)){
      mu_temp[,,g] = apply(sweep(X,MARGIN = 3,z_init[,g],FUN = '*'),MARGIN = c(1,2),sum)/sum(z_init[,g])
      for(d in c(1:p)){
        temp = mu_temp[d,,g]
        tryCatch({
          print(c(d,g))
          W <- nls(temp ~ sine_estimate_old(c(1:r), a, b, d, e, f), start = list(a = 1,b = 1, d = 1, e = 1, f = 1),control = list(minFactor = 1e-20,maxiter = 2000))
          init_pars_nls[d,,g] <- coef(W)},error = function(e){
            print(e)
          })
        temp_data = t(X[d,,])
        print(c(d,g))
        print(init_pars_nls[d,,g])
        W <- optim(par = init_pars_nls[d,,g], fn = sine_fit_leastsquares_old, gr = NULL, y = temp_data,z_g = z_init[,g])
        init_pars[d,,g] <- W$par
        mu[d,,g] = sine_estimate_old(c(1:r),init_pars[d,1,g],init_pars[d,2,g],init_pars[d,3,g],init_pars[d,4,g],init_pars[d,5,g])
      }
      phi[,,g] = diag(r)%*%diag(r)
      sigma[,,g] = diag(p)%*%diag(p)
      tau[,g] = length(which(init_values==g))/N
      logdetsigma[,g] = sum(log(eigen(sigma[,,g])$values))
      logdetphi[,g] = sum(log(eigen(phi[,,g])$values))
      sigma_inverse[,,g] = solve(sigma[,,g])
      phi_inverse[,,g] = solve(phi[,,g])
    }
    init_list = list(centers = mu,U = sigma, V = phi,tau = tau, init_sine_pars = init_pars)
  }
  else{
    Vector_X = data.frame()
    for(i in c(1:N)){
      Vector_X = rbind(Vector_X,c(X[,,i]))
    }
    Out = Mclust(Vector_X,G = G)
    init_values = Out$classification
    z_init = Out$z
    mu_temp = array(0,dim = c(p,r,G))
    init_pars = array(0,dim = c(p,5,G))
    init_pars_nls = array(1,dim = c(p,5,G))
    for(g in c(1:G)){
      mu_temp[,,g] = apply(sweep(X,MARGIN = 3,z_init[,g],FUN = '*'),MARGIN = c(1,2),sum)/sum(z_init[,g])
      for(d in c(1:p)){
        temp = mu_temp[d,,g]
        tryCatch({
          print(c(d,g))
          W <- nls(temp ~ sine_estimate_old(c(1:r), a, b, d, e, f), start = list(a = 1,b = 1, d = 1, e = 1, f = 1),control = list(minFactor = 1e-20,maxiter = 2000))
          init_pars_nls[d,,g] <- coef(W)},error = function(e){
            print(e)
          })
        temp_data = t(X[d,,])
        print(c(d,g))
        print(init_pars_nls[d,,g])
        W <- optim(par = init_pars_nls[d,,g], fn = sine_fit_leastsquares_old, gr = NULL, y = temp_data,z_g = z_init[,g])
        init_pars[d,,g] <- W$par
        mu[d,,g] = sine_estimate_old(c(1:r),init_pars[d,1,g],init_pars[d,2,g],init_pars[d,3,g],init_pars[d,4,g],init_pars[d,5,g])
      }
      phi[,,g] = diag(r)%*%diag(r)
      sigma[,,g] = diag(p)%*%diag(p)
      tau[,g] = length(which(init_values==g))/N
      logdetsigma[,g] = sum(log(eigen(sigma[,,g])$values))
      logdetphi[,g] = sum(log(eigen(phi[,,g])$values))
      sigma_inverse[,,g] = solve(sigma[,,g])
      phi_inverse[,,g] = solve(phi[,,g])
    }
    init_list = list(centers = mu,U = sigma, V = phi,tau = tau, init_sine_pars = init_pars)
  }
  for(ep in 0:epoch){
    #Expectation
    for(g in 1:G){
      z[,g] = c(log(tau[,g])) + c(dmatrixnorm(X,mean = mu[,,g],U = sigma[,,g] , V = phi[,,g] ,log = TRUE))
    }
    z_max = apply(z,1,max)
    z_sum = log(apply(exp(z-z_max),1,sum)) + z_max
    z_hat = exp(z-z_sum)
    #print('LogLik:')
    print(sum(z_sum))
    log_likelihood = c(log_likelihood,sum(z_sum))
    if(ep >= 3){
      log_lik_change = (log_likelihood[length(log_likelihood)]-log_likelihood[length(log_likelihood)-1])/(log_likelihood[length(log_likelihood)-1])
      if(abs(log_lik_change)< convergence){
        converged = 'Converged'
        print('Cluster solution converged')
        break
      }
    }
    phi_temp = array(0, dim = c(r,r,G))
    #Maximization
    #save_loc = paste('temp_posteriorprob_epoch_',as.character(e),'.RData',sep = '')
    #save(z_hat,file = save_loc)
    if(ep == 0){
	temp_z = z
	temp_mu = mu
	sine_pars = array(0,dim = c(p,5,G))
	sine_pars = init_pars
	for(g in 1:G){
		for(dd in c(1:p)){
			for(pp in c(1:5)){
				print(c(ep,g,dd,pp))
				Z = optim(par = c(sine_pars[dd,pp,g]),fn = sine_fit_complete_log_likelihood_old,method = 'BFGS',G=g, var_num = dd, par_num = pp, current_par_val = sine_pars, z = z_hat, z_prob = temp_z, y = X,mu = temp_mu, sigma_inv = sigma_inverse, psi_inv = phi_inverse, logdetsigma = logdetsigma, logdetpsi = logdetphi, control = list(maxit = 5))
				sine_pars[dd,pp,g] = Z$par[1]
				a = sine_pars[dd,1,g]
				b = sine_pars[dd,2,g]
				d = sine_pars[dd,3,g]
				e = sine_pars[dd,4,g]
				f = sine_pars[dd,5,g]
				temp_mu[dd,,g] = sine_estimate_old(c(1:r), a, b, d, e, f)
				temp_z[,g] = (c(log(tau[,g])) + c(dmatrixnorm(X,mean = temp_mu[,,g],U = sigma[,,g] , V = phi[,,g] ,log = TRUE))) 
			}
		}
	}
	init_sine_pars = sine_pars
    }
    else{
	temp_z = z
	temp_mu = mu
	for(g in 1:G){
		for(dd in c(1:p)){
			for(pp in c(1:5)){
				print(c(ep,g,dd,pp))
				Z = optim(par = c(sine_pars[dd,pp,g]),fn = sine_fit_complete_log_likelihood_old,method = 'BFGS',G=g, var_num = dd, par_num = pp, current_par_val = sine_pars, z = z_hat, z_prob = temp_z, y = X,mu = temp_mu, sigma_inv = sigma_inverse, psi_inv = phi_inverse, logdetsigma = logdetsigma, logdetpsi = logdetphi, control = list(maxit = 5))
				sine_pars[dd,pp,g] = Z$par[1]
				a = sine_pars[dd,1,g]
				b = sine_pars[dd,2,g]
				d = sine_pars[dd,3,g]
				e = sine_pars[dd,4,g]
				f = sine_pars[dd,5,g]
				temp_mu[dd,,g] = sine_estimate_old(c(1:r), a, b, d, e, f)
				temp_z[,g] = (c(log(tau[,g])) + c(dmatrixnorm(X,mean = temp_mu[,,g],U = sigma[,,g] , V = phi[,,g] ,log = TRUE))) 
			}
		}
	}
    }
    for(g in 1:G){
      tau[,g] = sum(z_hat[,g])/N
      for(d in c(1:p)){
        mu[d,,g] = sine_estimate_old(c(1:r),sine_pars[d,1,g],sine_pars[d,2,g],sine_pars[d,3,g],sine_pars[d,4,g],sine_pars[d,5,g])
      }
      temp_X = sweep(X,MARGIN = c(1,2),mu[,,g],FUN = '-')
      sigma[,,g]=apply(sweep(array(unlist(apply(temp_X,3,function(mat) mat%*%phi_inverse[,,g]%*%t(mat))),c(p,p,N)),MARGIN = 3,z_hat[,g],'*'),MARGIN = c(1,2),sum)/(r*sum(z_hat[,g]))
      sigma_inverse[,,g] = solve(sigma[,,g])
      CA1 = apply(temp_X,3,function(mat) sum(diag(sigma_inverse[,,g]%*% mat %*% A1 %*% t(mat))))%*%z_hat[,g]
      CA2 = apply(temp_X,3,function(mat) sum(diag(sigma_inverse[,,g]%*% mat %*% A2 %*% t(mat))))%*%z_hat[,g]
      CAI = apply(temp_X,3,function(mat) sum(diag(sigma_inverse[,,g]%*% mat %*% I %*% t(mat))))%*%z_hat[,g]
      a_poly = (2*(r-1)*CA2)
      b_poly = (r-2)*CA1
      c_poly = 2*(CAI+(r*CA2))
      d_poly = (r*CA1)
      temp_rho_k = min(abs(polyroot(c(d_poly,-c_poly,-b_poly,a_poly))))
      if(temp_rho_k<1){
        rho_k[,g] = temp_rho_k
      }
      sig_k[,g] = ((CA2*(rho_k[,g]^2))-(CA1*(rho_k[,g]))+CAI)/(p*r*sum(z_hat[,g]))
      phi[,,g] = (sig_k[,g]/(1-(rho_k[,g])^2))*(matrix(rho_k[,g],r,r)^power_mult)
      logdetsigma[,g] = sum(log(eigen(sigma[,,g])$values))
      logdetphi[,g] = sum(log(eigen(phi[,,g])$values))
      phi_inverse[,,g] = as.matrix((rho_k[,g]^2)*A2-(rho_k[,g]*A1)+(diag(1,r,r)))/(sig_k[,g])
    }
    #print(ep)
    if(ep%%2==0){
      temp_saver = list(probability = z_hat,tau=tau,mu = mu, mu_sine_pars = sine_pars, mu_sine_init_pars = init_sine_pars, sigma = sigma, phi = phi, rho_k = rho_k, sig_k = sig_k, log_likelihood_list = log_likelihood,init_list = init_list,init_values = init_values,init_mu_pars = init_pars, z_init = z_init)
      save_loc = paste(pass_loc,'G',as.character(G),'_AR1_SineSeasonality_completedataloglikelihood_epoch_',as.character(ep),'.RData',sep = '')
      save(temp_saver,file = save_loc)
    }
  }
  if(ep==epoch){
    converged = 'Not converged'
    print('Cluster solution not converged')
  }
  BIC = BIC_Calculator_AR1_Sine_old(sum(z_sum),G,N,p,r)
  predicted_cluster = colnames(data.frame(z_hat))[max.col(data.frame(z_hat), ties.method = "first")]
  return(list(predicted_cluster=predicted_cluster,probability = z_hat,tau=tau,mu = mu, mu_sine_pars = sine_pars, mu_sine_init_pars = init_sine_pars, sigma = sigma, phi = phi, rho_k = rho_k, sig_k = sig_k, log_likelihood_list = log_likelihood,init_list = init_list,init_values = init_values,init_mu_pars = init_pars, z_init = z_init, BIC = BIC))
}


#load('Manly_Transformed_TestData.RData')
load('Matrix_Weather_Data_Final.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
X = Matrix_Data

for(g in c(20)){
  US_Full_RawData_AR1_Old_Sine = my_matrix_GMM_AR1_SineSeasonality_final_old(X, convergence = 1e-8, G = g, z_start = 3, pass_loc = 'US_Full_RawData_AR1_Old_Sine_kmeansstart_rerun_v2_maxit5_optim_perparam_faster_G')
  out = list(G = g, model = US_Full_RawData_AR1_Old_Sine)
  loc = paste('US_Full_RawData_AR1_Old_Sine_kmeansstart_rerun_v2_maxit5_optim_perparam_faster_G',as.character(g),'.RData',sep = '')
  save(out, file = loc)
}
