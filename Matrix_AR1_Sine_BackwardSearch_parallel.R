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

BIC_Calculator_AR1_Sine_minuszeroedpars<-function(log_likelihood,G = 2, N = 100, p = 2, r = 2,n_zeroedpars = 0){
  n_params = (G-1)+(G*p*5)+(G)+(G*((p*(p+1))/2)) - n_zeroedpars
  #print(n_params)
  BIC = -2*log_likelihood + n_params*log(N)
  return(BIC)
}

maximization_implementer <- function(temp_z_hat,temp_mu,temp_mu_sine_pars,temp_sigma_inverse,temp_phi_inverse,p,r,N,X,A1,A2,I,power_mult,g){
  AA = sum(temp_z_hat)/N
  for(d in c(1:p)){
    temp_mu[d,] = sine_estimate_old(c(1:r),temp_mu_sine_pars[d,1],temp_mu_sine_pars[d,2],temp_mu_sine_pars[d,3],temp_mu_sine_pars[d,4],temp_mu_sine_pars[d,5])
  }
  temp_X = sweep(X,MARGIN = c(1,2),temp_mu,FUN = '-')
  BB = apply(sweep(array(unlist(apply(temp_X,3,function(mat) mat%*%temp_phi_inverse%*%t(mat))),c(p,p,N)),MARGIN = 3,temp_z_hat,'*'),MARGIN = c(1,2),sum)/(r*sum(temp_z_hat))
  CC = solve(BB)
  print(c(g,'A'))
  CA1 = apply(temp_X,3,function(mat) sum(diag(CC%*% mat %*% A1 %*% t(mat))))%*%temp_z_hat
  CA2 = apply(temp_X,3,function(mat) sum(diag(CC%*% mat %*% A2 %*% t(mat))))%*%temp_z_hat
  CAI = apply(temp_X,3,function(mat) sum(diag(CC%*% mat %*% I %*% t(mat))))%*%temp_z_hat
  print(c(g,'A'))
  a_poly = (2*(r-1)*CA2)
  b_poly = (r-2)*CA1
  c_poly = 2*(CAI+(r*CA2))
  d_poly = (r*CA1)
  temp_rho_k = min(abs(polyroot(c(d_poly,-c_poly,-b_poly,a_poly))))
  if(temp_rho_k<1){
    DD = temp_rho_k
  }
  print(c(g,'A'))
  EE = (((CA2*(DD^2))-(CA1*(DD))+CAI)/(p*r*sum(temp_z_hat)))[1,1]
  FF = (EE/(1-DD^2))*(matrix(DD,r,r)^power_mult)
  GG = sum(log(eigen(BB)$values))
  HH = sum(log(eigen(FF)$values))
  II = as.matrix((DD^2)*A2-(DD*A1)+(diag(1,r,r)))/(EE)
  return(list(AA = AA, temp_mu = temp_mu, BB = BB, CC = CC, DD = DD, EE = EE, FF = FF, GG = GG, HH = HH, II = II))
}


sine_estimate_old <- function(t, a, b, d, e, f){
  #(a*t+b) * sin((2 * pi / c) *t+d) + (e*f^t)
  (a*t+b) * sin((2 * pi / 12) *t+d) + (e+f*t)
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
  log_likelihood = sum(z_new)
  return(-log_likelihood)
}


Matrix_AR1_Sine_backwardsearch_parameterreduction <- function(current_best_model_loc,X,n_searches = 10000, epoch_model = 1000, convergence_model = 1e-8,temp_save_loc){
  load(current_best_model_loc)
  N = current_best_model$N 
  p = current_best_model$p
  r = current_best_model$r
  G = current_best_model$G
  for(s in c(1:n_searches)){
    best_model_update_count = 0
    for(mu_par in c(1,5)){
      for(g_var in c(1:G)){
        for(p_var in c(1:p)){
          if(current_best_model$mu_sine_pars[p_var,mu_par,g_var] == 0){
            next
          }
          else{
            print(c(s,mu_par,g_var,p_var))
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
            tau = current_best_model$tau
            mu = current_best_model$mu
            phi = current_best_model$phi
            phi_inverse = current_best_model$phi_inverse
            sigma = current_best_model$sigma
            sigma_inverse = current_best_model$sigma_inverse
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
            for(g in c(1:G)){
              logdetsigma[,g] = sum(log(eigen(sigma[,,g])$values))
              logdetphi[,g] = sum(log(eigen(phi[,,g])$values))
            }
            rho_k = current_best_model$rho_k
            sig_k = current_best_model$sig_k
            z_hat = current_best_model$z_hat
            colnames(z_hat) = c(1:G)
            previous_best_model_log_likelihood = current_best_model$best_model_log_likelihood
            previous_best_model_BIC = current_best_model$best_model_BIC
            mu_sine_pars = current_best_model$mu_sine_pars
            log_likelihood = c()
            for(ep in c(0:epoch_model)){
              #Expectation
              for(g in 1:G){
                z[,g] = c(log(tau[,g])) + c(dmatnorm_calc(X,mean = mu[,,g],U_inv = sigma_inverse[,,g], V_inv = phi_inverse[,,g],p = p, r = r, N = N,logdetU = logdetsigma[,g], logdetV = logdetphi[,g]))
              }
              z_max = apply(z,1,max)
              z_sum = log(apply(exp(z-z_max),1,sum)) + z_max
              z_hat = exp(z-z_sum)
              #print('LogLik:')
              print(sum(z_sum))
              if(ep>0){
                log_likelihood = c(log_likelihood,sum(z_sum))
              }
              if(ep >= 5){
                log_lik_change = (log_likelihood[length(log_likelihood)]-log_likelihood[length(log_likelihood)-1])/(log_likelihood[length(log_likelihood)-1])
                if(abs(log_lik_change)< convergence_model){
                  converged = 'Converged'
                  print('Cluster solution converged')
                  break
                }
              }
              phi_temp = array(0, dim = c(r,r,G))
              #Maximization
              temp_z = z
              temp_mu = mu
              mu_par_zeroed_count = 0
              for(g in 1:G){
                print(c(ep,g))
                for(dd in c(1:p)){
                  for(pp in c(1:5)){
                    if(pp == 1 & mu_sine_pars[dd,1,g] == 0){
                      mu_par_zeroed_count = mu_par_zeroed_count + 1
                      a = mu_sine_pars[dd,1,g]
                      b = mu_sine_pars[dd,2,g]
                      d = mu_sine_pars[dd,3,g]
                      e = mu_sine_pars[dd,4,g]
                      f = mu_sine_pars[dd,5,g]
                      temp_mu[dd,,g] = sine_estimate_old(c(1:r), a, b, d, e, f)
                      temp_z[,g] = (c(log(tau[,g])) + c(dmatnorm_calc(X,mean = temp_mu[,,g],U_inv = sigma_inverse[,,g], V_inv = phi_inverse[,,g],p = p, r = r, N = N,logdetU = logdetsigma[,g], logdetV = logdetphi[,g]))) 
                    }
                    else if(pp == 5 & mu_sine_pars[dd,5,g] == 0){
                      mu_par_zeroed_count = mu_par_zeroed_count + 1
                      a = mu_sine_pars[dd,1,g]
                      b = mu_sine_pars[dd,2,g]
                      d = mu_sine_pars[dd,3,g]
                      e = mu_sine_pars[dd,4,g]
                      f = mu_sine_pars[dd,5,g]
                      temp_mu[dd,,g] = sine_estimate_old(c(1:r), a, b, d, e, f)
                      temp_z[,g] = (c(log(tau[,g])) + c(dmatnorm_calc(X,mean = temp_mu[,,g],U_inv = sigma_inverse[,,g], V_inv = phi_inverse[,,g],p = p, r = r, N = N,logdetU = logdetsigma[,g], logdetV = logdetphi[,g]))) 
                    }
                    else if(g == g_var & dd == p_var & pp == mu_par){
                      mu_par_zeroed_count = mu_par_zeroed_count + 1
                      mu_sine_pars[dd,pp,g] = 0
                      a = mu_sine_pars[dd,1,g]
                      b = mu_sine_pars[dd,2,g]
                      d = mu_sine_pars[dd,3,g]
                      e = mu_sine_pars[dd,4,g]
                      f = mu_sine_pars[dd,5,g]
                      temp_mu[dd,,g] = sine_estimate_old(c(1:r), a, b, d, e, f)
                      temp_z[,g] = (c(log(tau[,g])) + c(dmatnorm_calc(X,mean = temp_mu[,,g],U_inv = sigma_inverse[,,g], V_inv = phi_inverse[,,g],p = p, r = r, N = N,logdetU = logdetsigma[,g], logdetV = logdetphi[,g])))
                    }
                    else{
                      Z = optim(par = c(mu_sine_pars[dd,pp,g]),fn = sine_fit_complete_log_likelihood_old,method = 'BFGS',G=g, var_num = dd, par_num = pp, current_par_val = mu_sine_pars, z = z_hat, z_prob = temp_z, y = X,mu = temp_mu, sigma_inv = sigma_inverse, psi_inv = phi_inverse, logdetsigma = logdetsigma, logdetpsi = logdetphi, control = list(maxit = 5))
                      mu_sine_pars[dd,pp,g] = Z$par[1]
                      a = mu_sine_pars[dd,1,g]
                      b = mu_sine_pars[dd,2,g]
                      d = mu_sine_pars[dd,3,g]
                      e = mu_sine_pars[dd,4,g]
                      f = mu_sine_pars[dd,5,g]
                      temp_mu[dd,,g] = sine_estimate_old(c(1:r), a, b, d, e, f)
                      temp_z[,g] = (c(log(tau[,g])) + c(dmatnorm_calc(X,mean = temp_mu[,,g],U_inv = sigma_inverse[,,g], V_inv = phi_inverse[,,g],p = p, r = r, N = N,logdetU = logdetsigma[,g], logdetV = logdetphi[,g]))) 
                    }
                  }
                }
              }
              result <- foreach(m = c(1:G),.export = c('maximization_implementer','sine_estimate_old')) %dopar% maximization_implementer(z_hat[,m],mu[,,m],mu_sine_pars[,,m],sigma_inverse[,,m],phi_inverse[,,m],p,r,N,X,A1,A2,I,power_mult,m)
              for(g in c(1:G)){
                tau[,g] = result[[g]]$AA
                mu[,,g] = result[[g]]$temp_mu
                sigma[,,g] = result[[g]]$BB
                sigma_inverse[,,g] = result[[g]]$CC
                rho_k[,g] = result[[g]]$DD
                sig_k[,g] = result[[g]]$EE
                phi[,,g] = result[[g]]$FF
                logdetsigma[,g] = result[[g]]$GG
                logdetphi[,g] = result[[g]]$HH
                phi_inverse[,,g] = result[[g]]$II 
              }
              #print(ep)
              if(ep%%5==0){
                temp_saver = list(probability = z_hat,tau=tau,mu = mu, mu_sine_pars = mu_sine_pars, sigma = sigma, phi = phi, rho_k = rho_k, sig_k = sig_k, log_likelihood_list = log_likelihood,n_search = s,best_model_update_count = best_model_update_count,g_var = g_var, p_var = p_var, mu_par = mu_par)
                save_loc = paste(temp_save_loc,'G',as.character(G),'_Matrix_AR1_Sine_backwardsearch_',as.character(s),'_',as.character(mu_par),'_',as.character(g_var),'_',as.character(p_var),'_interim_epoch_',as.character(ep),'.RData',sep = '')
                save(temp_saver,file = save_loc)
              }
            }
            if(ep==epoch_model){
              converged = 'Not converged'
              print('Cluster solution not converged')
            }
            print(c('mu_par_zeroed_count',mu_par_zeroed_count))
            BIC = BIC_Calculator_AR1_Sine_minuszeroedpars(sum(z_sum),G,N,p,r,n_zeroedpars = mu_par_zeroed_count)
            print(c('BIC',BIC,'previous best BIC',previous_best_model_BIC))
            if(BIC<previous_best_model_BIC){
              current_best_model = list(G = G, N = N, p = p, r = r, tau = tau, mu = mu, mu_sine_pars = mu_sine_pars, sigma = sigma, sigma_inverse = sigma_inverse, phi = phi, phi_inverse = phi_inverse,rho_k = rho_k, sig_k = sig_k, z_hat = z_hat, best_model_log_likelihood = sum(z_sum),best_model_BIC = BIC)
              best_model_update_count = best_model_update_count + 1
              save(current_best_model,file = current_best_model_loc)
	      save_loc = paste(temp_save_loc,'G',as.character(G),'_reduced_parameters_best_model_',as.character(s),'_',as.character(mu_par),'_',as.character(g_var),'_',as.character(p_var),'_saved','.RData',sep = '')
	      save(current_best_model,file = save_loc)
              print('best model updated')
            }
          }
          load(current_best_model_loc)
        }
      }      
    }
    print(c('best model update count',best_model_update_count))
    if(best_model_update_count == 0){
      load(current_best_model_loc)
      save(current_best_model,file = current_best_model_loc)
      print('final best model found')
      break
    }
  }
}

library(parallel)  
library(doParallel)
print(detectCores())
no_cores <- 20
no_cores
cl <- makeCluster(no_cores)  
registerDoParallel(cl)  
clusterEvalQ(cl, {
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
})


load('Matrix_AR1_Sine_reduced_parameters_best_model.RData')
load('Matrix_Weather_Data_Final.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
X = Matrix_Data

out = Matrix_AR1_Sine_backwardsearch_parameterreduction('Matrix_AR1_Sine_reduced_parameters_best_model.RData', X, temp_save_loc = 'US_Full_RawData_')