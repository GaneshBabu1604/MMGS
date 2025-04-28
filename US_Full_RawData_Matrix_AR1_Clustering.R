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

BIC_Calculator_AR1<-function(log_likelihood,G = 2, N = 100, p = 2, r = 2){
  n_params = (G-1)+(G*p*r)+(G)+(G*((p*(p+1))/2))
  #print(n_params)
  BIC = -2*log_likelihood + n_params*log(N)
  return(BIC)
}


my_matrix_GMM_AR1 <- function(X, convergence = 1e-5, epoch = 1000, z_start = 3, init_values = NULL, G = 2, pass_loc){
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
  rho_k = matrix(0,nrow = 1,ncol = G)
  sig_k = matrix(0,nrow = 1,ncol = G)
  z_hat = matrix(0,nrow = N, ncol = G)
  colnames(z_hat) = c(1:G)
  log_likelihood = c()
  init_list = NULL
  if(z_start==1){
    if(length(init_values)==dim(X)[3]){
      for(g in c(1:G)){
        mu[,,g] = apply(X[,,which(init_values==g)],MARGIN = c(1,2),mean)
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
    for(g in c(1:G)){
      mu[,,g] = apply(X[,,which(init_values==g)],MARGIN = c(1,2),mean)
      phi[,,g] = diag(r)%*%diag(r)
      sigma[,,g] = diag(p)%*%diag(p)
      tau[,g] = length(which(init_values==g))/N
      phi_inverse[,,g] = solve(phi[,,g])
    }
    init_list = list(centers = mu,U = sigma, V = phi,tau = tau)
  }
  else{
    Vector_X = data.frame()
    for(i in c(1:N)){
      Vector_X = rbind(Vector_X,c(X[,,i]))
    }
    Out = Mclust(Vector_X,G = G)
    init_values = Out$classification
    for(g in c(1:G)){
      mu[,,g] = apply(X[,,which(init_values==g)],MARGIN = c(1,2),mean)
      phi[,,g] = diag(r)%*%diag(r)
      sigma[,,g] = diag(p)%*%diag(p)
      tau[,g] = length(which(init_values==g))/N
      phi_inverse[,,g] = solve(phi[,,g])
    }
    init_list = list(centers = mu,U = sigma, V = phi,tau = tau)
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
    for(g in 1:G){
      tau[,g] = sum(z_hat[,g])/N
      mu[,,g] = apply(sweep(X,MARGIN = 3,z_hat[,g],FUN = '*'),MARGIN = c(1,2),sum)/sum(z_hat[,g])
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
	print(ep)
        rho_k[,g] = temp_rho_k
      }
      sig_k[,g] = ((CA2*(rho_k[,g]^2))-(CA1*(rho_k[,g]))+CAI)/(p*r*sum(z_hat[,g]))
      phi[,,g] = (sig_k[,g]/(1-(rho_k[,g])^2))*(matrix(rho_k[,g],r,r)^power_mult)
      phi_inverse[,,g] = as.matrix((rho_k[,g]^2)*A2-(rho_k[,g]*A1)+(diag(1,r,r)))/(sig_k[,g])
    }
  }
  if(ep==epoch){
    converged = 'Not converged'
    print('Cluster solution not converged')
  }
  BIC = BIC_Calculator_AR1(sum(z_sum),G,N,p,r)
  predicted_cluster = colnames(data.frame(z_hat))[max.col(data.frame(z_hat), ties.method = "first")]
  return(list(predicted_cluster=predicted_cluster,probability = z_hat,tau=tau,mu = mu, sigma = sigma, phi = phi, rho_k = rho_k, sig_k = sig_k, log_likelihood_list = log_likelihood,init_param_list = init_list, init_cluster = init_values, BIC = BIC))
}


load('Matrix_Weather_Data_Final.RData')
loc = read.csv('Matrix_Weather_Data_Final_Locations.csv')
X = Matrix_Data

for(g in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,50)){
  print(c('G',g))
  US_Full_RawData_Matrix_AR1 = my_matrix_GMM_AR1(X, convergence = 1e-8, G = g, z_start = 3, pass_loc = 'US_Full_RawData_Matrix_AR1_G')
  out = list(G = g, model = US_Full_RawData_Matrix_AR1)
  loc = paste('US_Full_RawData_Matrix_AR1_G',as.character(g),'.RData',sep = '')
  save(out, file = loc)
}
