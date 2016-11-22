## Exercise Sheet 2
install.packages
install.packages("mvtnorm")
library(kolmim)
library(mvtnorm)

# Ex 2.1 ------------------------------------------------------------------

# a) ----------------------------------------------------------------------


congruence_generator = function(z0, a, b, m, n){
 rand_vec = numeric(length = n + 1)
 rand_vec[1] = z0
 for(i in 1:n){
   rand_vec[1 + i] = ((a * rand_vec[i] + b) %% m)
 }
 return(rand_vec[-1])
}

z0 = 17
a = 2
b = 13
m = 50
n = 20
X = congruence_generator(z0, a, b, m, n)
X


# b) ----------------------------------------------------------------------


n = 1000
z0 = 11

# i)
m1 = 4096 
a = 65 
b = 37
X1 = congruence_generator(z0, a, b, m, n)
hist(X, breaks = 100)
plot(density(X, bw = 30))
length(unique(X))

# ii)
m2 = 64
a = 13
b = 41
X2 = congruence_generator(z0, a, b, m2, n)
X
length(unique(X))

# iii)
m3 = 6075
a = 106 
b = 1283
X3 = congruence_generator(z0, a, b, m3, n)
X
length(unique(X))

# iv)
m4 = 259200
a = 7141 
b = 54773
X4 = congruence_generator(z0, a, b, m4, n)
X
length(unique(X))


# Ex 2.2 ------------------------------------------------------------------


# a) ----------------------------------------------------------------------
m = 4096
J = 5
X = X1


# Chi^2 - test ------------------------------------------------------------


chisq_test_un = function(X, J, m){
  n = length(X)
  
  # Define classes
  steps = m / J
  classes = numeric(J  + 1)
  classes[1] = 0
  classes[-1] = steps 
  bins = cumsum(classes)
  
  # number of values in each class
  n_j = table(findInterval(X, vec = bins))
  teta_j = n_j / n
  
  teta_0j = rep(n / J / n, times = J) # uniformly distributed
  
  chi_sq = sum(((n_j - n * teta_0j)^2) / (n * teta_j))
  p_value = 1 - pchisq(chi_sq, (m-1))
  return(list("chi^2" = chi_sq, "p-value" = p_value ))
}

# for i
chisq_test_un(X1, 10, m = m1)
# for ii
chisq_test_un(X2, 20, m = m2)
# for iii
chisq_test_un(X3, 50, m = m3)
# for iv
chisq_test_un(X4, 2, m = m4)

X_ones = rep(1, 1000)
chisq_test_un(X, 10, m = 10)



# Kolmogorov-Smirnov ------------------------------------------------------


kolmo_test = function(X, m){
  n = length(X)
  X_order = sort(X, decreasing = FALSE)

  # compute emperical distribution
  F_emp = cumsum(rep(1, times = length(X_order))) / n
  
  # compute theretical distribution
  F_0 =punif(X_order, 0, (m - 1))
  
  # teststatistic
  C_ks = max(abs(F_emp - F_0))
  print(C_ks)
  print(1.36 / sqrt(n))
  return(ifelse(C_ks > (1.36 / sqrt(n)), "H1", "HO"))
  
}
# for i
kolmo_test(X1, m1)
# for ii
kolmo_test(X2, m2)
# for iii
kolmo_test(X3, m3)
# for iv
kolmo_test(X4, m4)


# Spectral properties -----------------------------------------------------


X1_spec = matrix(X1, nrow = (length(X1) / 2), byrow = FALSE )
plot(X1_spec)

X2_spec = matrix(X2, nrow = (length(X2) / 2), byrow = FALSE )
plot(X2_spec)

X3_spec = matrix(X3, nrow = (length(X3) / 2), byrow = FALSE )
plot(X3_spec)

X4_spec = matrix(X1, nrow = (length(X4) / 2), byrow = FALSE )
plot(X4_spec)

X_runif = runif(n = n, min = 0, max = 70)
X_runif_spec = matrix(X_runif, nrow = (length(X_runif) / 2), byrow = FALSE  )
plot(X_runif_spec)


# b) ----------------------------------------------------------------------


n = 10000
system.time(congruence_generator(z0, a, b, m4, n))[3]
system.time(runif(n, 0, m4))[3]



# Ex 2.3 ----------------------------------------------------------------


alpha = 0.3
beta = 0.4

gap_test = function(X, alpha, beta, m){
X_norm = X / m
in_gap = (alpha < X_norm)  &  (X_norm < beta)
gaps = table(cumsum(in_gap)) - 1
plot(density(gaps))
}

# own function
n = 10000
m = 259200
a = 7141 
b = 54773
X = congruence_generator(z0, a, b, m, n)
gap_test(X, alpha, beta, m)

# runif
X = runif(10000, min = 0, max = m)
gap_test(X, alpha, beta, m)


# Ex 2.4 ------------------------------------------------------------------

# Box-Muller transformation -----------------------------------------------

X1 = runif(10000)
X2 = runif(10000)

Y1 = sqrt(-2 * log(X1)) * cos(2 * pi * X2)
Y2 = sqrt(-2 * log(X1)) * sin(2 * pi * X2)

plot(density(Y1))
plot(density(Y2))


# Central Limit Theorem ---------------------------------------------------

# n = 6
n = 6
X = runif(100000)
X_mat = matrix(X, ncol = n)
means = rowMeans(X_mat)
plot(density(means))

# n = 10
n = 10
X = runif(100000)
X_mat = matrix(X, ncol = n)
means = rowMeans(X_mat)
plot(density(means))

# n = 30
n = 30
X = runif(100000)
X_mat = matrix(X, ncol = n)
means = rowMeans(X_mat)
plot(density(means))

# n = 100
n = 100
X = runif(100000)
X_mat = matrix(X, ncol = n)
means = rowMeans(X_mat)
plot(density(means))

plot(density(rnorm(100000)))


# Inversion method via rational approximation -----------------------------


a0 = 3.30753
a1 = 0.27061
a2 = 0
b1 = 0.99229
b2 = 0.04481
b3 = 0
a0 = 2.515517
a1 = 0.802853
a2 = 0.010328
b1 = 1.432788
b2 = 0.189269
b3 = 0.001308

n = 1000000
p = runif((n /2), 0, 0.5)
t = sqrt(-2  * log(p))
X_pos = t - ((a0 + a1 * t + a2 * t^2) / (1 + b1 * t + b2 * t^2 + b3 * t^3))
p = runif((n / 2), 0, 0.5)
t = sqrt(-2  * log(p))
X_neg = -1 * (t - ((a0 + a1 * t + a2 * t^2) / (1 + b1 * t + b2 * t^2 + b3 * t^3)))
X_p = rbind(X_pos, X_neg)

plot(density(X_p))



# Ex. 2.5 -----------------------------------------------------------------

n = 10000
mu = as.numeric(c(4, 2))
sigma = matrix(c(3, 0.8, 0.8, 2), nrow = 2)

X = matrix(rnorm(n * 2), ncol = 2)
L = chol(sigma)
Y = mu + L %*% t(X)
dim(Y)

rmvnorm(n = n, mean = mu, sigma = sigma)
