## Exercise sheet 1
# Ex 1.1 Law of large numbers
# a)

n = c(5 ,20, 50, 200, 1000, 5000, 10000)
#n = seq(1, 10000, 2)

lambda = 10

pois_means = numeric(length = length(n))

j =  0
for(i in n){
  j = j + 1
  pois_means[j] = mean(rpois(i, lambda = lambda))
}


plot(n, pois_means)

# b)

t_means = numeric(length = length(n))
j =  0
for(i in n){
  j = j + 1
  t_means[j] = mean(rt(i, df = 1))
}

plot(n, t_means)


# c)
nsims = 50
pois_means_mat = matrix(data = NA, nrow = nsims, ncol = length(n))

for(i in 1:nsims)
{
j =  0
for(ni in n){
  j = j + 1
  pois_means_mat[i, j] = mean(rchisq(ni, df = 3))
}
}
# Show with a boxplot
boxplot(pois_means_mat, names = n)

# Bring the data in logformat so we can scatter per n_factor
pois_means_long = melt(data = pois_means_mat)
pois_means_long
pois_means_long$X2 = factor(x = pois_means_long$X2, levels= n)
plot( as.numeric(pois_means_long$X2), pois_means_long$value, xaxt = "n")
axis(1, at = 1:7, labels = n)

## Ex 1.2 Central Limit Theorem
n = 50
sum_size = 100
chi_sums = numeric(length = n)
j =  0
for(i in 1:n){
  chi_sums[i] = sum(rchisq(sum_size, df = 1))
}

S = seq(0:(2 * sum_size))
plot(density(chi_sums, kernel = "gaussian"), col = "red", ylim = c(0, 0.04))
lines(S, dnorm(S, mean =  sum_size, sd = sqrt(2 * sum_size)), col = "blue")

## Ex 1.3 Confidence Intervals

# a)
confid = function(X, alpha = 0.05){
  limits = c(alpha / 2, 1 - alpha / 2)
  X_mean = mean(X)
  X_var = var(X)
  confid_int = qnorm(limits, mean =  X_mean, sd = sqrt(X_var))
  return(list("confid_int" = confid_int, "mean" = X_mean, "var" = X_var))
}
confid(rnorm(1000), alpha = 0.05)


# b)
data("iris")

confid(iris$Sepal.Length, 0.05)

library(dplyr)
group_by(iris, Species) %>% do(confid(X = .$Sepal.Length, alpha = 0.05))

unique(Species)
setosa = filter(iris, Species == "setosa")
versicolor = filter(iris, Species == "versicolor")
virginica = filter(iris, Species == "virginica")

confid(setosa$Sepal.Length, 0.05)

confid(versicolor$Sepal.Length, 0.05)

confid(virginica$Sepal.Length, 0.05)


## Ex 1.4
setwd("D:\\Uni\\Statistik\\1. Semester\\Monte_Carlo\\tutorials")
dax = read.csv("DAX.csv")
dax = dax[order(dax$Date), ]
dax$Date = as.Date(dax$Date)

dax.sd = sd(dax$Adj.Close)
dax.mean = mean(dax$Adj.Close)

wn = rnorm(n = length(dax$Adj.Close), mean = 0,  sd = 1)
rw = cumsum(wn)
dax$norm.adj.close = (dax$Adj.Close - dax.mean) / dax.sd

plot(dax$Date, rw,  col = "blue", type = "l")
lines(dax$Date, dax$norm.adj.close, type = "l")

returns = diff(dax$Adj.Close) / dax$Adj.Close[2:length(dax$Adj.Close)] * 100
plot(returns, type = "l")

autocor = function(data, lag = 1){
  x = data[(lag + 1): length(data)]
  x_lag = data[1: (length(data) - lag)]
  print(length(x_lag))
  return(cor(x, x_lag))
}


cors = sapply(1:20, function(x) autocor(returns, lag = x))
barplot(cors)


## Ex. 1.5

# Die Hansen-Hurwitz Methode rechnet jede Einheit hoch und bildet dann aus den gesampelten Einheiten den Durchschnitt.
# Im Gegensatz dazu wird beim simplen Verfahren der gemeinsame Durchschnitt der aller gesampelten Eingheiten verwendet.
# Das Verfahren ist robuster
