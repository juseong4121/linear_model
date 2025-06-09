#rencher: linermodel in statistics
#ch8. overall Test
data(mtcars)

mtcars[,c('mpg','wt','hp','disp')] %>% head(5)

# y : mpg /  x1 ,x2,x3 in Col(X) 
fit <- lm(mpg ~ wt + hp + disp, data = mtcars) 

# 분산분석표 (ANOVA table)로 F검정 수행
anova(fit)


#F검정 이론으로 구해보기.
fitted(fit) # hat{y}

SSR <- sum((fitted(fit) - mean(mtcars$mpg))^2)
#sum((mtcars$mpg - fitted(fit))^2)
SSE <- sum(resid(fit)^2) # sum error^2 , error = resid() = y- hat(y)
SSE
n <- mtcars %>% nrow()
k <- 3
F_stat <- (SSR / k) / (SSE / (n - k - 1))
F_stat
pf(F_stat, 3, 28, lower.tail = F) # p_value check.
summary(fit) # F-statistic check. 44.57 on df1 3 and df2 28

pf(q = 121.7305, df1 = 1, df2 =  28,lower.tail = F) #wt(X1)의 p_value

#H0가 거짓일때 비중심 F분포 lambda= (Xc *beta1)' (Xc *beta1)/2sigma^2
#bold{beta_hat} = 열벡터 1에 대응하는 alpha를 제외한 (beta1 beta2 ... betak)'
beta_hat <- coefficients(fit)[-1]

#Disign matrix
X <- model.matrix(fit)

#X1 : 첫 컬럼벡터들 "1"을 제외한 남은 X 행렬
X1 <- X[,-1]

#Xc : (I- [1/n]*J)X1 ~ centered form
n <- X1 %>% nrow()
I <- diag(n)
J <- matrix(1, nrow = n, ncol = n)

Xc <- (I- J/n)%*%X1

# sigma^2 : 실제로 모르는 미지의 수임. 
#그러므로, hat{sigma^2} = MSE = SSE/(n-k-1)을 사용 
sigma2_hat <- SSE/(n-k-1)

lambda_centered <-  t(beta_hat) %*% t(Xc) %*% Xc %*% beta_hat / (2 * sigma2_hat)
lambda_centered #matrix form
lambda_centered <- as.numeric(lambda_centered)

#plot
x_vals <- seq(0, 10, length.out = 500)

df1 <- ncol(Xc)
df2 <- n-k-1

F_central <- df(x_vals, df1, df2)
F_noncentral <- df(x_vals, df1, df2, ncp = lambda_centered)

#red : under H0, blue : under H1
plot(x_vals, F_central, lwd=3, col='red', main = "F-dist : central and noncentral", type='l')
lines(x_vals, F_noncentral, lwd=3, col= 'blue', type= 'l')

alpha <- 0.05
F_crit <- qf(1 - alpha, df1, df2, lower.tail =T) # Pr(F<F*)
F_crit

#power[검정력: beta(theta)] 계산 
#under H1 -> reject H0, P[X in R] where, R is crit. 
#P[X in R] = P[X > F_crit] why? reject H0 해야하기 때문!!
power_noncentral <- 1 - pf(F_crit, df1, df2, ncp = lambda_centered)
power_noncentral


lambda_seq <- seq(0, 100, length.out = 200)
power_vals <- 1 - pf(F_crit, df1, df2, ncp = lambda_seq)

