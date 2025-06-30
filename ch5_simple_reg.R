#추정
df <- data.frame(x=c(4,8,9,8,8,
                         12,6,10,6,9),
                 y=c(9,20,22,15,17,
                          30,18,25,10,20))
df
reg <- lm(y~x,data = df)
#beta1 confidence interval
hat_beta1 <- reg$coefficients[2]
s_xx <- sum((df$x-mean(df$x))^2)
MSE <- sum(resid(reg)^2)/reg$df.residual
MSE
beta1_sigma <- sqrt(MSE/s_xx)
t_0.975 <- qt(p = 0.975,lower.tail = T,df=reg$df.residual)

beta1_lower_bound <- hat_beta1 - t_0.975*sqrt(MSE/s_xx)
beta1_upper_bound <- hat_beta1 + t_0.975*sqrt(MSE/s_xx) 
paste0("beta1 confidence interval is (", round(beta1_lower_bound,3), round(beta1_upper_bound,3),") on 95%")

#beta0 confidence interval
hat_beta0 <- reg$coefficients[1]
#SSE <- sum(resid(reg)^2)
beta0_sigma <- sqrt(MSE*((1/nrow(df)) + mean(df$x)^2/s_xx))
beta0_lower_bound <- hat_beta0 - t_0.975*beta0_sigma
beta0_upper_bound <- hat_beta0 + t_0.975*beta0_sigma
paste0("beta0 confidence interval is (", round(beta0_lower_bound,3), round(beta0_upper_bound,3),") on 95%")

#딸깍 코드
confint(reg, level = 0.95)

#hat{y}의 기댓값의 신뢰구간(x=4)
hat_y <- reg$coefficients[1] +reg$coefficients[2]*4
hat_y_sigma <- sqrt(MSE*((1/nrow(df)) + (4 - mean(df$x))^2 / s_xx))
hat_y_lower_bound <- hat_y - t_0.975*hat_y_sigma
hat_y_upper_bound <- hat_y + t_0.975*hat_y_sigma
paste0("beta0 confidence interval is (", round(hat_y_lower_bound,3), round(hat_y_upper_bound,3),") on 95%")
#hat{y}의 기댓값의 신뢰구간 딸깍 코드
predict(reg,newdata = data.frame(x=c(4,6,8,9,10,12)), interval = "confidence", level=0.95)

#실제값 y의 예측구간 : 오차항 변동성 포함.
predict(reg, newdata = data.frame(x = 10), interval = "prediction")

#confidence band
# 예측 + 신뢰대(confidence interval)
pred_conf <- predict(reg, newdata = data.frame(x=df$x), interval = "confidence", level = 0.95)
pred_conf

plot_data <- cbind(data.frame(x=df$x), pred_conf)
plot_data
ggplot(df, aes(x= x,y= y))+
    geom_point()+
    geom_line(data= plot_data,aes(x=x, y= fit),color='blue',lwd=1)+
    geom_ribbon(data = plot_data, aes(x=x, ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue")
#geom_ribbon()에서 y의 aesthetic를 요구하지 않는데 
#이미 based layer를 aes(x=x,y=y)로 받고 있음.

ggplot() +
    geom_point(data = df, aes(x = x, y = y)) +
    geom_line(data = plot_data, aes(x = x, y = fit), color = 'blue', lwd = 1) +
    geom_ribbon(data = plot_data, aes(x = x, ymin = lwr, ymax = upr), 
                fill = "blue", alpha = 0.2)
