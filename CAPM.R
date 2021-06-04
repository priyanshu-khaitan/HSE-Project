#estimate a linear model for an asset/ portfolio
model<- lm(formula= y~x, data= data)
summary(model)
#Plot the graph for the  residuals 
resdf<- data.frame(time= as.numeric(time(data$y)),
                   model=as.numeric(residuals(model)))
plot(resdf$model)
#	Plot the graph of residuals and their lagged values
n<- nrow(data)
res_long<- pivot_longer(data=resdf, cols= model, names_to = "model", values_to = "residual")
lag_df<- data.frame(time= as.numeric(time(data$y)),
                    model= c(NA, resdf$model[1:n-1]))
lag_long<- pivot_longer(data= lag_df, cols= model, names_to = "model", values_to = "lag")

res_long<- left_join(res_long, lag_long, by=c("time", "model"))
ggplot(data= res_long, aes(x= time, y= residual, group= model))+
  geom_point()+
  geom_smooth(formula = y~x, method = "lm")+
  facet_wrap(facet= vars(model))
plot(resdf$model, lag_df$model)
# Durbin Watson test
dwt(model, alternative="two.sided")
dwt(model, alternative="negative")
dwt(model, alternative="positive")
# H- Durbin Watson test
data$y_lag<- c(NA, data$y[1:(n-1)])
model_lagged<- lm(y~y_lag+x, data=data)
var_beta<- as.numeric(diag(vcov(model_lagged))[2])
d_st<- dwt(model_lagged)$dw
h_st<- (1-0.5*d_st)*sqrt(n/(1-n*var_beta))
h_st
# Breusch- Godfrey test.
bgtest(model, order=1)
#One sided test that beta is grater than 1
t.test(data$x, mu = 1,
       +        alternative = "greater")
#	One sided test that beta is less than 1.
t.test(data$x, mu = 1,
       +        alternative = "less")