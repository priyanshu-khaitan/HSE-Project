# To provide descriptives for the data
summary(data.1)
# To create histogram  for main variable of interest
hist(data.1$sales)
qplot(data.1$sales, bins=50, xlim = c(0,80000000))
hist(data.1$n_employees)
qplot(data.1$n_employees, bins=50, xlim = c(0,600))
# To provide Scatter plot for the main variables.
ggplot(data = data.1 , mapping = aes(x= n_employees, y = sales)) + geom_point()+ geom_smooth(aes(x= n_employees, y = sales), formula = y ~ x, method ="lm",colour="blue")
## To estimate Model1: salesi = ??0 + ??1n_employeesi + ??i
model1<- lm(formula = sales~n_employees, data = data.1)
summary(model1)
## To estimate log-log and log-lin specification of the model
##lnsalesi = ??0 + ??1n_employeesi + ??i
##lnsalesi = ??0 + ??1 lnn_employeesi + ??i
Model3<- lm(formula= log(sales)~n_employees, data= data.1)
summary(Model3)
model2<- lm(formula= log(sales)~ log(n_employees), data = data.1)
summary(model2)
### Provide interpretation of the models??
#Include a dummy variable rd_spendings as an intercept dummy, as a slope dummy and simultaneously (as intercept and slope dummy).
model_d1 <- lm(formula = sales ~ n_employees + rd_spendings , data = data.1)           
summary(model_d1) 
model_d2 <- lm(formula = sales ~ n_employees + rd_spendings:n_employees , data = data.1)                       
summary(model_d2) 
model_d3 <- lm(formula = sales ~ n_employees*rd_spendings , data = data.1)
summary(model_d3)
# Conduct a Chow test
Model3<- lm(formula= log(sales)~n_employees, data= data.1)
model_d3 <- lm(formula = log(sales) ~ n_employees*rd_spendings , data = data.1)
waldtest(Model3, model_d3)
Model 1: log(sales) ~ n_employees
Model 2: log(sales) ~ n_employees * rd_spendings
model_sub1 <- lm(data = data.1 , log(sales) ~ n_employees ,subset = (rd_spendings =="1"))
model_sub2 <- lm(data = data.1 , log(sales) ~ n_employees ,subset = (rd_spendings =="2"))
summary(model_sub1)
summary(model_sub2)
RSS1<- deviance(model_sub1)
RSS1
RSS2<- deviance(model_sub2)
RSS2
RSS<- deviance(Model3)
RSS
nobs(Model3)
nobs(model_sub1)
nobs(model_sub2)
chowF <- ((RSS-RSS1-RSS2)/6)/((RSS1+RSS2)/(nobs(Model3)-12))
chowF
# Conduct the RESET-Ramsey test
Model1<- lm(formula = sales~n_employees+rd_spendings*n_employees+ female_top_manager*n_employees+size+industry*n_employees, data=data.1)
resettest(Model1)
#Investigate residuals for normality
##Histogram
residuals_df_wide<- data_frame(Model1= residuals(Model1))
hist(residuals_df_wide$Model1)
residuals_df<- pivot_longer(residuals_df_wide, cols= starts_with("model"), names_to ="model", values_to = "res" )
residuals_df<- residuals_df %>%
  group_by(model) %>%
  mutate(mean= mean(res), sd= sd(res)) %>%
  mutate(X=seq(from= min(res), to=max(res), length.out=n()),
         +          pdf= dnorm(x=X, mean= mean, sd= sd))
ggplot(data=residuals_df, aes(x=res, group= model))+
  geom_histogram(aes(x=res,y= stat(density)), bins = 50)+
  facet_wrap(facets = vars(model), scales="free_x")+
  geom_line(mapping = aes(x=X, y=pdf, group= model))
##QQ-PLOT.
qqnorm(residuals_df_wide$Model1)
qqline(residuals_df_wide$Model1, lwd=2)
##KOLOMOGOROV- SMIRNOV TEST.
residuals_df2<- data_frame(Model1= residuals(Model1))
ks.test(unique(residuals_df2$Model1),"pnorm")
##SHAPIRO-WILK TEST
residuals_df2_small<- residuals_df2 %>%
  sample_n(size = 377, replace= FALSE)
shapiro.test(residuals_df2_small$Model1)
#VIF
vif(Model1)
#correlations.
X<- model.matrix(sales~  0+n_employees + rd_spendings * n_employees + 
                   female_top_manager * n_employees + size + industry * n_employees, data=data.1 )
cor(X)
#Goldfeld-Quandt Test
gqtest(Model1, order.by= data.1$n_employees, fraction=0.2)
# Breusch-Pagan test
bptest(Model1)
#WHITE TEST:
bptest(formula= Model1, varformula = ~n_employees+ I(n_employees^2), data= data.1 )