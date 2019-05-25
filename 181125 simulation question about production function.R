#181125 simulation question about production function

#we know that in sao paulo the production function coefficients are 
#very different physical capital have a lot
##################################################################################
#> basetemp <- mybase[mybase$UF=='SP',]
#> #regression without the intercept
#  > m1 <- lm(pib2000 ~ -1 + pc2000 + hc2000, data=basetemp)
#  > summary(m1)
#  Call:
#    lm(formula = pib2000 ~ -1 + pc2000 + hc2000, data = basetemp)
#  
#  Residuals:
#    Min       1Q   Median       3Q      Max 
#  -0.82321 -0.27073 -0.08068  0.13916  2.17596 
#  
#  Coefficients:
#    Estimate Std. Error t value Pr(>|t|)    
#  pc2000  1.00768    0.05351  18.830   <2e-16 ***
#    hc2000 -0.01194    0.04716  -0.253      0.8    
#  ---
#    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  
#  Residual standard error: 0.4242 on 641 degrees of freedom
#  Multiple R-squared:  0.9986,	Adjusted R-squared:  0.9986 
#  F-statistic: 2.331e+05 on 2 and 641 DF,  p-value: < 2.2e-16
##################################################################################

#is this due to the ratio of human and physical capital?
#maybe not maybe this does not have anything to do
#or maybe it does, we will simulate some data and see if the ratio of
#human and physical capital are different if affects the results







