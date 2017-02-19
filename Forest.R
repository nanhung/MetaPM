library(metafor)

#Construct data frame
name<-c("Ibfelt et al., 2010", "Ibfelt et al., 2010","Ibfelt et al., 2010","Ibfelt et al., 2010","Ibfelt et al., 2010","Ibfelt et al., 2010","Ibfelt et al., 2010","Ibfelt et al., 2010","Ibfelt et al., 2010","Wilbert et al., 2012","Wilbert et al., 2012", "Costello et al., 2014","Costello et al., 2014","Costello et al., 2014","Costello et al., 2014")
HR<-c(1.11, 1.43, 1.03, 1.23, 1.41, 1.21, 2.51, 2.79, 1.70, 1.21, 1.23, 1.58, 1.78, 1.48, 1.48)
LCL<-c(0.65, 0.85, 0.61, 0.73, 0.84, 0.72, 1.15, 1.29, 0.78, 1.11, 1.17, 0.88,1.02, 0.83, 0.77)
UCL<-c(1.89, 2.41, 1.74, 2.08, 2.36, 2.03, 5.49, 6.04, 3.72, 1.31,1.31,2.63,3.11,2.66,2.85)
dat<- data.frame(name, HR, LCL, UCL)

#Effect sizes == log HR, Estimate sei, Assign values for plotting
dat$yi <- with(dat, log(HR))
dat$sei<- with(dat, (log(UCL)-log(LCL))/(2*1.96))
labs <- dat$name

# Combine data into summary estimate
res <- rma(measure="RR", yi, sei, data=dat, method="DL")
summary(res)

## A fixed-effects model is fitted by setting method="FE". A randomeffects model is fitted by setting method="ML" (the default)
## method="DL" = DerSimonian-Laird estimator

forest(res, transf=exp, refline=1, xlab="HR (95%CI)", slab=labs,  showweight=TRUE, mlab="Summary Estimate")
mtext(paste("I-squared=",summary(res)$I^2),side=3, line=1)
mtext(paste("Heterogeneity p-value=",summary(res)$QEp),side=3, line=-0.25)
text(26, 16.5, "HR [95% CI]", pos = 2)
text(18, 16.5, "Weight", pos = 2)