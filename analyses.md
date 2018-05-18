analyses
================
Bruno Hérault
5/18/2018

-   [Ptot versus Clay](#ptot-versus-clay)
-   [Bray P versus Clays](#bray-p-versus-clays)

Ptot versus Clay
================

Exploring the relation between the two through a Michaelis-Menten equation.

**Total P = Theta\_0 + (Theta\_1 \* Clay) / (Theta2 + Clay) with**

Theta\_0: the Total P value when clay equal 0,

(Theta\_0 + Theta\_1) the asymptotic maximum Total P value

Theta\_2 driving the slope of the relationship when Clay close to 0.

Models written in Stan language. Inference using HMC. Total P follows a lognormal law because always positive and variabiliy increase with Clay.

``` r
dataP<-data.frame("ptot"=log(data$Ptot), "clay"=data$Clay)
dataP<-na.omit(dataP)
# library(rstan)
# ptot <- stan(file="Ptot.stan", 
#         data=list(N=length(dataP$ptot), ptot=dataP$ptot, clay=dataP$clay), 
#                      pars=c("theta_0", "theta_1", "theta_2", "sigma"), 
#                       chains=1, 
#                       iter=2500, 
#                       warmup=1000)
# save(ptot, file="ptot.Rdata")
load(file="ptot.Rdata")
```

``` r
library(rstan)
```

    ## Loading required package: ggplot2

    ## Loading required package: StanHeaders

    ## rstan (Version 2.17.3, GitRev: 2e1f913d3ca3)

    ## For execution on a local, multicore CPU with excess RAM we recommend calling
    ## options(mc.cores = parallel::detectCores()).
    ## To avoid recompilation of unchanged Stan programs, we recommend calling
    ## rstan_options(auto_write = TRUE)

``` r
traceplot(ptot, pars=c("theta_0", "theta_1", "theta_2","sigma"))
```

![](analyses_files/figure-markdown_github/Ptot%20diag-1.png)

``` r
plot(ptot)
```

    ## ci_level: 0.8 (80% intervals)

    ## outer_level: 0.95 (95% intervals)

![](analyses_files/figure-markdown_github/Ptot%20diag-2.png)

``` r
par<-extract(ptot)
clay<-0:70
med<-numeric()
q05<-numeric()
q95<-numeric()
for (i in clay){
med<-c(med,median(
  par$theta_0 + par$theta_1*i/(par$theta_2+i)
  ))
q05<-c(q05,quantile(
  par$theta_0 + par$theta_1*i/(par$theta_2+i)
  , probs=0.01))
q95<-c(q95,quantile(
 par$theta_0 + par$theta_1*i/(par$theta_2+i)
  , probs=0.99))
}
```

``` r
library(ggplot2)
newdata <- data.frame(clay=clay, med=med, q05=q05,
                    q95= q95)
ggplot() +
  geom_ribbon(data=newdata, aes(x=clay, ymin = q05, ymax = q95), alpha = .25)+
  geom_point(data=dataP, aes(x = clay, y=ptot))+
  geom_line(data=newdata, aes(x=clay, y=med))+
  xlab("Perc. Clay") + ylab("log(Total P)") + theme(legend.position=" none")
```

![](analyses_files/figure-markdown_github/Ptot%20fig-1.png)

This means that, in this ecosystem :

1.  when, clay = 0, The predicted Total P value is exp(theta\_0) = exp(2.59) = 13.32

2.  when clay tends to 100, the predicted maximum Total P value is exp(theta\_0 + theta\_1) = exp(2.59 + 3.54) = 464.4

3.  the 50% Total\_P point (between clay = 0 and clay =100) is reached when Clay = theta\_3 = 6.73% (so low!)

Bray P versus Clays
===================

to be done
