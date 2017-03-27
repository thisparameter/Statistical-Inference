library(quantmod)
library(tseries)
library(stringi)
library(MASS)
library(car)
library(mgcv)
library(bestglm)
library(tree)
SymbolList = read.table("http://www.stat.cmu.edu/~cschafer/MSCF/Project/ChallengeSymbols2015.txt")
Longlistall = read.csv("https://s3.amazonaws.com/quandl-static-content/Ticker+CSV%27s/Indicies/SP500.csv")
Longlist = Longlistall[,1]
symbols = Longlist
symbols2 = data.frame()
# Code for modelling
SymbolList2 = data.frame(SymbolList)
SymbolList2 = SymbolList2[-5,]


prevvol1 = numeric(length(symbols))
prevvol2 = numeric(length(symbols))
logvol = numeric(length(symbols))
hilo = numeric(length(symbols))
above_avg = numeric(length(symbols))

resp = numeric(length(symbols))

#Getting the Response
for(i in 1:length(symbols)){
  tryCatch({holddata1 = getSymbols(as.character(symbols[i]), from=Sys.Date()-61, to=Sys.Date()-31, auto.assign=F)  
  resp[i] = sqrt(mean(dailyReturn(Ad(holddata1),type="log")^2))
  },error=function(e){resp[i]=0})  
}
resp

for(i in 1:length(symbols)){
  tryCatch({
    if(resp[i]==0){resp[i]=0.0000000001}
  },error=function(e){resp[i]=0})
}

#The Predictors
for(i in 1:length(symbols)){
  tryCatch({holddata = getSymbols(as.character(symbols[i]), from=(Sys.Date()-61), to=Sys.Date()-31, auto.assign=F)  
  prevvol1[i] = sqrt(mean(dailyReturn(Ad(holddata),type="log")^2))  
  logvol[i] = log(mean(as.numeric(holddata[,c(5)])))
  hilo[i] = mean((as.numeric(holddata[,c(2)]) - as.numeric(holddata[,c(3)]))/as.numeric(holddata[,c(6)]))  
  },error=function(e){})
}


for(i in 1:length(symbols)){
  tryCatch({
    if(is.na(prevvol1[i])){prevvol1[i]=0}
    if(is.na(logvol[i])){logvol[i]=0}
    if(is.na(hilo[i])){hilo[i]=0}
    },error=function(e){prevvol1[i]=0})
}

stockvolavg = mean(prevvol1);
for(i in 1:length(symbols)){
  tryCatch({
  holddata2 = getSymbols(as.character(symbols[i]), from=(Sys.Date()-91), to=Sys.Date()-61, auto.assign=F);
  prevvol2[i] = sqrt(mean(dailyReturn(Ad(holddata2),type="log")^2));
  if(prevvol1[i]>stockvolavg) above_avg[i] = 1 else above_avg[i] = 0;
  },error=function(e){})
}


main_metrics <- yahooQF(c(
  "Short Ratio",
  "Percent Change From 50-day Moving Average", 
  "Percent Change From 200-day Moving Average", 
  "Earnings/Share",
  "Market Capitalization"
  ))
metrics <- getQuote(paste(symbols, sep="", collapse=";"), what=main_metrics, start=Sys.Date()-30)
metrics = data.frame(metrics[,-1])
marcapmetrics = data.frame(metrics[,5])
names(metrics[5])
MarCap=numeric(length(symbols))
for(i in 1:length(symbols))
{ tryCatch({
  string_val = marcapmetrics[i,1]
  num_val = as.numeric(substr(string_val,1,stri_length(string_val)-1));
  if (substr(string_val,stri_length(string_val),stri_length(string_val)) == "B"){
    num_val = num_val * 1000
  } else if (substr(string_val,stri_length(string_val),stri_length(string_val)) == "M"){
    num_val = num_val
  } else {num_val = 0}
  
  MarCap[i]=num_val
 }, error=function(e){num_val =0
                      MarCap[i]=num_val    })
}
logmarcap= log(MarCap+1)

shortratiometrics = data.frame(metrics[,1])
Shortratio=numeric(length(symbols))
for(i in 1:length(symbols))
{ string_val = shortratiometrics[i,1]
  if(string_val=="N/A"){num_val=0
  } else{num_val = as.numeric(substr(string_val,1,stri_length(string_val)-1))}
  Shortratio[i] = num_val
}
logshortratio= log(Shortratio+1)

ch50mametrics = data.frame(metrics[,2])
ch50ma=numeric(length(symbols))
for(i in 1:length(symbols))
{ string_val = ch50mametrics[i,1]
  if(string_val=="N/A"){num_val=0
  } else{num_val = as.numeric(substr(string_val,1,stri_length(string_val)-1))}
  ch50ma[i] = num_val
}

ch200mametrics = data.frame(metrics[,3])
ch200ma=numeric(length(symbols))
for(i in 1:length(symbols))
{ string_val = ch200mametrics[i,1]
  if(string_val=="N/A"){num_val=0
  } else{num_val = as.numeric(substr(string_val,1,stri_length(string_val)-1))}
  ch200ma[i] = num_val
}

epsmetrics = data.frame(metrics[,4])
eps=numeric(length(symbols))
for(i in 1:length(symbols))
{ string_val = epsmetrics[i,1]
  if(string_val=="N/A"){num_val=0
  } else{num_val = as.numeric(substr(string_val,1,stri_length(string_val)-1))}
  eps[i] = num_val
}


par(mfrow=c(1,1))



plot_diagnostics <- function(resp, curmod, model_name = "", file_name = ""){
  par(mfrow=c(2,2))
  cookd = as.numeric(cooks.distance(curmod))
  plot(cookd, xlab= "Observation", ylab = "Cook's Distance")
  lines(c(1, length(cookd)),c(4/length(cookd), 4/length(cookd)),lwd =2, col = 2, lty =2)
  plot(as.numeric(curmod$fitted.values),as.numeric(curmod$residuals), pch=16,xlab="Fitted Values", ylab="Residuals",cex.axis=1.3,cex.lab=1.3)
  qqnorm(as.numeric(curmod$residuals),cex.axis=1.3,cex.lab=1.3,pch=16,main="")
  qqline(as.numeric(curmod$residuals))
  plot(as.numeric(curmod$fitted.values),as.numeric(resp), pch=16,xlab="Fitted Values", ylab="Actual Response",cex.axis=1.3,cex.lab=1.3)
  abline(0,1,lwd=2,col=4)
}

#Models
#Linear Model
boxCox(resp~prevvol1+prevvol2+hilo+logvol+factor(above_avg)+ch200ma+ch50ma+eps+logmarcap+logshortratio)
transresp = bcPower(resp, 0.5)
mod1 = lm(transresp~prevvol1+prevvol2+hilo+logvol+factor(above_avg)+ch200ma+ch50ma+eps+logmarcap+logshortratio)
summary(mod1)
plot(mod1)
AIC(mod1)
#Exhaustive Mod
XYFrame2 = data.frame(prevvol1,prevvol2,hilo,logvol,factor(above_avg),ch200ma,ch50ma,eps,logmarcap,logshortratio,transresp)
#XYFrame2[Reduce(`&`, lapply(XYFrame2, function(x) is.na(x)  | !is.finite(x))),] = 0
XYFrame2 = XYFrame2[Reduce(`&`, lapply(XYFrame2, function(x) !is.na(x)  & is.finite(x))),]
XYFrame = data.frame(prevvol1,prevvol2,hilo,logvol,factor(above_avg),transresp)
XYFrame = XYFrame[Reduce(`&`, lapply(XYFrame, function(x) !is.na(x)  & is.finite(x))),]
bestglmresult = bestglm(XYFrame, IC="AIC", method = "exhaustive", intercept = TRUE)  

exhaustivemod = bestglmresult$BestModel
summary(exhaustivemod)
plot_diagnostics(XYFrame["transresp"],exhaustivemod)
#AIC Based Stepmod
aicmod = stepAIC(mod1)
#Robust Regression
robmod = rlm(transresp~prevvol1+hilo+logmarcap+logshortratio)
summary(robmod)
AIC(robmod)
AIC(exhaustivemod)

#General Additive Models
gammod = gam(transresp~s(prevvol1)+s(prevvol2)+s(hilo)+s(logvol)+factor(above_avg)+s(ch200ma)+s(ch50ma)+s(eps)+s(logmarcap)+s(logshortratio))
summary(gammod)
gammod2 = gam(transresp~s(prevvol1)+s(prevvol2)+s(hilo)+s(ch200ma)+s(ch50ma)+s(logshortratio))
summary(gammod2)
AIC(gammod)
AIC(gammod2)
plot(gammod2, pages =2, scale=0,scheme=1)
gammod3 = gam(transresp~s(prevvol1)+s(prevvol2)+hilo+s(ch200ma)+s(ch50ma)+s(logshortratio))
plot(gammod3, pages =2, scale=0,scheme=1)
AIC(gammod3)
summary(gammod3)
deviance(gammod3)
sum(gammod3$edf)
plot_diagnostics(transresp, gammod3)

#PPR Model and crossvalidation
source("http://www.stat.cmu.edu/~cschafer/MSCF/CVforppr.R")
modelformula = transresp ~ prevvol1 + prevvol2 + hilo + ch200ma + ch50ma + logshortratio
pprCV = matrix(0,nrow=10,ncol=4)
for(j in 1:ncol(pprCV))
{
  set.seed(j)
  for(i in 1:nrow(pprCV))
  {
    pprCV[i,j] = CVforppr(modelformula, nterms=i, numfolds=10, 
                          sm.method="gcvspline")
  }
}
plot(1:nrow(pprCV),apply(pprCV,1,mean),type="b",pch=16,col=2,cex.axis=1.3,
     cex.lab=1.3,xlab="Number of Ridge Functions (M)",
     ylab="Squared Error (via Cross-Validation)")

for(j in 1:ncol(pprCV))
{
  points(1:nrow(pprCV),pprCV[,j],pch=16)
}

pprmod = ppr(transresp~prevvol1+prevvol2+hilo+ch200ma+ch50ma+logshortratio,nterms=4,sm.method="gcvspline")
par(mfrow=c(1,2))
plot(pprmod)
summary(pprmod)
complexity_ppr = sum(pprmod$edf) + length(pprmod$beta) + length(pprmod$alpha)+1
plot(pprmod$fit, pprmod$residuals, xlab="Fitted Values", ylab="Residuals",cex.axis=1.3,cex.lab=1.3,pch=16,cex=0.7)
plot(predict(pprmod),transresp,pch=16,cex=0.7,xlab="Predicted Value",ylab="Actual Response", cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)

#Tree Based Model
fulltree = tree(transresp~prevvol1+prevvol2+hilo+ch200ma+ch50ma+logshortratio, mindev=0, minsize=2)
prunedtree = prune.tree(fulltree, k=0.002)
plot(prunedtree)
help(prune.tree)
text(prunedtree, cex=0.75,digits=4)
cvout = cv.tree.full(fulltree)
plot(cvout)
optalpha = cvout$k[which.min(cvout$dev)]
opttree = prune.tree(fulltree, k=optalpha)
plot(opttree)
text(opttree, cex=0.75, digits = 3)
summary(opttree)
summary(prunedtree)
opttree$
par(mfrow=c(1,1))
plot(as.numeric(predict(prunedtree)),as.numeric(transresp), pch=16,xlab="Fitted Values", ylab="Actual Response",cex.axis=1.3,cex.lab=1.3)
abline(0,1,lwd=2,col=4)
plot(as.numeric(predict(opttree)),as.numeric(residuals(opttree)), pch=16,xlab="Optimal Fitted Values", ylab="Residuals",cex.axis=1.3,cex.lab=1.3)
plot(as.numeric(predict(prunedtree)),as.numeric(residuals(prunedtree)), pch=16,xlab="Fitted Values", ylab="Residuals",cex.axis=1.3,cex.lab=1.3)



#Cross Validation Code for regression trees
cv.tree.full <- function (object, rand, FUN = prune.tree, K = 10, mindev=0, mincut=1, minsize=2,...) 
{
  require(tree)
  if (!inherits(object, "tree")) 
    stop("Not legitimate tree")
  m <- model.frame(object)
  extras <- match.call(expand.dots = FALSE)$...
  FUN <- deparse(substitute(FUN))
  init <- do.call(FUN, c(list(object), extras))
  if (missing(rand)) 
    rand <- sample(K, length(m[[1]]), replace = TRUE)
  cvdev <- 0
  for (i in unique(rand)) {
    tlearn <- tree(model = m[rand != i, , drop = FALSE],mindev=mindev,mincut=mincut,minsize=minsize)
    plearn <- do.call(FUN, c(list(tlearn, newdata = m[rand == 
                                                        i, , drop = FALSE], k = init$k), extras))
    cvdev <- cvdev + plearn$dev
  }
  init$dev <- cvdev
  init
}
#Diagnostics Plot
plot_diagnostics(transresp,exhaustivemod)


#Extra

summary(mod1)
summary(gammod3)

#Comparisons

sum(robmod$residuals^2)
sum(exhaustivemod$residuals^2)
sum(mod1$residuals^2)
sum(gammod3$residuals^2)
sum(pprmod$residuals^2)
AIC(robmod)
AIC(mod1)
AIC(exhaustivemod)
AIC(gammod3)


#Finishing up
print("Saving best model");
finalmodel = gammod3 
save(finalmodel, file="Abhatna1_Models.Robj");
print("done");
