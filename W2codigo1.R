
#tickers = as.vector(top100$firm)

library(quantmod)
tickers = c("MSFT","AAPL","TESLA","asdf","wegf","WMT")
listatickers=c()
i = 0
for(t in tickers) {
  i=i+1
   tryCatch(
     {
       getSymbols(t, periodicity = "monthly",
                  from = "2020-01-01", to = "2023-10-31")
       listatickers = c(listatickers,t)
       cat("Ticker ",t, "was found in Yahoo Finance \n")
       if (i==1) {
         temp = get(t)
         prices = Ad(temp)
       }
       else {
         temp = get(t)
         prices=merge(prices,Ad(temp))  
       }
     },
     error=function(e) {
       cat('Ticker ',t, " NOT FOUND in Yahoo Finance\n")
     }
   )
}

returns = diff(log(prices))
colnames(returns) = listatickers

listatickers
# borro todos los datasets de precios de todos los stocks
do.call(rm,as.list(listatickers))

getSymbols(Symbols="^GSPC", periodicity = "monthly",
           from = "2020-01-01", to = "2023-10-31")

mktreturns = diff(log(Ad(GSPC)))
names(mktreturns)= c("SP500")

# I run market regression models for all stocks:

matresults = c()
for (i in 1:ncol(returns)) {
  # Running the market regression model:
  model <- lm(returns[,i] ~ mktreturns$SP500, na.action=na.omit)
  # Saving the beta coefficients along with their standard errors, t Statistics and p-values:
  coefs = summary(model)$coefficients
  # Putting the beta information into a vector
  vcoefs = c(coefs[1,],coefs[2,])
  # Adding the vector into the result matrix:
  matresults = rbind(matresults,vcoefs)
}

matresults = as.data.frame(matresults)
names(matresults) = c("b0","se_b0","t_b0","pv_b0","b1","se_b1","t_b1","pv_b1")
rownames(matresults) = listatickers

