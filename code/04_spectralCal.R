library(leaps)
library(DescTools)

# Prep ----
## Read data
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")

## Combine plant and soil for model fitting
pirms = pirms[, names(pirms) != "Species"]
names(sirms) = names(pirms)
airms = rbind(pirms, sirms)

# Model selection d18O ----
airms$d18O.off = airms$d18O - airms$d18O.irms

## Visualize
plot(airms$BaseShift, airms$d18O.off)
plot(airms$SlopeShift, airms$d18O.off)
plot(airms$Residuals, airms$d18O.off)
plot(airms$BaseCurve, airms$d18O.off)
plot(airms$CH4, airms$d18O.off)

## Search all models
rsO = regsubsets(d18O.off ~ BaseShift * SlopeShift * Residuals * BaseCurve * CH4,
                data = airms, method = "exhaustive")

## Look at all models
ci = apply(summary(rsO)$which, 2, any)
data.frame(summary(rsO)$which[, ci], summary(rsO)$bic, summary(rsO)$adjr2)

## Best BIC
ci = summary(rsO)$which[5, ]
rsO$xnames[ci]

## Fit
lmO = lm(d18O.off ~ BaseShift + Residuals + CH4 + 
           Residuals:BaseCurve + BaseShift:Residuals:BaseCurve,  data = airms)

## Strong colliniarity
VIF(lmO)

## Search models w/o interactions
rsO = regsubsets(d18O.off ~ BaseShift + SlopeShift + Residuals + BaseCurve + CH4,
                 data = airms, method = "exhaustive")

## Look at models
ci = apply(summary(rsO)$which, 2, any)
data.frame(summary(rsO)$which[, ci], summary(rsO)$bic, summary(rsO)$adjr2)

## Best BIC
ci = summary(rsO)$which[3, ]
rsO$xnames[ci]

## Fit
lmO = lm(d18O.off ~ BaseShift + Residuals + CH4,  data = airms)

## Much better
VIF(lmO)

# Model selection d2H ----
airms$d2H.off = airms$d2H - airms$d2H.irms

## Visualize
plot(airms$BaseShift, airms$d2H.off)
plot(airms$SlopeShift, airms$d2H.off)
plot(airms$Residuals, airms$d2H.off)
plot(airms$BaseCurve, airms$d2H.off)
plot(airms$CH4, airms$d2H.off)

## Search all models
rsH = regsubsets(d2H.off ~ BaseShift * SlopeShift * Residuals * BaseCurve * CH4,
                 data = airms, method = "exhaustive")

## Look at all models
ci = apply(summary(rsH)$which, 2, any)
data.frame(summary(rsH)$which[, ci], summary(rsH)$bic, summary(rsH)$adjr2)

## Best BIC
ci = summary(rsH)$which[1, ]
rsH$xnames[ci]

## Fit
lmH = lm(d2H.off ~ SlopeShift:CH4,  data = airms)

# Residuals ----
## d18O
shapiro.test(lmO$residuals)

plot(density(lmO$residuals))
points(lmO$residuals[airms$Instrument == "HIDS2046"],
       rep(0, sum(airms$Instrument == "HIDS2046")))
points(lmO$residuals[airms$Instrument == "HIDS2052"],
       rep(0, sum(airms$Instrument == "HIDS2052")), col = 2)

wilcox.test(lmO$residuals[airms$Instrument == "HIDS2046"],
       lmO$residuals[airms$Instrument == "HIDS2052"])

## d2H
shapiro.test(lmH$residuals)

plot(density(lmH$residuals))
points(lmH$residuals[airms$Instrument == "HIDS2046"],
       rep(0, sum(airms$Instrument == "HIDS2046")))
points(lmH$residuals[airms$Instrument == "HIDS2052"],
       rep(0, sum(airms$Instrument == "HIDS2052")), col = 2)

t.test(lmH$residuals[airms$Instrument == "HIDS2046"],
            lmH$residuals[airms$Instrument == "HIDS2052"])

# Test model ----
test = function(ntest, niter){
  resO = resH = numeric()
  for(i in 1:niter){
    ind = sample(1:nrow(airms), ntest)
    tr = airms[-ind,]
    ts = airms[ind,]
    
    lmO = lm(d18O.off ~ BaseShift + Residuals + CH4, data = tr)
    lmH = lm(d2H.off ~ SlopeShift:CH4, data = tr)
    
    pO = predict(lmO, ts)
    pH = predict(lmH, ts)  
    
    resO = c(resO, ts$d18O.off - pO)
    resH = c(resH, ts$d2H.off - pH)
  }
  return(list(resO = resO, resH = resH))
}

t10 = test(7, 1000)
t20 = test(15, 1000)

sd(t10$resO)
sd(t10$resH)

sd(t20$resO)
sd(t20$resH)

## Save
write.csv(pirms, "out/pirms.csv", row.names = FALSE)
write.csv(sirms, "out/sirms.csv", row.names = FALSE)
write.csv(airms, "out/airms.csv", row.names = FALSE)
save(lmO, lmH, airms, file = "out/lm.rda")
