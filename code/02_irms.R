library(DescTools)

# Prep ----
## Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")
irms = read.csv("data/irms.csv")

# Compare with irms data ----
s$ID = substr(s$Sample_ID, 8, nchar(s$Sample_ID))
p$ID = substr(p$Sample_ID, 8, nchar(p$Sample_ID))
sirms = merge(s, irms, by = "ID")
pirms = merge(p, irms, by = "ID")

## Soil stats
s.diff.h = apply(cbind(sirms$d2H, sirms$d2H.irms), 1, diff)
s.diff.o = apply(cbind(sirms$d18O, sirms$d18O.irms), 1, diff)
### One outlier looks like it evaporated between analyses
s.diff.o = s.diff.o[s.diff.o < 5]
shapiro.test(s.diff.h)
shapiro.test(s.diff.o)
t.test(s.diff.h)
t.test(s.diff.o)
sd(s.diff.h)
sd(s.diff.o)

## Plant stats
p.diff.h = apply(cbind(pirms$d2H, pirms$d2H.irms), 1, diff)
p.diff.o = apply(cbind(pirms$d18O, pirms$d18O.irms), 1, diff)
shapiro.test(p.diff.h)
shapiro.test(p.diff.o)
SignTest(p.diff.h)
SignTest(p.diff.o)
t.test(p.diff.h)
t.test(p.diff.o)
sd(p.diff.h)
sd(p.diff.o)

## Write out these results
p = cbind(pirms, p.diff.h, p.diff.o)
write.csv(p, "out/plants.csv")
