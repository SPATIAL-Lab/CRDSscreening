library(DescTools)

# Prep
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")
irms = read.csv("data/irms.csv")

# Compare with irms data
s$ID = substr(s$Sample_ID, 8, nchar(s$Sample_ID))
p$ID = substr(p$Sample_ID, 8, nchar(p$Sample_ID))
sirms = merge(s, irms, by = "ID")
pirms = merge(p, irms, by = "ID")

# Soil combine
s.diff.h = apply(cbind(sirms$d2H.irms, sirms$d2H), 1, diff)
s.diff.o = apply(cbind(sirms$d18O.irms, sirms$d18O), 1, diff)

# One outlier looks like it evaporated between analyses
sirms = sirms[s.diff.o > -5, ]
s.diff.h = s.diff.h[s.diff.o > -5]
s.diff.o = s.diff.o[s.diff.o > -5]

# Soil stats
shapiro.test(s.diff.h)
shapiro.test(s.diff.o)
t.test(s.diff.h)
t.test(s.diff.o)
sd(s.diff.h)
sd(s.diff.o)

# Plant combine
p.diff.h = apply(cbind(pirms$d2H.irms, pirms$d2H), 1, diff)
p.diff.o = apply(cbind(pirms$d18O.irms, pirms$d18O), 1, diff)

# Plant stats
shapiro.test(p.diff.h)
shapiro.test(p.diff.o)
SignTest(p.diff.h)
SignTest(p.diff.o)
t.test(p.diff.h)
t.test(p.diff.o)
sd(p.diff.h)
sd(p.diff.o)

# Write out these results
write.csv(pirms, "out/pirms.csv", row.names = FALSE)
write.csv(sirms, "out/sirms.csv", row.names = FALSE)