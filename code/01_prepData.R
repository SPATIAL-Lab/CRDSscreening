library(isoWater)

# Get soil and plant data from wiDB
d = wiDB_data(projects = '00384')
d = d$data

# Add bout index
bout = substr(d$Sample_ID, 8, 12)
sort(unique(bout))
d$Bout = bout

# Add Dex
d$Dex = d$d2H - d$d18O * 8

# Plants
p = d[d$Type == "Stem",]
p$Species = p$Sample_Comments
sort(unique(p$Species))

# Soils
s = d[d$Type == "Soil",]

# Save ----
write.csv(p, file = "out/plants.csv", row.names = FALSE)
write.csv(s, file = "out/soils.csv", row.names = FALSE)

