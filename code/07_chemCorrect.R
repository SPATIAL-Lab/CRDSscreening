# Parse the evil ChemCorrect files to extract flags
# Files were created using instruction set chemcorrect_inst avg_orgeval_10.csv
# Then opened and saved as .xslx

library(openxlsx)
library(tidyxl)

flagged = function(x){
  rbind(table(x), table(x) / length(x))
}

# Extract flags ----
## ChemCorrect files
fl = list.files("data/cc/", full.names = TRUE, pattern = ".xlsx")

## Space to store results
cc = data.frame("ID" = character(), "RunDate" = character(), 
                "Instrument" = character(), "Flag" = integer())

## Cycle through files
for(i in seq_along(fl)){
  ### Get cf file name from cc file
  cfn = as.character(read.xlsx(fl[i], "Summary", rows = 2, colNames = FALSE))
  
  ### Read data from cc file
  d = read.xlsx(fl[i], "Summary", startRow = 4, cols = 3)
  names(d) = "ID"
  d$RunDate = rep(substr(cfn, 1, 6))
  d$Instrument = rep(substr(cfn, nchar(cfn) - 7, nchar(cfn)))
  
  ### Pull relevant cells from cc file 
  cells = xlsx_cells(fl[i], sheets = "Summary")
  cells = cells[cells$col == 3, ]
  cells = cells[-(1:3), ]
  
  ### Get formats from cc file
  f = xlsx_formats(fl[i])
  form = f$local$fill$patternFill$bgColor
  
  ### Red cells
  rc = which(f$local$fill$patternFill$bgColor$rgb == "FF993300")
  red = cells$local_format_id %in% rc
  
  ### Yellow cells
  yc = which(f$local$fill$patternFill$bgColor$rgb == "FFFFFF00")
  yell = cells$local_format_id %in% yc
  
  ### Store flag status: green = 0, yellow = 1, red = 2
  d$Flag = rep(0)
  d$Flag[yell] = 1
  d$Flag[red] = 2
  
  cc = rbind(cc, d)
}

## Condense info per sample...choose highest flag status
ID = unique(cc$ID)
Flag = integer(length(ID))

for(i in seq_along(ID)){
  Flag[i] = max(cc$Flag[cc$ID == ID[i]])
}

cc = data.frame(ID, Flag)

# Merge - IRMS ----
## Read data
pirms = read.csv("out/pirms.csv")
sirms = read.csv("out/sirms.csv")

## Get plant sample names and match 
pirms.ID = substr(pirms$Sample_ID, 8, nchar(pirms$Sample_ID))
pind = match(pirms.ID, cc$ID)
pirms$Flag = cc$Flag[pind]

## Get soil sample names and match 
sirms.ID = substr(sirms$Sample_ID, 8, nchar(sirms$Sample_ID))
sind = match(sirms.ID, cc$ID)
sirms$Flag = cc$Flag[sind]

## Summarize
### Total number and fraction of flagged samples
rbind(table(pirms$Flag), table(pirms$Flag) / nrow(pirms))
rbind(table(sirms$Flag), table(sirms$Flag) / nrow(sirms))
rbind(table(c(pirms$Flag, sirms$Flag)), table(c(pirms$Flag, sirms$Flag)) / 
        (nrow(pirms) + nrow(sirms)))

### Relationship between offsets and flags
tapply(pirms$Flag, 
       pirms$d18O - pirms$d18O.irms > 1 | pirms$d2H - pirms$d2H.irms > 8, 
       flagged)

tapply(sirms$Flag, 
       sirms$d18O - sirms$d18O.irms > 1 | sirms$d2H - sirms$d2H.irms > 8, 
       flagged)

cc.irms = data.frame("d18O.diff" = pirms$d18O - pirms$d18O.irms, 
                     "d2H.diff" = pirms$d2H - pirms$d2H.irms, 
                     "Flag" = pirms$Flag)
cc.irms = rbind(cc.irms, data.frame("d18O.diff" = sirms$d18O - sirms$d18O.irms, 
                                    "d2H.diff" = sirms$d2H - sirms$d2H.irms, 
                                    "Flag" = sirms$Flag))
cc.irms$large = cc.irms$d18O.diff > 1 | cc.irms$d2H.diff > 8

tapply(cc.irms$Flag, cc.irms$large, flagged)

write.csv(pirms, "out/pirms.csv", row.names = FALSE)
write.csv(sirms, "out/sirms.csv", row.names = FALSE)

# Merge - all data ----
## Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")

## Get plant sample names and match 
p.ID = substr(p$Sample_ID, 8, nchar(p$Sample_ID))
pind = match(p.ID, cc$ID)
p$Flag = cc$Flag[pind]

## Get soil sample names and match 
s.ID = substr(s$Sample_ID, 8, nchar(s$Sample_ID))
sind = match(s.ID, cc$ID)
s$Flag = cc$Flag[sind]

## Summarize
### Total number and fraction of flagged samples
rbind(table(p$Flag), table(p$Flag) / nrow(p))
rbind(table(s$Flag), table(s$Flag) / nrow(s))
rbind(table(c(p$Flag, s$Flag)), table(c(p$Flag, s$Flag)) / (nrow(p) + nrow(s)))

### Relationship between offsets and flags
tapply(p$Flag, abs(p$d18O.off) > 1 | abs(p$d2H.off) > 8, flagged)

tapply(s$Flag, abs(s$d18O.off) > 1 | abs(s$d2H.off) > 8, flagged)

cc.all = data.frame("d18O.off" = p$d18O.off, "d2H.off" = p$d2H.off, 
                    "Flag" = p$Flag)
cc.all = rbind(cc.all, data.frame("d18O.off" = s$d18O.off, 
                                  "d2H.off" = s$d2H.off, "Flag" = s$Flag))
cc.all$large = abs(cc.all$d18O.off) > 1 | abs(cc.all$d2H.off) > 8

tapply(cc.all$Flag, cc.all$large, flagged)

write.csv(p, "out/plants.csv", row.names = FALSE)
write.csv(s, "out/soils.csv", row.names = FALSE)
