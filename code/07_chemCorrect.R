# Parse the evil ChemCorrect files to extract flags
# Files were created using instruction set chemcorrect_inst avg_orgeval_10.csv
# Then opened and saved as .xslx

library(openxlsx)
library(tidyxl)

# Read data
p = read.csv("out/plants.csv")
s = read.csv("out/soils.csv")

# ChemCorrect files
fl = list.files("data/cc/", full.names = TRUE, pattern = ".xlsx")

# Space to store results
cc = data.frame("ID" = character(), "RunDate" = character(), 
                "Instrument" = character(), "Flag" = integer())

# Cycle through files
for(i in seq_along(fl)){
  ## Get cf file name from cc file
  cfn = as.character(read.xlsx(fl[i], "Summary", rows = 2, colNames = FALSE))
  
  ## Read data from cc file
  d = read.xlsx(fl[i], "Summary", startRow = 4, cols = 3)
  names(d) = "ID"
  d$RunDate = rep(substr(cfn, 1, 6))
  d$Instrument = rep(substr(cfn, nchar(cfn) - 7, nchar(cfn)))
  
  ## Pull relevant cells from cc file 
  cells = xlsx_cells(fl[i], sheets = "Summary")
  cells = cells[cells$col == 3, ]
  cells = cells[-(1:3), ]
  
  ## Get formats from cc file
  f = xlsx_formats(fl[i])
  form = f$local$fill$patternFill$bgColor
  
  ## Red cells
  rc = which(f$local$fill$patternFill$bgColor$rgb == "FF993300")
  red = cells$local_format_id %in% rc
  
  ## Yellow cells
  yc = which(f$local$fill$patternFill$bgColor$rgb == "FFFFFF00")
  yell = cells$local_format_id %in% yc
  
  ## Store flag status: green = 0, yellow = 1, red = 2
  d$Flag = rep(0)
  d$Flag[yell] = 1
  d$Flag[red] = 2
  
  cc = rbind(cc, d)
}

# Condense info per sample...choose highest flag status
ID = unique(cc$ID)
Flag = integer(length(ID))

for(i in seq_along(ID)){
  Flag[i] = max(cc$Flag[cc$ID == ID[i]])
}

# Merge with plant and soil data
## Make space
cc = data.frame(ID, Flag)

## Get plant sample names and match 
pID = substr(p$Sample_ID, 8, nchar(p$Sample_ID))
pind = match(pID, cc$ID)
p$Flag = cc$Flag[pind]

## Get soil sample names and match 
sID = substr(s$Sample_ID, 8, nchar(s$Sample_ID))
sind = match(sID, cc$ID)
s$Flag = cc$Flag[sind]

# Summarize
## Total number and fraction of flagged samples
rbind(table(p$Flag), table(p$Flag) / nrow(p))
rbind(table(s$Flag), table(s$Flag) / nrow(s))
rbind(table(c(p$Flag, s$Flag)), table(c(p$Flag, s$Flag)) / (nrow(p) + nrow(s)))

## 
exceed = function(x, y){
  c(sum(x > y), sum(x > y) / length(x))
}

tapply(p$d18O.off, p$Flag, exceed, 1)
tapply(p$d2H.off, p$Flag, exceed, 8)

tapply(s$d18O.off, s$Flag, exceed, 1)
tapply(s$d2H.off, s$Flag, exceed, 8)

cc.all = data.frame("d18O.off" = p$d18O.off, "d2H.off" = p$d2H.off, 
                    "Flag" = p$Flag)
cc.all = rbind(cc.all, data.frame("d18O.off" = s$d18O.off, 
                                  "d2H.off" = s$d2H.off, "Flag" = s$Flag))

tapply(cc.all$d18O.off, cc.all$Flag, exceed, 1)
tapply(cc.all$d2H.off, cc.all$Flag, exceed, 8)
