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
  # Get cf file name from cc file
  cfn = as.character(read.xlsx(fl[i], "Summary", rows = 2, colNames = FALSE))
  
  # Read data from cc file
  d = read.xlsx(fl[i], "Summary", startRow = 4, cols = 3)
  names(d) = "ID"
  d$RunDate = rep(substr(cfn, 1, 6))
  d$Instrument = rep(substr(cfn, nchar(cfn) - 7, nchar(cfn)))
  
  # Pull relevant cells from cc file 
  cells = xlsx_cells(fl[i], sheets = "Summary")
  cells = cells[cells$col == 3, ]
  cells = cells[-(1:3), ]
  
  # Get formats from cc file
  f = xlsx_formats(fl[i])
  form = f$local$fill$patternFill$bgColor
  
  # Red cells
  rc = which(f$local$fill$patternFill$bgColor$rgb == "FF993300")
  red = cells$local_format_id %in% rc
  
  # Yellow cells
  yc = which(f$local$fill$patternFill$bgColor$rgb == "FFFFFF00")
  yell = cells$local_format_id %in% yc
  
  d$Flag = rep(0)
  d$Flag[yell] = 1
  d$Flag[red] = 2
  
  cc = rbind(cc, d)
}
