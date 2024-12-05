#####
# Identify CRDS coordinator files and pull into project
# Not portable
#####

# Read data
p = read.csv("out/plants.csv")

# Get list of job numbers
sid = strsplit(p$Sample_ID, "_")
sid = sapply(sid, "[[", 1)
sid = unique(sid)

# Directories 
fp = "~/../Dropbox"

# Find runfiles files from jobnumbers
cf = rf = character()
for(i in 1:length(sid)){
  rf = append(rf, list.files(file.path(fp, "hids2046", "runfiles"),
                             sid[i], full.names = TRUE))
  rf = append(rf, list.files(file.path(fp, "hids2052", "runfiles"), 
                             sid[i], full.names = TRUE))
}

# Parse out the directory and runfile name
fstring = strsplit(rf, "/")
fpath = sapply(fstring, "[[", 7)
fname = sapply(fstring, "[[", 9)

# From runfile name get date string
fdate = substr(fname, 1, 6)

# Find all coordinator files
for(i in seq_along(fdate)){
  cfs = list.files(file.path(fp, fpath[i]), fdate[i], full.names = TRUE)
  cf = append(cf, cfs[length(cfs)])
}

# Copy to project
for(i in seq_along(rf)){
  file.copy(rf[i], file.path("data", "rf", fname[i]))
  file.copy(cf[i], file.path("data", "cf", fname[i]))
}
