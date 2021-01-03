library(RCurl)
library(dplyr)
outdir = here::here("data", "mortality")
url = "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/"
res = getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
res <- strsplit(res, "\n")
res = unlist(res)
filenames = res[grepl("[.](dat|R|sas|do)$", res, ignore.case = TRUE)]

filename = filenames[1]
for (filename in filenames) {
  destfile =     file.path(outdir, filename)
  if (!file.exists(destfile)) {
    download.file(
      paste0(url, filename), 
      destfile,
      mode = "wb")
  }
}
