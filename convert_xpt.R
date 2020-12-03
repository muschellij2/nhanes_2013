library(haven)
x = list.files(pattern = "[.]XPT$", path = here::here("data"),
               full.names = TRUE)
x = x[order(file.size(x), decreasing = FALSE)]

for (i in x) {
  print(i)
  outfile = sub("[.]XPT$", ".rds", i)
  if (!file.exists(outfile)) {
    df = haven::read_xpt(i)
    message("data read in")
    readr::write_rds(df, outfile)
  }
}
