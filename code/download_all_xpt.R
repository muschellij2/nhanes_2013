library(httr)
library(rvest)
library(xml2)
library(dplyr)
library(curl)
library(tibble)
library(tidyr)
data_dir = here::here("data")

components = c("Examination", "Questionnaire", "Laboratory", "Dietary", 
               "Demographics")
years = c(2011, 2013)
year = years[1]
eg = expand.grid(component = components, year = years, 
                 stringsAsFactors = FALSE) %>% 
  mutate(stub = paste0(component, "_", year))

all_urls = paste(
  "https://wwwn.cdc.gov/nchs/nhanes/Search/DataPage.aspx?Component=", eg$component, 
  "&CycleBeginYear=", eg$year, sep = "")

base_url = "https://wwwn.cdc.gov"
url = "https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/"


results = lapply(all_urls, function(run_url) {
  out = read_html(run_url)
  
  tables <- xml2::xml_find_all(out, ".//table")
  hrefs = xml_find_all(tables, "//a")
  hrefs = xml_attr(hrefs, "href")
  hrefs = hrefs[grepl("(XPT|xpt)$", basename(hrefs))]
  tab = html_table(out)
  tab = tab[[1]]
  hrefs = paste0(base_url, hrefs)
  list(table = tab, url = hrefs)
})

tab = lapply(results, function(x) x$table)
names(tab) = eg$stub
tab = bind_rows(tab, .id = "stub")
tab = as_tibble(tab)
tab = tab %>% 
  separate(stub, into =c("component", "year"), sep= "_") %>% 
  mutate(year = as.numeric(year))
tab = tab %>% 
  mutate(name = trimws(sub("Data.*", "", `Data File`)),
         file = file.path(data_dir, paste0(name, ".XPT")),
         doc = paste0(name, ".htm"),
         file_exists = file.exists(file),
         url = paste0(base_url,"/Nchs/Nhanes/", year, "-", year + 1, "/", basename(file)))

tab = tab %>% 
  arrange(`Data File Name`)
tab = tab %>% 
  select(-`Doc File`) %>% 
  rename(description = `Data File Name`,
         file_size = `Data File` ,
         date_published = `Date Published`
         ) %>% 
  mutate(file_size = sub(".*XPT\\s*-\\s*(.*)\\]", "\\1", file_size)) %>% 
  mutate(doc_url = sub("[.]XPT$", ".htm", url))
readr::write_rds(tab, file.path(data_dir, "xpt_names.rds"))

all_urls = tab %>% 
  filter(!file_exists) %>% 
  pull(url)

out  = lapply(all_urls, function(url) {
  destfile = file.path(data_dir, basename(url))
  print(destfile)
  out = try({curl::curl_download(url = url, destfile = destfile, quiet = FALSE)},
            silent = TRUE)
  if (inherits(out, "try-error")) {
    url = sub("2011-2012", "1999-2000", url, fixed = TRUE)
    url = sub("2013-2014", "1999-2000", url, fixed = TRUE)
    out = try({curl::curl_download(url = url, destfile = destfile, quiet = FALSE)})
  }
})


out  = lapply(all_urls, function(url) {
  destfile = file.path(data_dir, basename(url))
  url = sub("[.]XPT", ".xpt", url)
  print(destfile)
  curl::curl_download(url = url, destfile = destfile, quiet = FALSE)
})

