library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(hms)
library(lubridate)
setwd(here::here())
wave = c("G" = 2011, "H" = 2013)
x = list.files(pattern = ".(rds|RDS)$", path = here::here("data"), 
               full.names = FALSE)
x = sub("[.]RDS", "", toupper(x))
x = sub("_GOOD$", "", x)
x = sub("_(H|G)$", "", x)
stubs = unique(x)

stub = stubs[1]
# stub = "PAXDAY" 

for (stub in stubs) {
  print(stub)
  outfile = here::here("summarized_data", paste0(stub, ".rds"))
  
  if (!file.exists(outfile)) {
    
    all_data = lapply(names(wave), function(w) {
      year = wave[[w]]
      file = paste0(stub, "_", w, ".rds")
      file = here::here("data", file)
      if (!file.exists(file)) {
        return(NULL)
      }
      x = readr::read_rds(file)
      x$year = year
      attr(x$year, "label") = "Wave of NHANES"
      x
    })
    labs = lapply(all_data[[1]], attr, "label")
    cols = colnames(all_data[[1]])
    
    data = dplyr::bind_rows(all_data)
    
    n_id = length(unique(data$SEQN))
    
    q10 = function(r) {
      r = na.omit(r)
      qa = quantile(r, probs = seq(0, 1, by = 0.1))
    }
    cvars = colnames(data)
    cvars = cvars[!cvars %in% c("SEQN", "year")]
    
    results = vector(mode = "list", length = length(cvars))
    names(results) = cvars
    
    
    rounder = function(x, ...) {
      if (is.numeric(x)) {
        x = round(x, ...)
      }
      x
    }
    hister = function(x, breaks = 200) {
      if (all(is.na(x))) {
        df = data.frame(
          x = unique(x),
          n = length(x))
      } else {
        h = hist(x, breaks = breaks, plot = FALSE)
        df = data.frame(x = h$mids,
                        n = h$counts)
      }
      df
    }
    ivar = cvars[2]
    
    for (ivar in cvars) {
      print(paste0("   ", ivar))
      d = data[, c(ivar, "year")]
      colnames(d)[1] = "x"
      dt = FALSE
      if (hms::is_hms(d$x)) {
        d$x = as.character(d$x)
      }
      if (!is.character(d$x) & !is.logical(d$x)) {
        ss = split(d, d$year)
        if (all(is.na(d$x))) {
          h = list(breaks=200)
        } else {
          h = hist(d$x, breaks = 200, plot = FALSE)
        }
        out = lapply(ss, function(x) hister(x$x, breaks = h$breaks))
        out = dplyr::bind_rows(out, .id = "year") %>% 
          mutate(year = as.numeric(year))
      } else {
        out = d %>% 
          count(x, year) %>% 
          ungroup()
      }
      out  = out[, c("year", "n", "x")]
      out = tibble::as_tibble(out)
      attr(out, "label") = labs[[ivar]]
      results[[ivar]] = out
    }
    
    readr::write_rds(results, outfile)
  }
  
}
