library(readr)
library(dplyr)
library(haven)
library(hms)
library(lubridate)
library(ggplot2)
library(here)
wave = "H"

outfile = here::here("data", paste0("PAXMIN_", wave, "_good.rds"))

if (!file.exists(outfile)) {
  xhdr = hdr = read_rds(here::here("data", paste0("PAXHD_", wave, ".rds")))
  hdr = hdr %>% 
    filter(PAXSTS == 1) %>%
    mutate(n_days = as.numeric(PAXLDAY) - 1,
           PAXETLDY = hms(PAXETLDY),
           PAXFTIME = hms(PAXFTIME)) %>% 
    select(SEQN, PAXETLDY, PAXFTIME, n_days)
  
  hdr = hdr %>% 
    mutate(n_minutes = n_days * 24*60 + 
             (as.numeric(PAXETLDY) / 60 - as.numeric(PAXFTIME) /60),
           n_minutes = round(n_minutes))
  n_min = hdr %>% 
    select(SEQN, n_minutes)
  hdr = hdr %>% 
    select(SEQN, PAXFTIME)
  
  df = read_rds(here::here("data", paste0("PAXMIN_", wave, ".rds")))
  
  df = df %>% 
    select(SEQN, PAXDAYM, PAXDAYWM, PAXSSNMP,
           PAXAISMM, PAXMTSM, PAXPREDM, PAXQFM)
  
  # seconds to minutes
  df = df %>% 
    mutate(PAXSSNMP = PAXSSNMP / 80) # 80 hz, to seconds
  
  df = left_join(df, hdr)
  
  df = df %>% 
    mutate(time = as.numeric(PAXFTIME + PAXSSNMP) - 
             (as.numeric(PAXDAYM) - 1) * 1440*60,
           time = as_hms(time))
  
  df = df %>% 
    select(-PAXFTIME, -PAXSSNMP)
  
  df = df %>% 
    mutate(PAXMTSM = ifelse(PAXMTSM < 0, NA_real_, PAXMTSM))
  # df %>% 
  # count(PAXQFM)
  
  df = df %>% 
    mutate(PAXMTSM = if_else(PAXQFM > 0, NA_real_, PAXMTSM))
  
  df = df %>% 
    mutate(good = !is.na(PAXQFM) & (PAXPREDM != 3))
  
  
  # should we exclude REALLY high values?
  # df = df %>% 
  #   mutate(good = good & (PAXMTSM >= 100))
  
  # count the good data
  good = df %>% 
    group_by(SEQN, PAXDAYM) %>% 
    summarise(n_good = sum(good))
  # 95% cutoff
  good = good %>% 
    filter(n_good >= 0.95*1440)
  
  # use mutate because we want to keep 
  # the SEQN/day combinations
  good = good %>% 
    group_by(SEQN) %>% 
    mutate(n_good_days = n())
  
  good = good %>% 
    filter(n_good_days >= 3) %>% 
    select(SEQN, PAXDAYM)
  
  
  df = right_join(df, good)
  
  df = df %>% 
    arrange(SEQN, PAXDAYM, time)
  
  u_ids = unique(df$SEQN)
  n_ids = length(u_ids)
  
  write_rds(df, outfile)
  
} else {
  df = read_rds(outfile)
}

##############################################
# Now read stuff
##############################################
demog = read_rds(here::here("data", paste0("DEMO_", wave, ".rds") ))
demog = demog %>% 
  select(SEQN, gender = RIAGENDR, 
         age_screening = RIDAGEYR,
         race = RIDRETH1) %>% 
  mutate(white = race == 3,
         male = gender == 1) %>% 
  select(-gender) %>% 
  mutate(age_cat = cut(age_screening, 
                       breaks=c(0, 17.9, 44, 64, 79, 80), 
                       include.lowest = TRUE))



bmi = read_rds(here::here("data", paste0("BMX_", wave, ".rds") )) %>% 
  select(SEQN, bmi = BMXBMI)
range_bmi = range(bmi, na.rm = TRUE)
bmi = bmi %>% 
  mutate(bmi_cat = cut(bmi, 
                       breaks = c(range_bmi[1], 18.5, 24.9, 
                                  29.9, range_bmi[2]),
                       include.lowest = TRUE))

demog = full_join(demog, bmi)

daily = df %>% 
  group_by(SEQN, time) %>% 
  summarise(mims = mean(PAXMTSM, na.rm = TRUE),
            median = median(PAXMTSM, na.rm = TRUE),
            n = sum(!is.na(PAXMTSM)))

daily = daily %>% left_join(demog)




outdir = here::here("results", "")
save_plot = function(...) {
  ggsave(..., width = 7, height = 3.5)
}
avg = daily %>% 
  group_by(time) %>% 
  summarise(
    med = median(mims, na.rm = TRUE),
    mims = mean(mims, na.rm = TRUE))
g = avg %>% 
  ggplot(aes(x = time, y = med)) + 
  geom_line() 
save_plot(filename = paste0(outdir, "average_", wave, ".png"), plot = g)


make_plot = function(var, daily) {
  var = enquo(var)
  print(var)
  ivar = rlang::as_name(var)
  v = daily %>% 
    group_by(time, !!var) %>% 
    summarise(
      med = median(mims, na.rm = TRUE),
      mims = mean(mims, na.rm = TRUE))
  g = v %>% 
    ungroup() %>% 
    ggplot(aes(x = time, y = med, colour = !!var) )+ 
    geom_line() 
  save_plot(
    filename = paste0(outdir, "median_", ivar, "_", wave, ".png"), 
    plot = g)
  return(g)
}

g = make_plot(age_cat, daily)

age = daily %>% 
  group_by(time, age_cat) %>% 
  summarise(
    med = median(mims, na.rm = TRUE),
    mims = mean(mims, na.rm = TRUE))
g = age %>% 
  filter(age_cat != "[0,17.9]") %>% 
  ungroup() %>% 
  ggplot(aes(x = time, y = med, colour = age_cat) )+ 
  geom_line() 
save_plot(filename = paste0(outdir, "median_age_cat_adults_", wave, ".png"), plot = g)

daily = daily %>% 
  filter(age_screening >= 18)

g = make_plot(bmi_cat, daily = daily %>% filter(!is.na(bmi_cat)))
g = make_plot(white, daily = daily)
g = make_plot(male, daily = daily)

avg = daily %>% 
  group_by(time, male, age_cat) %>% 
  summarise(
    med = median(mims, na.rm = TRUE),
    mims = mean(mims, na.rm = TRUE))
g = avg %>% 
  ungroup() %>% 
  ggplot(aes(x = time, y = med, colour = age_cat) )+ 
  facet_wrap(~ male) + 
  geom_line() 
save_plot(
  filename = paste0(outdir, "median_sex_age", "_", wave, ".png"), 
  plot = g)
