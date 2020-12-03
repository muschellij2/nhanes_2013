library(readr)
library(dplyr)
library(haven)
library(hms)
library(lubridate)
library(ggplot2)
wave = "H"

outfile = paste0("data/PAXMIN_", wave, "_good.rds")

if (!file.exists(outfile)) {
  xhdr = hdr = read_rds(paste0("data/PAXHD_", wave, ".rds"))
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
  
  df = read_rds(paste0("data/PAXMIN_", wave, ".rds"))
  
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
demog = read_rds(paste0("data/DEMO_", wave, ".rds") )
demog = demog %>% 
  select(SEQN, gender = RIAGENDR, 
         age_screening = RIDAGEYR,
         age_exam = RIDEXAGY,
         race = RIDRETH1) %>% 
  mutate(white = race == 3,
         male = gender == 1) %>% 
  select(-gender) %>% 
  mutate(age_cat = cut(age_screening, 
                       breaks=c(0, 18, 44, 64, 80), 
                       include.lowest = TRUE))



bmi = read_rds(paste0("data/BMX_", wave, ".rds") ) %>% 
  select(SEQN, bmi = BMXBMI)
range_bmi = range(bmi, na.rm = TRUE)
bmi = bmi %>% 
  mutate(bmi_cat = cut(bmi, 
                       breaks = c(range_bmi[1], 18.5, 24.9, 
                                  29.9, range_bmi[2]),
                       include.lowest = TRUE))


daily = df %>% 
  group_by(SEQN, time) %>% 
  summarise(mims = mean(PAXMTSM, na.rm = TRUE),
            median = median(PAXMTSM, na.rm = TRUE),
            n = sum(!is.na(PAXMTSM)))

daily = daily %>% left_join(bmi)
daily = daily %>% left_join(demog)


avg = daily %>% 
  group_by(time) %>% 
  summarise(
    med = median(mims, na.rm = TRUE),
    mims = mean(mims, na.rm = TRUE))


avg %>% 
  ggplot(aes(x = time, y = med)) + 
  geom_line() 


age = daily %>% 
  group_by(time, age_cat) %>% 
  summarise(
    med = median(mims, na.rm = TRUE),
    mims = mean(mims, na.rm = TRUE))
age %>% 
  ungroup() %>% 
  ggplot(aes(x = time, y = med, colour = age_cat) )+ 
  geom_line() 


bmi_cat = daily %>% 
  filter(age_screening >= 18 & !is.na(bmi_cat)) %>% 
  group_by(time, bmi_cat) %>% 
  summarise(
    med = median(mims, na.rm = TRUE),
    n_ids = length(unique(SEQN)),
    mims = mean(mims, na.rm = TRUE))
bmi_cat %>% 
  filter(n_ids > 50) %>% 
  ungroup() %>% 
  ggplot(aes(x = time, y = mims, colour = bmi_cat) )+ 
  geom_line() 

