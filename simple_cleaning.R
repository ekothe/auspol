library(dplyr)
library(stringr)
bound <- bind_rows(df_2019, df_2016,df_2013)

bound <- select(bound, Date, Firm, Labor_2PP, LNP_2PP, Labor, LNP, ONP, Other, SampleSize, Margin, Method)

bound <- bound %>% 
  filter(!is.na(Firm), 
         !is.na(Labor_2PP), 
         Firm != "2013 election") %>% 
  mutate(Labor_2PP = readr::parse_number(Labor_2PP),
         LNP_2PP = readr::parse_number(LNP_2PP),
         Labor = readr::parse_number(Labor),
         LNP = readr::parse_number(LNP),
         ONP = readr::parse_number(ONP),
         Other = readr::parse_number(Other),
         SampleSize = stringr::str_replace(SampleSize, "\\n", ""),
         SampleSize = readr::parse_number(stringr::str_replace_all(SampleSize, " ", "")),
         Firm = stringr::str_replace(Firm, "\\[.*\\]", ""),
         Firm = stringr::str_replace(Firm, "\\(.*\\)", ""))

dates <- bound$Date

dates <- tibble(dates = dates) %>%
  mutate(end_year = str_extract(dates, "[0-9]*$"),
         end_year = ifelse(str_length(end_year) == 2, paste0("20", end_year), end_year),
         month_one = str_extract(dates, "[A-Z][a-z][a-z]"),
         month_two = str_sub(str_extract(dates, "[A-Z][a-z][a-z].*[A-Z][a-z][a-z]"), start = -3),
         month_two = if_else(is.na(month_two), month_one, month_two),
         day_one = str_extract(dates, "[0-9]+"),
         dates_without_day_one = gsub("^[0-9]+", "", dates),
         day_two = str_extract(dates_without_day_one, "[0-9]+"),
         day_two = str_squish(gsub("[-–]", "", day_two)),
         day_three_four = str_extract(dates, "/.+[-–] *[0-9]+"),
         day_three = str_extract(day_three_four, "/ *[0-9]+"),
         day_three = str_squish(gsub("/", "", day_three)),
         day_four = str_extract(day_three_four, "[-–] *[0-9]+"),
         day_four = str_squish(gsub("[-–]", "", day_four))
  ) %>%
  # dates that are only a single day:
  mutate(day_two = if_else(is.na(day_two), day_one, day_two)) %>%
  # dates that actually have four days:
  mutate(day_one = ifelse(is.na(day_three),
                          day_one,
                          round((as.numeric(day_one) + as.numeric(day_two)) / 2)),
         day_two = ifelse(is.na(day_three),
                          day_two,
                          round((as.numeric(day_three) + as.numeric(day_four)) / 2))) %>%
  select(-day_three_four, -dates_without_day_one) %>%
  
  mutate(start_date = as.Date(paste(end_year,month_one, day_one, sep = "-"), format = "%Y-%b-%d"),
         end_date = as.Date(paste(end_year,month_two, day_two, sep = "-"), format = "%Y-%b-%d"),
         Date = dates) %>%
  select(Date, start_date, end_date, everything())

auspol <- cbind(bound, dates)

readr::write_csv(auspol, "auspol.csv")
