{
  setwd("~/R/datiPorti/R")

  library(glue)
  library(tidyverse)
  library(lubridate)
  library(stringr)

  library(devtools)
  library(roxygen2)
  library(devtools)
}

porto <- "BARI"

hr <- grep(list.files(pattern = glue("^{porto}"), recursive = TRUE, full.names = TRUE),
           pattern = 'hr', invert = FALSE, value = TRUE)

nhr <- grep(list.files(pattern = glue("^{porto}"), recursive = TRUE, full.names = TRUE),
            pattern = 'hr', invert = TRUE, value = TRUE)

# estrae l'inquinante dal nome file (è sempre il terzo elemento)
get_pltnt <- function(x) {
  return(str_split(gsub(".txt", "", basename(x), ), pattern = "_")[[1]][3])
}
# map(nhr, get_pltnt)
# map(hr, get_pltnt)

# hr ####
campi <-
  c(
    "year",
    "month",
    "day",
    "hour",
    "terminal",
    "Emiss_HOT",
    "Emiss_MAN",
    "Emiss_tug",
    "Emiss_Cter",
    "Emiss_Cext"
  )

lapply(hr, function(x) {
       read_table(x,
         col_names = campi,
         skip = 2,
         col_types = cols(
           year = col_integer(),
           month = col_integer(),
           day = col_integer(),
           hour = col_integer(),
           terminal = col_integer(),
           Emiss_HOT = col_double(),
           Emiss_MAN = col_double(),
           Emiss_tug = col_double(),
           Emiss_Cter = col_double(),
           Emiss_Cext = col_double()
         )
       )
}) -> dfs_hr

names(dfs_hr) <-  map(hr, get_pltnt) %>% unlist()

do.call(rbind, dfs_hr) %>%
  rownames_to_column(var = "pltnt") %>%
  mutate(
    data = as.POSIXct(
      paste0(year, "/", str_pad(month, 2, pad = "0"), "/", str_pad(day, 2, pad = "0"), " ", str_pad(hour, 2, pad = "0"), ':00'),
      format = "%Y/%m/%d %H:%M"
    ),
    pltnt = gsub('\\.[[:digit:]]+', '', pltnt)
  ) %>% select(-c(year, month, day, hour)) -> df_hr


# non hr ####
campi_nhr <-
  c(
    "irec",
    "gt",
    "terminal",
    "Ship_categ",
    "ntug",
    "sep",
    "Emiss_HOT_MDGO",
    "Emiss_MAN_MDGO",
    "Emiss_tug_MDGO",
    "Emiss_Cter_MDGO",
    "Emiss_Cext_MDGO",
    "Emiss_HOT_BFO",
    "Emiss_MAN_BFO",
    "Emiss_tug_BFO",
    "Emiss_Cter_BFO",
    "Emiss_Cext_BFO"
  )


lapply(nhr, function(x) {
  read_fwf(x,
           col_positions = fwf_empty(x, skip = 6, col_names = campi_nhr),
           skip = 5,
  )
}) -> dfs_nhr

names(dfs_nhr) <-  map(nhr, get_pltnt) %>% unlist()

do.call(rbind, dfs_nhr) %>%
  rownames_to_column(var = "pltnt") %>%
  mutate(pltnt = gsub('\\.[[:digit:]]+', '', pltnt)) -> df_nhr
