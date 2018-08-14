# for use with lsm source code version 203

Sys.setenv(TZ = "UTC")
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(xts))

t0  <- "2017-10-10 00"
t1  <- "2018-03-01 00"
dt  <- 1800
loc <- list(lon = -8.876, lat = 41.839, height = 30)

layer <- 1

t_range <- ymd_h(t0, t1)
t_res   <- dt
timestamps <- seq.POSIXt(t_range[1], t_range[2], by = t_res)

source("functions.R")

# set up temporary folder for running the lsm model
tmp <- "tmp/"
unlink(tmp, recursive = TRUE)
dir.create(tmp)

forcing <- str_c(tmp, "forcing.in")
forcing.file(loc, t_range, t_res, forcing)

lsm <- compile.lsm(tmp, layer = layer)
out <- system(str_c("cd ", tmp, "; ./", lsm), intern = TRUE, ignore.stderr = TRUE)
t <- tibble(time = timestamps, temp = as.numeric(out) - 273.15)

unlink(tmp, recursive = TRUE)

ggplot(filter(t, time < ymd_h("2017-10-20 00"))) +
  geom_line(aes(time, temp)) + 
  xlab("") + ylab("")
