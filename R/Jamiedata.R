# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Clean IRI
#' @export
#' @param data csv file with the PMIS data
#' @param dif upper percentile threshold used to eliminate spikes
#' @param noise percentile used to determine the level of sensor noise

iri_cleaner <- function(data, dif, noise) {

  df <- data %>% select(`District Name`, station, year, county, highway, ptyp, ds, cs, irir, iril) %>% arrange(station, year) %>% filter(ptyp < 7 | ptyp >8)

  df <- df %>% mutate(f_iril = ifelse(station == lead(station, n = 1), iril-lead(iril, n=1), 0),
                      b_iril = ifelse(station == lag(station, n = 1), iril-lag(iril, n=1), 0),
                      f_irir = ifelse(station == lead(station, n = 1), irir-lead(irir, n=1), 0),
                      b_irir = ifelse(station == lag(station, n = 1), irir-lag(irir, n=1), 0),)

  df$f_iril[is.na(df$f_iril)] <- 0; df$f_irir[is.na(df$f_irir)] <- 0
  df$b_iril[is.na(df$b_iril)] <- 0; df$b_irir[is.na(df$b_irir)] <- 0


  t1 <- quantile(df$f_iril, probs = dif); t2 <- quantile(df$f_iril, probs = 1-dif)
  t3 <- quantile(df$f_irir, probs = dif); t4 <- quantile(df$f_irir, probs = 1-dif)


  df <- df %>% mutate(c_iril = ifelse(( (f_iril>t1 & b_iril>t1) | (f_iril<t2 & b_iril<t2) ), NA, iril),
                      c_irir = ifelse(( (f_irir>t3 & b_irir>t3) | (f_irir<t4 & b_irir<t4) ), NA, irir))

  cat(paste(sum(is.na(df$c_iril)), "spikes removed for iril using a threshold of +/-", t1 , "delta iril \n",
            sum(is.na(df$c_irir)), "spikes removed for irir using a threshold of +/-", t3 , "delta irir \n\n"))

  df$c_iril <- round(na_interpolation(x = df$c_iril, option = "linear"), digits = 0)
  df$c_irir <- round(na_interpolation(x = df$c_irir, option = "linear"), digits = 0)

  df <- df %>% mutate(f_iril = ifelse(station == lead(station, n = 1), c_iril-lead(c_iril, n=1), 0),
                      b_iril = ifelse(station == lag(station, n = 1), c_iril-lag(c_iril, n=1), 0),
                      f_irir = ifelse(station == lead(station, n = 1), c_irir-lead(c_irir, n=1), 0),
                      b_irir = ifelse(station == lag(station, n = 1), c_irir-lag(c_irir, n=1), 0),)


  df$f_iril[is.na(df$f_iril)] <- 0; df$f_irir[is.na(df$f_irir)] <- 0
  df$b_iril[is.na(df$b_iril)] <- 0; df$b_irir[is.na(df$b_irir)] <- 0

  nt1 <- quantile(df$b_iril, probs = 1-noise); nt2 <- quantile(df$b_irir, probs = 1-noise)

  cat(paste("noise threshold is at", nt1 , "delta iril \n",
            "noise threshold is at", nt2 , "delta irir \n"))


  df <- df %>% mutate(cc_iril = ifelse((b_iril>nt1 & b_iril<0), lag(c_iril, n = 1), c_iril),
                      cc_irir = ifelse((b_irir>nt2 & b_irir<0), lag(c_irir, n = 1), c_irir))

  df <- df %>% mutate(cc_iril = ifelse((b_iril>nt1 & b_iril<0), lag(cc_iril, n = 1), c_iril),
                      cc_irir = ifelse((b_irir>nt2 & b_irir<0), lag(cc_irir, n = 1), c_irir))



  database <- df %>% select(year, highway, ptyp, `District Name`, county, station, ds, cs, iril, cc_iril, irir, cc_irir)

  return(database)
}

#' Clean DS
#' @export
#' @param data csv file with the PMIS data
#' @param dif upper percentile threshold used to eliminate spikes
#' @param noise percentile used to determine the level of sensor noise
ds_cleaner <- function(data, dif, noise) {

  df <- data

  df$ds[is.na(df$ds)] <- 0;

  df <- df %>% mutate(f_ds = ifelse(station == lead(station, n = 1), ds-lead(ds, n=1), 0),
                      b_ds = ifelse(station == lag(station, n = 1), ds-lag(ds, n=1), 0))

  df$f_ds[is.na(df$f_ds)] <- 0; df$b_ds[is.na(df$b_ds)] <- 0



  t1 <- quantile(df$f_ds, probs = dif); t2 <- quantile(df$f_ds, probs = 1-dif)

  df <- df %>% mutate(c_ds = ifelse(( (f_ds>t1 & b_ds>t1) | (f_ds<t2 & b_ds<t2) ), NA, ds))

  cat(paste(sum(is.na(df$c_ds)), "spikes removed for ds using a threshold of +/-", t1 , "delta ds \n\n"))

  df$c_ds <- round(na_interpolation(x = df$c_ds, option = "linear"), digits = 0)

  df <- df %>% mutate(f_ds = ifelse(station == lead(station, n = 1), c_ds-lead(c_ds, n=1), 0),
                      b_ds = ifelse(station == lag(station, n = 1), c_ds-lag(c_ds, n=1), 0))

  df$f_ds[is.na(df$f_ds)] <- 0; df$b_ds[is.na(df$b_ds)] <- 0

  nt1 <- quantile(df$b_ds, probs = noise)

  cat(paste("noise threshold is at", nt1 , "delta ds \n"))


  df <- df %>% mutate(cc_ds = ifelse((b_ds<nt1 & b_ds>0), lag(c_ds, n = 1), c_ds))

  df <- df %>% mutate(cc_ds = ifelse((b_ds<nt1 & b_ds>0), lag(cc_ds, n = 1), c_ds))



  database <- df %>% select(year, highway, ptyp, `District Name`, county, station, ds, cc_ds, cs, iril, cc_iril, irir, cc_irir)

  return(database)
}


#' @import "tidyverse"
#' @import "imputeTS"
