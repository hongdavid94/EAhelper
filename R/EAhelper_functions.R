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

# 1. general

print(paste0("libPaths are: ", paste0(.libPaths(), collapse = " | ")))

library(dplyr)
library(stringr)
library(haven, include.only = 'read_sas')
library(rlist, include.only = 'list.clean')
library(survival)
library(kableExtra)
library(ggplot2)
library(extrafont)
library(XML, include.only = 'readHTMLTable')

# R.version
# 4.2.1

# get library versions
# sessionInfo()

# import fonts
# font_import()

# load fonts
loadfonts(device = "win")

# basic functions
`%!in%` <- Negate(`%in%`)
not.na <- Negate(is.na)
not.null <- Negate(is.null)
not.infinite <- Negate(is.infinite)


fn_measure_time <- function(time_pre, title = "", units = "secs") {
  if (units %!in% c("secs", "mins")) {
    stop("Error: parameter <units> must be secs or mins")
  }
  print(paste0(title, " took ", (Sys.time() - time_pre) %>% as.numeric(units = units), " ", units))
}

str_split_df <- function(string, pattern) {
  plyr::ldply(stringr::str_split(string, pattern), rbind)
}

fn_check_dir <- function(path) {
  dir.create(path = path, showWarnings = F, recursive = T)
}

not.empty <- function(x) {not.na(x) & x != ""}

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

# function for printing progression of
fn_print_progress <- function(i_prog_4234, i_total_4234) {
  cat("\r", i_prog_4234 / i_total_4234)
}

fn_count_total <- function(i_total_4234) {
  i_total_4234 <<- i_total_4234
}

fn_count_progress <- function(i_prog_4234) {
  i_total_4234 <<- i_total_4234
}

fn_timer_start <- function() {
  time_start_4234 <<- Sys.time()
}

fn_timer_end <- function(name_job = "Job", unit_job = "mins") {
  if(time_start_4234 %>% is.null) {print("timer had not started")}
  paste(name_job, "ended after", difftime(Sys.time(), time_start_4234, units = unit_job) %>% round(2), unit_job) %>% print
}

# return n characters of x from left
fn_left <- function(x, n) {
  return(substr(x = x, start = 1, stop = n))
}

# return n characters of x from right
fn_right <- function(x, n) {
  return(substr(x = x, start = nchar(x) - n + 1, stop = nchar(x)))
}

# detach all packages
fn_detach.all <- function() {
  # take out basic packages that won't be detached
  basic.packages <- c("package:stats", "package:graphics",
                      "package:grDevices","package:utils",
                      "package:datasets","package:methods",
                      "package:base")

  # search() gives packages and R objects
  # subset a vector of packages from base::search()
  package.list <- base::search()[ifelse(unlist(gregexpr("package:", base::search()))==1,TRUE,FALSE)]

  # take out basic packages
  package.list <- setdiff(package.list,basic.packages)

  # for-loop to detach all packages (except for basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)

}

# Setting for kable
mykable <- function(temp,
                    align = "c", position = "center",
                    mytitle = "",
                    fullwidth = T) {
  temp %>% kable(align = paste(rep(align, ncol(temp), caption = mytitle),
                               collapse = "")) %>% kable_styling(full_width = fullwidth, position = position)}

# theme for ggplot
theme_font <- function(font = "Times New Roman") {
  theme(text = element_text(family = font),
        title = element_text(family = font),
        axis.title = element_text(family = font),
        axis.text = element_text(family = font),
        legend.text = element_text(family = font))
}

# 2. EA
col_junk <- c("userid", "projectid", "project", "studyid", "environmentName",
              "subjectId", "StudySiteId", "SDVTier", "siteid",
              "Site", "SiteNumber", "SiteGroup", "instanceId",
              "InstanceRepeatNumber", "folderid", "Folder", "FolderName",
              "FolderSeq", "TargetDays", "DataPageId", "DataPageName",
              "PageRepeatNumber", "RecordDate", "RecordId", "RecordPosition",
              "MinCreated", "MaxUpdated", "SaveTS", "StudyEnvSiteNumber", "RecordActive", "FORM_OID")

# Theme for ggplot
mytheme <- function() {
  theme_bw() %+replace%
    theme(plot.title = element_text(size = text_size_title),
          axis.title = element_text(size = text_size_axis),
          axis.text.x = element_text(size = text_size_axis),
          axis.text.y = element_text(size = text_size_axis),
          legend.title = element_text(size = 0),
          legend.text = element_text(size = text_size_axis),
          strip.text.y = element_text(size = text_size_axis,color="black",
                                      face = "bold",angle = 0))}

#![Sketch of Concept](Z:/EA/Concept/Katona/data/flowchart.PNG)

# extract digits inside parentheses
fn_ex_digits <- function(x) {stringr::str_extract(string = x, pattern = "(?<=\\().*(?=\\))")}

# read sas7bdat file
# "path" is a string form of the protocol
# "file" is a string form of the file name (sas7bdat)
# "path" is an optional file path to overwrite the default rave database path. By default, it is ignored.
fn_readsas <- function(file, file_cat = NULL, path = "ignored", prot = NULL, exclude_junk = T) {
  if (path != "ignored") {
    temp <- read_sas(data_file = paste0(path, "/", file, ".sas7bdat"),
                     catalog_file = file_cat)
  }

  else if (!is.null(prot)) {
    temp <- read_sas(data_file = paste0("E:/Prots/", prot, "/Data/Raw/", file, ".sas7bdat"),
                     catalog_file = file_cat)
  }

  else {
    temp <- read_sas(data_file = paste0("E:/Prots/", prot_global, "/Data/Std/", file, ".sas7bdat"),
                     catalog_file = file_cat)
  }
  if (exclude_junk) {
    temp %>%
      select_if(colnames(temp) %!in% col_junk) %>%
      return()
  }
  else {
    temp %>% return()
  }
}

fn_read_vtstat <- function(prot, path = NULL) {
  if (path %>% is.null) {
    temp <- read_sas("E:/Survival/Data/vtstat.sas7bdat")
  }

  else {
    temp <- read_sas(paste0(path, "vtstat.sas7bdat"))
  }

  temp %>% rename(PROT = prot) %>% filter(PROT == prot) %>% return()
}

# function to count how many rows fit the description for a specific column
# file: a string of sas7bdat file
# col: a string of column name of interest
# desc: a column entry of interest
# path: an optional file path to overwrite the default rave database path. By default, it is ignored.
fn_count_desc <- function(file, col, desc, path = "ignored", prot = NULL) {
  temp <- fn_readsas(prot = prot, file = file, path = path)
  temp %>%
    select("Subject", !!rlang::sym(col)) %>%
    filter(!!rlang::sym(col) == desc) %>%
    return()
}

# function to extract columns from a file
# path: specified path to file, default is ignored and uses rave dataset
# file: file name (takes in .sas7bdat files)
# col: character vector of column names
# include_subject: by default and when set to T, includes column "Subject"
fn_column <- function(path = "ignored", file, col, include_subject = T, prot = NULL, exclude_junk = T) {
  if (include_subject) {
    col <- c("Subject", col)
  }
  fn_readsas(file = file, path = path, prot = prot, exclude_junk = exclude_junk) %>%
    select(!!!syms(col)) %>%
    return()
}

# converts days into units of months
# days is a numeric that represent days
fn_days_to_month <- function(days) {
  return(days/365.25*12)
}

# function to return a dataframe of the table inside the html
fn_load_html_table <- function(html) {
  # reads html page
  temp <- readHTMLTable(html)
  # removes all Null items in the list
  temp <- list.clean(temp, fun = is.null, recursive = FALSE)
  # calculates the size of each table in html
  n.rows <- unlist(lapply(temp, function(t) dim(t)[1]))
  # returns the biggest table in the html page
  temp[[which.max(n.rows)]] %>% return()
}

# Brute force way to convert itype into character
fn_convert_itype <- function(vec_sample) {
  temp <- vec_sample %>% as.character()
  case_when(temp == 1  ~ "Main",
            temp == 2  ~ "NCORP",
            temp == 3  ~ "Affiliate",
            temp == 10 ~ "Minority/Underserved NCORP",
            temp == 11 ~ "Integrated Component",
            TRUE ~ temp) %>%
    return()
}

# returns a dataframe after mutating subset of dataframe that satisfies the input condition
fn_mutate_subset <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

# returns the p-value of log-rank test of survdiff object
fn_survdiff_p <- function(survdiff_sample, digit = 2) {
  round(1 - pchisq(survdiff_sample$chisq, length(survdiff_sample$n) - 1), digit)
}

# returns the data frame of percentage of rejecting the logrank test for each median PFS for treatment group
# for a given range of HR
fn_crit_bound <- function(B = 1e3, n, PFS_control, min_HR, max_HR, step_HR, alpha) {
  p_sim <- numeric(B)
  df_sim <- data.frame(
    status = rep(1, 2*n),
    arm = c(rep("C", n), rep("T", n)))
  range_HR <- seq(from = min_HR, to = max_HR, by = step_HR)
  df_reject <- as.data.frame(matrix(0, ncol = 2, nrow = range_HR %>% length))
  colnames(df_reject) <- c("HR", "Reject Probability")
  df_reject$HR <- range_HR
  count <- 1

  for (HR_i in range_HR) {
    for (i in 1:B) {
      df_sim$time = c(rexp(n, rate = log(2)/PFS_control), rexp(n, rate = log(2)/(PFS_control/HR_i)))
      p_sim[i] = survdiff(Surv(time, status) ~ arm, data = df_sim) %>% fn_survdiff_p(digit = 3)
    }
    df_reject[count, 2] <- sum(p_sim <= alpha)/B
    count <- count + 1
  }
  return(df_reject)
}

fn_coxph_CI <- function(coxph, CI = 0.95) {
  exp(summary(coxph)$coefficients[1, 1] + summary(coxph)$coefficients[1, 3]*c(qnorm((1 - CI)/2), qnorm((1 - CI)/2 + CI)))
}

# returns the data frame of total sample size, accrual and follow-up time
fn_acc_table <- function(HR, power, acc_rate,
                         stat_control, alpha = 0.05,
                         acc_period_min = 10, acc_period_max = 120,
                         fu_min = NULL, fu_max = NULL) {
  if (fu_min %>% is.null) {
    fu_min <- floor(stat_control/2)
    fu_max <- stat_control*2
  }
  count <- 1
  df <- as.data.frame(matrix(0, ncol = 4, nrow = seq(fu_min, fu_max) %>% length))
  for (f_i in seq(fu_min, fu_max)) {
    seqss_main <- seqss(power = power,
                        acc.per = c(acc_period_min, acc_period_max),
                        acc.rate = acc_rate,
                        add.fu = f_i,
                        anal.int = 10,
                        control.med = stat_control,
                        pct.imp = 100/HR - 100,
                        alpha = alpha)
    acc_per_main <- seqss_main$acc.per

    df[count, ] <- c(acc_per_main %>% ceiling + f_i,
                     acc_per_main %>% ceiling,
                     f_i,
                     (acc_per_main %>% ceiling * acc_rate))
    count <- count + 1
  }
  colnames(df) <- c("Total_Time", "Accrual_Time", "Follow_Up", "Sample_Size")
  list_temp <- list()
  list_temp[["df"]] <- df
  return(list_temp)
}

# reads all the sas
fn_create_list_col <- function(path_data) {
  fn_timer_start()
  paste0("Starting reading sas7bdat files from ", path_data) %>% print
  list_col <- list()
  count_file <- 0
  count_sas <- 0
  for (name_file in (list.files(path = path_data) %>% str_extract(".*(?=.sas7bdat)"))) {
    if (name_file %>% not.na) {
      list_col[[name_file]] <- fn_readsas(file = name_file, path = path_data) %>% colnames
      count_sas = count_sas + 1
    }
    count_file = count_file + 1
  }
  paste0("found ", count_file, " files of which ", count_sas, " files are sas7bdat") %>% print
  paste0("ending reading sas7bdat files from ", path_data) %>% print
  fn_timer_end()
  list_col %>% return()
}

# CTEP organization codes and country
fn_load_org_ctep <- function() {
  df_org_ctep <- readHTMLTable(doc = "file:///Z:/EA/OrganizationInfo.html", header = T)
  df_org_ctep <- df_org_ctep$`NULL`
  return(df_org_ctep)
}

# round up number to be multiple of x
fn_round_mult <- function(x, digit) {
  x_ceil <- x %>% ceiling
  carry <- x_ceil %% digit
  return(ifelse(carry == 0, x_ceil, x_ceil + digit - carry))
}

# convert all *.sas7bdat files to df_* dataframes
fn_import_allsas <- function(path_sas) {
  now = Sys.time()

  vec_files <- list.files(path = path_sas)
  vec_subset <- vec_files[vec_files %>% str_detect("sas7bdat$")] %>% str_remove(pattern = ".sas7bdat")
  n_file <- vec_subset %>% length

  index_progress = 1
  for (file in vec_subset) {
    print(paste0("importing ", file))
    assign(paste0("df_", file),
           fn_readsas(
             file = file, path = path_sas),
           envir = .GlobalEnv)

    print(paste0("finished importing ", file, " / ", round(index_progress/n_file * 100, 2), " % done"))
    index_progress = index_progress + 1
  }
  time_compute <- Sys.time() - now
  units(time_compute) <- "mins"
  print(paste0("imported all ", n_file, "files, took ", time_compute %>% round(2), " mins"))
}

# extracts all Subjects as numeric vector
fn_subject <- function(df){
  df %>%
    select(Subject) %>%
    unlist() %>%
    as.numeric()
}
