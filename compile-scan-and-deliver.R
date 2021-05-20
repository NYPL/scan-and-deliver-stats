#!/usr/local/bin//Rscript --vanilla


# ------------------------------ #
rm(list=ls())

options(echo=TRUE)
options(width=80)
options(warn=2)
options(scipen=10)
options(datatable.prettyprint.char=50)
options(datatable.print.class=TRUE)
options(datatable.print.keys=TRUE)
options(datatable.fwrite.sep='\t')

args <- commandArgs(trailingOnly=TRUE)

library(colorout)
library(data.table)
library(magrittr)
library(stringr)
library(libbib)   # v >= 1.6.2
library(assertr)

# ------------------------------ #


PLOTS_P <- FALSE
SHADOW_DATA_LOCATION <- "~/Dropbox/NYPL/nypl-shadow-export/target/"


####################
## DAILY ###########
####################


dat <- fread_plus_date("./data/lair-scan-and-deliver.dat")
set_lb_attribute(dat, "source", "https://docs.google.com/spreadsheets/d/13zzPYWSM4YTeBfApgVdgdpEZWaIdk_Bu2io5JQqS0KY/edit#gid=0")


NAMESTHATSHOULDBEINIT <- c("Transaction ID", "Application",
                           "Transaction Type", "Transaction Date",
                           "Item Record No", "Bib Record No",
                           "Item Location", "Item Type", "Patron Type",
                           "Home Library", "Stat Code")

dat %>% verify(has_only_names(NAMESTHATSHOULDBEINIT),
               success_fun=success_report)

better.names <- c("transaction_ID", "application", "transaction_type",
                  "transaction_date", "itemid", "bibid", "item_loc",
                  "itype", "patron_type", "home_library", "stat_code")
setnames(dat, better.names)

dat %>% assert(within_bounds(850, 852), stat_code,
               success_fun=success_report)

dat[, from_where:=fcase(stat_code==850, "SASB",
                        stat_code==851, "LPA",
                        stat_code==852, "Schomburg")]

dat[, xdate:=as.Date(str_replace(transaction_date, "(202\\d).+$", "\\1"),
                     format="%m/%d/%Y")]

dat <- dat[transaction_type=="Filled Request"]
# dat <- dat[transaction_type=="Checkout"]

reuse <- copy(dat)

dat[, .N, xdate] -> tmp
tmp[, from_where:="total"]

dat[, .N, .(from_where, xdate)] -> tmp2


setcolorder(tmp2, c("xdate", "from_where", "N"))
setcolorder(tmp, c("xdate", "from_where", "N"))

tmp <- rbind(tmp2, tmp)
setnames(tmp, "from_where", "center")

tmp %>% dcast(xdate~center, value.var='N', fill=0) -> tmp2
tmp %>% dcast(xdate~center, value.var='N', fill=0) -> tmp3

tmp2 %>% melt(id="xdate", variable.name="center", value.name="total") -> tmp2

if(PLOTS_P){
  library(ggplot2)
  options(warn=1)
  ggplot(tmp2, aes(x=xdate, y=total, group=center, color=center, fill=center)) +
    geom_line(size=1.5) +
    ylim(c(0, 40)) +
    ggtitle("scan and deliver filled requests (sierra metrics)") +
    xlab("date") + ylab("number of filled requests")
}

cp_lb_attributes(dat, tmp2)
tmp2 %>% fwrite_plus_date("./target/scan-and-deliver-daily.dat")

# --------------------------------------------------------------- #
# --------------------------------------------------------------- #

####################
## WEEKLY ##########
####################

dat <- copy(reuse)

dat[, theweek:=week(xdate)]

# this is... _a_ solution
dat[, theweek:=theweek+(53*(year(xdate)-2020))]


dat[, .N, theweek] -> tmp
tmp[, from_where:="total"]

dat[, .N, .(from_where, theweek)] -> tmp2


setcolorder(tmp2, c("theweek", "from_where", "N"))
setcolorder(tmp, c("theweek", "from_where", "N"))

tmp <- rbind(tmp2, tmp)
setnames(tmp, "from_where", "center")

tmp %>% dcast(theweek~center, value.var='N', fill=0) -> tmp2
tmp %>% dcast(theweek~center, value.var='N', fill=0) -> tmp3

tmp2 %>% melt(id="theweek", variable.name="center", value.name="total") -> tmp2

if(PLOTS_P){
  options(warn=1)
  ggplot(tmp2, aes(x=theweek, y=total, group=center, color=center, fill=center)) +
    geom_line(size=1.5) +
    # ylim(c(0, 40)) +
    ggtitle("scan and deliver filled requests (sierra metrics)") +
    xlab("week") + ylab("number of filled requests")
}


dat[order(theweek, xdate), .(theweek, olddate=xdate)][
      !duplicated(theweek)][,
      .(theweek, xdate=min(olddate)+((theweek-37)*7))] -> weeknumxwalk


### Eliding most recent week
tmp2 <- tmp2[theweek<max(theweek)]
weeknumxwalk <- weeknumxwalk[theweek<max(theweek)]


setkey(weeknumxwalk, "theweek")
setkey(tmp2, "theweek")

weeknumxwalk[, .N]
tmp2[,.N]
tmp2[weeknumxwalk] -> tmp2

cp_lb_attributes(dat, tmp2)
tmp2 %>% fwrite_plus_date("./target/scan-and-deliver-weekly.dat")


# --------------------------------------------------------------- #
# --------------------------------------------------------------- #

###########################################
## LANGUAGE / SUBJECT BREAKDOWNS  #########
###########################################


big <- fread_plus_date(sprintf("%s/sierra-research-healed-joined.dat.gz",
                               SHADOW_DATA_LOCATION))
big[,itemid:=as.character(itemid)]

small <- dat[, .(itemid=str_replace(itemid, "^i", ""))]

small %>% verify(nchar(itemid)==8, success_fun=success_report)

setkey(big, "itemid")
setkey(small, "itemid")

together <- big[small, nomatch=NULL][, .(bibid, itemid, lang,
                                         pub_year, lccall, title, author)]

together

together %>% dt_percent_not_na("bibid")
# 100%

# language
together %>% dt_percent_not_na("lang")
# 100%

together %>% dt_counts_and_percents("lang") -> lang
cp_lb_attributes(dat, lang)
lang %>% fwrite_plus_date("./target/scan-and-deliver-language-breakdown.dat")




together %>% dt_percent_not_na("lccall")
# only 68% has lccall number

together[, subject_classification:=get_lc_call_subject_classification(lccall)]
together[, subject_subclassification:=get_lc_call_subject_classification(lccall,
                                                                         subclassification=TRUE)]

together %>% dt_counts_and_percents("subject_classification") -> lc1
cp_lb_attributes(dat, lc1)
lc1 %>% fwrite_plus_date("./target/scan-and-deliver-subject-classification-breakdown.dat",
                         na="NA")

together %>% dt_counts_and_percents("subject_subclassification") -> lc2
cp_lb_attributes(dat, lc2)
lc2 %>% fwrite_plus_date("./target/scan-and-deliver-subject-subclassification-breakdown.dat",
                         na="NA")


