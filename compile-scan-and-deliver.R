#!/usr/local/bin//Rscript --vanilla


# ------------------------------ #
rm(list=ls())

options(echo=TRUE)
options(width = 80)
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
library(libbib)
library(assertr)

library(lubridate)
library(ggplot2)

# ------------------------------ #

PLOTS_P <- FALSE


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

dat[, xdate:=mdy(str_replace(transaction_date, "(2020|2021).+$", "\\1"))]

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


# now at the week level

dat <- copy(reuse)

dat[order(xdate), .(theweek=week(xdate), xdate)][!duplicated(theweek)] -> wxwalk
dat[, theweek:=week(xdate)]

#!!!!!!! THIS IS AN UNFORGIVABLE KLUDGE
dat[year(xdate)==2021, theweek:=theweek+53]


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


dat[order(theweek, xdate), .(theweek, xdate)][!duplicated(theweek)] -> weeknumxwalk

### EDIT THIS EVERYTIME IT IS REFRESHED!!!! ###
### EDIT THIS EVERYTIME IT IS REFRESHED!!!! ###
### EDIT THIS EVERYTIME IT IS REFRESHED!!!! ###
### EDIT THIS EVERYTIME IT IS REFRESHED!!!! ###
tmp2 <- tmp2[theweek<71]
weeknumxwalk <- weeknumxwalk[theweek<71]
### EDIT THIS EVERYTIME IT IS REFRESHED!!!! ###
### EDIT THIS EVERYTIME IT IS REFRESHED!!!! ###
### EDIT THIS EVERYTIME IT IS REFRESHED!!!! ###
### EDIT THIS EVERYTIME IT IS REFRESHED!!!! ###


setkey(weeknumxwalk, "theweek")
setkey(tmp2, "theweek")

weeknumxwalk[, .N]
tmp2[,.N]
tmp2[weeknumxwalk] -> tmp2

cp_lb_attributes(dat, tmp2)
tmp2 %>% fwrite_plus_date("./target/scan-and-deliver-weekly.dat")


#######################################################
#######################################################
#######################################################

# language/subject analysis for arcadia grant

big <- fread_plus_date("../nypl-shadow-export/target/sierra-research-healed-joined.dat.gz")
big[,itemid:=as.character(itemid)]

small <- dat[, .(itemid=str_replace(itemid, "^i", ""))]

small %>% verify(nchar(itemid)==8, success_fun=success_report)

setkey(big, "itemid")
setkey(small, "itemid")

big[,.N]
small[,.N]

together <- big[small, nomatch=NULL][, .(bibid, itemid, lang, pub_year, lccall, title, author)]

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


