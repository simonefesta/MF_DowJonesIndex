#### References ##############################################################################################################
##############################################################################################################################
#
# Rob J Hyndman and George Athanasopoulos - Forecasting: Principles and Practice
# Monash Univerisity, Australia
# https://otexts.com/fpp2/
#
# Robert H. Shumway, David S. Stoffer - Time Series Analysis and Its Applications (with R Examples) 4th Edition
# Springer Texts in Statistics - Springer Verlag
# https://www.stat.pitt.edu/stoffer/tsa4/tsa4.pdf
#
# R - Residual Diagnostic
# https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html
#
##############################################################################################################################
# Reading libraries
library(base)
library(stats)
library(readxl)
library(rlist)
library(tibble)
library(dplyr)
library(tidyverse)
library("data.table")
library(reshape2)
library(ggplot2)
library(scales)
library(numbers)
library(lubridate)
library(zoo)
library(xts)
library(TTR)
library(quantmod)

library(moments)
library(lmtest) 

library(strucchange)
library(broom)
library(rlang)
library(gridSVG)
library(grid)
##############################################################################################################################
##### Environment Setting
#
# Remove all items in Global Environment
rm(list=ls())
#
# Clear all Plots
# try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)
#
# Clear the Console
cat(rep("\n",100))
#
# Set the working directory
WD <- dirname(rstudioapi::getSourceEditorContext()$path)
show(WD)
setwd(WD)
dir()
#
##############################################################################################################################
#### Functions
#
na.rm <- function(x){x <- as.vector(x[!is.na(as.vector(x))])}
#
##############################################################################################################################
################################ Create a data frame of data from US Department of Treasury ##################################
# From the website "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics",
# we download the "Daily Treasure Par Yield Curve Rates" selecting the years from 2020 to 2023 as the "time period".
# We obtain the files "Daily Treasury Rates - 2020.csv", ..., "Daily Treasury Rates - 2023.csv".
# Hence, we transform the above files in data frames.
US_DTR_2020_df <- read.csv("Daily Treasury Rates - 2020.csv")
class(US_DTR_2020_df)
head(US_DTR_2020_df,15)
tail(US_DTR_2020_df,15)
US_DTR_2021_df <- read.csv("Daily Treasury Rates - 2021.csv")
class(US_DTR_2021_df)
head(US_DTR_2021_df,15)
tail(US_DTR_2021_df,15)
US_DTR_2022_df <- read.csv("Daily Treasury Rates - 2022.csv")
class(US_DTR_2022_df)
head(US_DTR_2022_df,15)
tail(US_DTR_2022_df,15)
US_DTR_2023_df <- read.csv("Daily Treasury Rates - 2023.csv")
class(US_DTR_2023_df)
head(US_DTR_2023_df,15)
tail(US_DTR_2023_df,15)
# Note that in the above data frames, the temporal ordering of the rows, accounted by the column Date, against the row names
# is decreasing: following the order of the row names, the rows go from the most recent to the least recent.
# However, for our purposes, it is more convenient to dispose of data in increasing temporal order (from the least
# recent to the most recent). Therefore, we invert the temporal order of the rows in the data frames.
US_DTR_2020_df <- US_DTR_2020_df[nrow(US_DTR_2020_df):1,]
head(US_DTR_2020_df,15)
tail(US_DTR_2020_df,15)
US_DTR_2021_df <- US_DTR_2021_df[nrow(US_DTR_2021_df):1,]
head(US_DTR_2021_df,15)
tail(US_DTR_2021_df,15)
US_DTR_2022_df <- US_DTR_2022_df[nrow(US_DTR_2022_df):1,]
head(US_DTR_2022_df,15)
tail(US_DTR_2022_df,15)
US_DTR_2023_df <- US_DTR_2023_df[nrow(US_DTR_2023_df):1,]
head(US_DTR_2023_df,15)
tail(US_DTR_2023_df,15)
# Note also that the "four months daily treasury rate" is reported in the column *X4.Mo* only from the year 2022.
# More precisely, the "four months daily treasury rate" is reported from the day
# US_DTR_2022_df$Date[min(which(!is.na(US_DTR_2022_df$X4.Mo)))]
# as it clearly appears by observing the data frame *US_DTR_2022_df* in the vicinity of the above determined day.
# show(US_DTR_2022_df[(min(which(!is.na(US_DTR_2022_df$X4.Mo)))-5):(min(which(!is.na(US_DTR_2022_df$X4.Mo)))+5),])
# Therefore, with the goal of merging the different data frames into a single one, we add a *X4.Mo* column with *NA* entries
# to the data frames *US_DTR_2020_df* and *US_DTR_2021_df*.
# library(tibble)
US_DTR_2020_df <- add_column(US_DTR_2020_df, X4.Mo=rep(NA, nrow(US_DTR_2020_df)), .after="X3.Mo")
head(US_DTR_2020_df)
tail(US_DTR_2020_df)
US_DTR_2021_df <- add_column(US_DTR_2021_df, X4.Mo=rep(NA, nrow(US_DTR_2021_df)), .after="X3.Mo")
head(US_DTR_2021_df)
tail(US_DTR_2021_df)
# Hence, we merge the data frames *US_DTR_2020_df*, ..., *US_DTR_2023_df* in a single data frame.
# library(dplyr)
US_DTR_2020_2023_df <- bind_rows(US_DTR_2020_df,US_DTR_2021_df,US_DTR_2022_df,US_DTR_2023_df)
head(US_DTR_2020_2023_df)
tail(US_DTR_2020_2023_df)
#
# We check whether the Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_DTR_2020_2023_df$Date)
US_DTR_2020_2023_df$Date <- as.Date(US_DTR_2020_2023_df$Date, format="%m/%d/%Y")
class(US_DTR_2020_2023_df$Date)
head(US_DTR_2020_2023_df,15)
tail(US_DTR_2020_2023_df,15)
# In the end, although unnecessary, we rename the columns in a more friendly way.
# library(tidyverse)
US_DTR_2020_2023_df <- rename(US_DTR_2020_2023_df, Mo01=X1.Mo, Mo02=X2.Mo, Mo03=X3.Mo, Mo04=X4.Mo,
                              Mo06=X6.Mo, Yr01=X1.Yr, Yr02=X2.Yr, Yr03=X3.Yr, Yr05=X5.Yr, Yr07=X7.Yr,
                              Yr10=X10.Yr, Yr20=X20.Yr, Yr30=X30.Yr)
head(US_DTR_2020_2023_df)
tail(US_DTR_2020_2023_df)
# We save the data frame US_DTR_2020_2023_df as a RData file, remove it from the environment, and verify that it has been 
# removed.
save(US_DTR_2020_2023_df, file="US_DTR_2020_2023_df.RData")
# rm(US_DTR_2020_2023_df)
head(US_DTR_2020_2023_df)
#
# We load the data frame US_DTR_2020_2023_df.
# LOAD!
load("US_DTR_2020_2023_df.RData")
head(US_DTR_2020_2023_df)
tail(US_DTR_2020_2023_df)
#
##############################################################################################################################
# To draw a plot of the Treasury Yield Curve Rates, we need to manipulate the data frame *US_DTR_2020_2023_df*.
# First, we extract some rows (e.g., from March 31st to April 11th, 2023) from the data frame and rename the rows.
init_date  <- which(US_DTR_2020_2023_df$Date=="2023-03-31")
final_date <- which(US_DTR_2020_2023_df$Date=="2023-04-11")
selec_rows <- seq.int(from=init_date, to=final_date, by=1)
selec_rows_US_DTR_2020_2023_df <- US_DTR_2020_2023_df[selec_rows,]
show(selec_rows_US_DTR_2020_2023_df)
rownames(selec_rows_US_DTR_2020_2023_df) <- seq(from=1, to=nrow(selec_rows_US_DTR_2020_2023_df))
show(selec_rows_US_DTR_2020_2023_df)
#
save(selec_rows_US_DTR_2020_2023_df, file="selec_rows_US_DTR_2020_2023_df.RData")
# rm(selec_rows_US_DTR_2020_2023_df)
show(selec_rows_US_DTR_2020_2023_df)
#
# We load the data frame selec_rows_US_DTR_2020_2023_df.
# LOAD!
load("selec_rows_US_DTR_2020_2023_df.RData")
show(selec_rows_US_DTR_2020_2023_df)
#
# Second, we change the data frame in a data table.
# library("data.table")
selec_rows_US_DTR_2020_2023_tb <- setDT(selec_rows_US_DTR_2020_2023_df)
class(selec_rows_US_DTR_2020_2023_tb)
show(selec_rows_US_DTR_2020_2023_tb)
#
# Third, by the command *melt* we reshape the wide data frame to a long data frame.
# library(reshape2)
rsh_selec_rows_US_DTR_2020_2023_tb <- melt(selec_rows_US_DTR_2020_2023_tb, id.vars="Date")
class(rsh_selec_rows_US_DTR_2020_2023_tb)
# equivalently
# rsh_selec_rows_US_DTR_2020_2023_tb <- melt(selec_rows_US_DTR_2020_2023_tb, measure.vars=c(2:ncol(selec_rows_US_DTR_2020_2023_tb)))
show(rsh_selec_rows_US_DTR_2020_2023_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2023_tb[(nrow(rsh_selec_rows_US_DTR_2020_2023_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2023_tb),])
nrow(rsh_selec_rows_US_DTR_2020_2023_tb)
# 104
# In the end, we add an Index column to the data frame rsh_sel_US_DTR_2020_2023_tb
rsh_selec_rows_US_DTR_2020_2023_tb <- add_column(rsh_selec_rows_US_DTR_2020_2023_tb,
                                                 Index=rep(1:nrow(selec_rows_US_DTR_2020_2023_df), times=(ncol(selec_rows_US_DTR_2020_2023_df)-1)),
                                                 .before="Date")
show(rsh_selec_rows_US_DTR_2020_2023_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2023_tb[(nrow(rsh_selec_rows_US_DTR_2020_2023_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2023_tb),])
#
# We save the data frame rsh_selec_rows_US_DTR_2020_2023_tb as a RData file, remove it from the environment, and verify that 
# it has been removed.
save(rsh_selec_rows_US_DTR_2020_2023_tb, file="rsh_selec_rows_US_DTR_2020_2023_tb__2023_04_11.RData")
# rm(rsh_selec_rows_US_DTR_2020_2023_tb)
head(rsh_selec_rows_US_DTR_2020_2023_tb)
#
# We load the data frame rsh_selec_rows_US_DTR_2020_2023_tb. Note that although we have saved the data frame with a different 
# name, after reloading it retains the same name it had before saving.
# LOAD!
load("rsh_selec_rows_US_DTR_2020_2023_tb__2023_04_11.RData")
head(rsh_selec_rows_US_DTR_2020_2023_tb,10)
tail(rsh_selec_rows_US_DTR_2020_2023_tb,10)
#
# Finally, We are in a position to draw a draft plot of the Daily Treasury Yield Curve Rates
# library(ggplot2)
Data_df <- rsh_selec_rows_US_DTR_2020_2023_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_03_31_04_11_2023_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("yield rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_03_31_04_11_2023_Curve_Rate_lp)
##############################################################################################################################
# We compare the Treasury Yield Curve Rates in the period 03-31/04-11 2023 plotted above with the curve at the beginning of 
# 2021, 2022, and 2023.
#
# First, we manipulate the data from January 4th to January 15th 2021) from the data frame.
init_date  <- which(US_DTR_2020_2023_df$Date=="2021-01-04")
final_date <- which(US_DTR_2020_2023_df$Date=="2021-01-15")
selec_rows <- seq.int(from=init_date, to=final_date, by=1)
selec_rows_US_DTR_2020_2023_df <- US_DTR_2020_2023_df[selec_rows,]
show(selec_rows_US_DTR_2020_2023_df)
rownames(selec_rows_US_DTR_2020_2023_df) <- seq(from=1, to=nrow(selec_rows_US_DTR_2020_2023_df))
#
# We change the data frame in a table.
# library("data.table")
selec_rows_US_DTR_2020_2023_tb <- setDT(selec_rows_US_DTR_2020_2023_df)   
class(selec_rows_US_DTR_2020_2023_tb)
show(selec_rows_US_DTR_2020_2023_tb)
#
# We reshape the wide data frame to a long data frame.
# library(reshape2)
rsh_selec_rows_US_DTR_2020_2023_tb <- melt(selec_rows_US_DTR_2020_2023_tb, id.vars="Date")
# equivalently
# rsh_selec_rows_US_DTR_2020_2023_tb <- melt(selec_rows_US_DTR_2020_2023_tb, measure.vars=c(2:ncol(selec_rows_US_DTR_2020_2023_tb)))
show(rsh_selec_rows_US_DTR_2020_2023_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2023_tb[(nrow(rsh_selec_rows_US_DTR_2020_2023_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2023_tb),])
nrow(rsh_selec_rows_US_DTR_2020_2023_tb)
# 130
# In the end, we add an Index column to the data frame rsh_sel_US_DTR_2020_2023_tb
rsh_selec_rows_US_DTR_2020_2023_tb <- add_column(rsh_selec_rows_US_DTR_2020_2023_tb,
                                                 Index=rep(1:nrow(selec_rows_US_DTR_2020_2023_df), times=(ncol(selec_rows_US_DTR_2020_2023_df)-1)),
                                                 .before="Date")
show(rsh_selec_rows_US_DTR_2020_2023_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2023_tb[(nrow(rsh_selec_rows_US_DTR_2020_2023_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2023_tb),])
#
save(rsh_selec_rows_US_DTR_2020_2023_tb, file="rsh_selec_rows_US_DTR_2020_2023_tb__2021_01_15.RData")
# rm(rsh_selec_rows_US_DTR_2020_2023_tb)
head(rsh_selec_rows_US_DTR_2020_2023_tb)
# 
# LOAD!
load("rsh_selec_rows_US_DTR_2020_2023_tb__2021_01_15.RData")
head(rsh_selec_rows_US_DTR_2020_2023_tb)
tail(rsh_selec_rows_US_DTR_2020_2023_tb)
# We draw a plot of the Daily Treasury Yield Curve Rates
# library(ggplot2)
Data_df <- rsh_selec_rows_US_DTR_2020_2023_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_04_01_15_2021_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("yield rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_04_01_15_2021_Curve_Rate_lp)
#
# Interpolating the NA value, the plot can be drawn as follows
show(rsh_selec_rows_US_DTR_2020_2023_tb[20:52,])
as.vector(which(is.na(Data_df$value)))
NA_length <- length(as.vector(which(is.na(Data_df$value))))
#
(as.vector(which(is.na(Data_df$value)))-NA_length)[1]
(as.vector(which(is.na(Data_df$value)))+NA_length)[1]
#
(as.vector(which(is.na(Data_df$value)))-NA_length)[2]
(as.vector(which(is.na(Data_df$value)))+NA_length)[2]
#
# ...
#
Data_df$variable[(as.vector(which(is.na(Data_df$value)))- NA_length)[1]]
Data_df$value[(as.vector(which(is.na(Data_df$value)))-NA_length)[1]]
Data_df$variable[as.vector(which(is.na(Data_df$value))+NA_length)[1]]
Data_df$value[as.vector(which(is.na(Data_df$value))+NA_length)[1]]
# ...
Data_df$variable[(as.vector(which(is.na(Data_df$value)))- NA_length)[6]]
Data_df$value[(as.vector(which(is.na(Data_df$value)))-NA_length)[6]]
Data_df$variable[as.vector(which(is.na(Data_df$value))+NA_length)[6]]
Data_df$value[as.vector(which(is.na(Data_df$value))+NA_length)[6]]
# ...
#
Data_df <- rsh_selec_rows_US_DTR_2020_2023_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
NA_length <- length(as.vector(which(is.na(Data_df$value))))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_04_01_15_2021_Interp_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[1]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[1]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[1]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[1]], 
                   color=factor(Index)[1]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[2]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[2]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[2]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[2]], 
                   color=factor(Index)[2]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[3]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[3]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[3]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[3]], 
                   color=factor(Index)[3]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[4]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[4]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[4]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[4]], 
                   color=factor(Index)[4]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[5]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[5]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[5]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[5]], 
                   color=factor(Index)[5]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[6]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[6]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[6]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[6]], 
                   color=factor(Index)[6]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[7]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[7]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[7]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[7]], 
                   color=factor(Index)[7]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[8]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[8]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[8]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[8]], 
                   color=factor(Index)[8]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[9]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[9]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[9]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[9]], 
                   color=factor(Index)[9]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[10]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[10]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[10]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[10]], 
                   color=factor(Index)[10]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("yield rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_04_01_15_2021_Interp_Curve_Rate_lp)
##############################################################################################################################
# Second, we manipulate the data from January 3rd to January 14th 2022) from the data frame.
init_date  <- which(US_DTR_2020_2023_df$Date=="2022-01-03")
final_date <- which(US_DTR_2020_2023_df$Date=="2022-01-14")
selec_rows <- seq.int(from=init_date, to=final_date, by=1)
selec_rows_US_DTR_2020_2023_df <- US_DTR_2020_2023_df[selec_rows,]
show(selec_rows_US_DTR_2020_2023_df)
rownames(selec_rows_US_DTR_2020_2023_df) <- seq(from=1, to=nrow(selec_rows_US_DTR_2020_2023_df))
#
# We change the data frame in a table.
# library("data.table")
selec_rows_US_DTR_2020_2023_tb <- setDT(selec_rows_US_DTR_2020_2023_df)   
class(selec_rows_US_DTR_2020_2023_tb)
show(selec_rows_US_DTR_2020_2023_tb)
#
# We reshape the wide data frame to a long data frame.
# library(reshape2)
rsh_selec_rows_US_DTR_2020_2023_tb <- melt(selec_rows_US_DTR_2020_2023_tb, id.vars="Date")
# equivalently
# rsh_selec_rows_US_DTR_2020_2023_tb <- melt(selec_rows_US_DTR_2020_2023_tb, measure.vars=c(2:ncol(selec_rows_US_DTR_2020_2023_tb)))
show(rsh_selec_rows_US_DTR_2020_2023_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2023_tb[(nrow(rsh_selec_rows_US_DTR_2020_2023_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2023_tb),])
nrow(rsh_selec_rows_US_DTR_2020_2023_tb)
# 130
#
# We add an Index column to the data frame rsh_sel_US_DTR_2020_2023_tb
# library(tibble)
rsh_selec_rows_US_DTR_2020_2023_tb <- add_column(rsh_selec_rows_US_DTR_2020_2023_tb,
                                                 Index=rep(1:nrow(selec_rows_US_DTR_2020_2023_df), times=(ncol(selec_rows_US_DTR_2020_2023_df)-1)),
                                                 .before="Date")
show(rsh_selec_rows_US_DTR_2020_2023_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2023_tb[(nrow(rsh_selec_rows_US_DTR_2020_2023_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2023_tb),])
#
save(rsh_selec_rows_US_DTR_2020_2023_tb, file="rsh_selec_rows_US_DTR_2020_2023_tb__2022-01-14.RData")
rm(rsh_selec_rows_US_DTR_2020_2023_tb)
head(rsh_selec_rows_US_DTR_2020_2023_tb)
#
# LOAD!
load("rsh_selec_rows_US_DTR_2020_2023_tb__2022-01-14.RData")
head(rsh_selec_rows_US_DTR_2020_2023_tb)
tail(rsh_selec_rows_US_DTR_2020_2023_tb)
#
# We draw a plot of the Daily Treasury Yield Curve Rates
# library(ggplot2)
Data_df <- rsh_selec_rows_US_DTR_2020_2023_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_03_01_14_2022_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("yield rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_03_01_14_2022_Curve_Rate_lp)
#
# Interpolating the NA value we the plot can be drawn as follows
show(rsh_selec_rows_US_DTR_2020_2023_tb[20:52,])
as.vector(which(is.na(Data_df$value)))
NA_length <- length(as.vector(which(is.na(Data_df$value))))
#
(as.vector(which(is.na(Data_df$value)))-NA_length)[1]
(as.vector(which(is.na(Data_df$value)))+NA_length)[1]
#
(as.vector(which(is.na(Data_df$value)))-NA_length)[2]
(as.vector(which(is.na(Data_df$value)))+NA_length)[2]
#
# ...
#
Data_df$variable[(as.vector(which(is.na(Data_df$value)))- NA_length)[1]]
Data_df$value[(as.vector(which(is.na(Data_df$value)))-NA_length)[1]]
Data_df$variable[as.vector(which(is.na(Data_df$value))+NA_length)[1]]
Data_df$value[as.vector(which(is.na(Data_df$value))+NA_length)[1]]
# ...
Data_df$variable[(as.vector(which(is.na(Data_df$value)))- NA_length)[6]]
Data_df$value[(as.vector(which(is.na(Data_df$value)))-NA_length)[6]]
Data_df$variable[as.vector(which(is.na(Data_df$value))+NA_length)[6]]
Data_df$value[as.vector(which(is.na(Data_df$value))+NA_length)[6]]
# ...
#
Data_df <- rsh_selec_rows_US_DTR_2020_2023_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
NA_length <- length(as.vector(which(is.na(Data_df$value))))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_03_01_14_2022_Interp_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[1]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[1]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[1]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[1]], 
                   color=factor(Index)[1]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[2]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[2]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[2]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[2]], 
                   color=factor(Index)[2]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[3]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[3]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[3]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[3]], 
                   color=factor(Index)[3]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[4]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[4]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[4]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[4]], 
                   color=factor(Index)[4]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[5]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[5]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[5]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[5]], 
                   color=factor(Index)[5]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[6]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[6]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[6]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[6]], 
                   color=factor(Index)[6]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[7]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[7]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[7]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[7]], 
                   color=factor(Index)[7]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[8]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[8]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[8]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[8]], 
                   color=factor(Index)[8]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[9]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[9]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[9]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[9]], 
                   color=factor(Index)[9]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  geom_segment(aes(x=variable[(as.vector(which(is.na(value)))-NA_length)[10]], 
                   y=value[(as.vector(which(is.na(value)))-NA_length)[10]], 
                   xend=variable[(as.vector(which(is.na(value)))+NA_length)[10]], 
                   yend=value[(as.vector(which(is.na(value)))+NA_length)[10]], 
                   color=factor(Index)[10]), linetype="dashed", alpha=1, linewidth=1, group=1) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("yield rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_03_01_14_2022_Interp_Curve_Rate_lp)
##############################################################################################################################
# Third, we manipulate the data from January 3rd to January 14th 2023 from the data frame.
init_date  <- which(US_DTR_2020_2023_df$Date=="2023-01-03")
final_date <- which(US_DTR_2020_2023_df$Date=="2023-01-17")
selec_rows <- seq.int(from=init_date, to=final_date, by=1)
selec_rows_US_DTR_2020_2023_df <- US_DTR_2020_2023_df[selec_rows,]
show(selec_rows_US_DTR_2020_2023_df)
rownames(selec_rows_US_DTR_2020_2023_df) <- seq(from=1, to=nrow(selec_rows_US_DTR_2020_2023_df))
#
# We change the data frame in a table
# library("data.table")
selec_rows_US_DTR_2020_2023_tb <- setDT(selec_rows_US_DTR_2020_2023_df)   
class(selec_rows_US_DTR_2020_2023_tb)
show(selec_rows_US_DTR_2020_2023_tb)
#
# library(reshape2)
rsh_selec_rows_US_DTR_2020_2023_tb <- melt(selec_rows_US_DTR_2020_2023_tb, id.vars="Date")
# equivalently
# rsh_selec_rows_US_DTR_2020_2023_tb <- melt(selec_rows_US_DTR_2020_2023_tb, measure.vars=c(2:ncol(selec_rows_US_DTR_2020_2023_tb)))
show(rsh_selec_rows_US_DTR_2020_2023_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2023_tb[(nrow(rsh_selec_rows_US_DTR_2020_2023_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2023_tb),])
nrow(rsh_selec_rows_US_DTR_2020_2023_tb)
#
# We add an Index column to the data frame rsh_sel_US_DTR_2020_2023_tb
# library(tibble)
rsh_selec_rows_US_DTR_2020_2023_tb <- add_column(rsh_selec_rows_US_DTR_2020_2023_tb,
                                                 Index=rep(1:nrow(selec_rows_US_DTR_2020_2023_df), times=(ncol(selec_rows_US_DTR_2020_2023_df)-1)),
                                                 .before="Date")
show(rsh_selec_rows_US_DTR_2020_2023_tb[1:20,])
show(rsh_selec_rows_US_DTR_2020_2023_tb[(nrow(rsh_selec_rows_US_DTR_2020_2023_tb)-20):nrow(rsh_selec_rows_US_DTR_2020_2023_tb),])
#
save(rsh_selec_rows_US_DTR_2020_2023_tb, file="rsh_selec_rows_US_DTR_2020_2023_tb__2023-01-17.RData")
# rm(rsh_selec_rows_US_DTR_2020_2023_tb)
head(rsh_selec_rows_US_DTR_2020_2023_tb)
#
# LOAD!
load("rsh_selec_rows_US_DTR_2020_2023_tb__2023-01-17.RData")
head(rsh_selec_rows_US_DTR_2020_2023_tb)
tail(rsh_selec_rows_US_DTR_2020_2023_tb)
#
# Finally, We are in a position to draw a draft plot of the Daily Treasury Yield Curve Rates
# library(ggplot2)
Data_df <- rsh_selec_rows_US_DTR_2020_2023_tb
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
length <- nrow(Data_df)/length(Uniq_dates)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plots of U.S. Treasury Yield Curve Rates (business days from ", .(First_Day), " to ", .(Last_Day),")")))
link <- "https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics"
subtitle_content <- bquote(paste("path length ", .(length), " sample points. Data by courtesy of  U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("expiration dates")
leg_labs <- as.character(Uniq_dates)
# leg_vals <- levels(factor(Data_df$Index))
leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_DTR_01_03_01_17_2023_Curve_Rate_lp <- ggplot(Data_df, aes(x=variable, y=value, group=factor(Index))) + 
  geom_line(aes(color=factor(Index)), linewidth=0.8, linetype="solid") +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time to maturity") + ylab("yield rates") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_DTR_01_03_01_17_2023_Curve_Rate_lp)
#
# Clearly, in this case, interpolation is meaningless.
# Note that, in the three cases, we have used the same piece of script with the only modifications in the choice of the dates.
#
##############################################################################################################################
# https://www.usbank.com/investing/financial-perspectives/market-news/treasury-yields-invert-as-investors-weigh-risk-of-recession.html#:~:text=When%20coupon%20payments%20on%20shorter,2022%20and%20continues%20in%202023.
# https://www.reuters.com/markets/us/deeply-inverted-us-curve-flashed-bank-danger-months-2023-03-14/
# https://www.wsj.com/livecoverage/stock-market-news-today-03-13-2023/card/yield-curve-inversion-unwinds-zEqHtaxQAoKpcXd1J6mF?mod=article_inline
# https://www.forbes.com/sites/johntobey/2023/02/28/this-inverted-yield-curve-is-not-forecasting-a-recession/?sh=2f471c187450
# https://ig.ft.com/the-yield-curve-explained/
# https://fred.stlouisfed.org/series/T10Y2Y
# https://www.statista.com/statistics/1058454/yield-curve-usa/
##############################################################################################################################
# Now, we try to compute the Treasury Yield Rates from the prices of Market Based Bills.
# https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate
# We build a data frame with data from the "securityprice-2023-01-03.csv" file.
US_SP_2023_01_03_df <- read.csv("securityprice-2023-01-03.csv", header=FALSE)
show(US_SP_2023_01_03_df[1:15,])
# We rename the columns according to the terminology in https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.
US_SP_2023_01_03_df <- rename(US_SP_2023_01_03_df, CUSIP=V1, Security.Type=V2, Rate=V3, Maturity.Date=V4,
                              Call.Date=V5, Buy=V6, Sell=V7, End.of.Day=V8)
head(US_SP_2023_01_03_df)
# We check whether the Maturity.Date column is in "Date" format. In case it is not, we change the format to "Date".
# class(US_SP_2023_01_03_df$Maturity.Date)
US_SP_2023_01_03_df$Maturity.Date <- as.Date(US_SP_2023_01_03_df$Maturity.Date,  format="%m/%d/%Y")
show(US_SP_2023_01_03_df[1:15,])
class(US_SP_2023_01_03_df$Maturity.Date)
# We add an Index column to help in plots.
US_SP_2023_01_03_df <- add_column(US_SP_2023_01_03_df, Index=1:nrow(US_SP_2023_01_03_df), .before="CUSIP")
show(US_SP_2023_01_03_df[1:15,])
# We add a column *Days.to.Maturity*, which accounts for the number of days from January 1st, 2023, to the Maturity Date.
US_SP_2023_01_03_df <- add_column(US_SP_2023_01_03_df,
                                  Days.to.Maturity=as.vector(difftime(US_SP_2023_01_03_df$Maturity.Date, as.Date(as.character("2023-01-03")), units="days")),
                                  .after="Maturity.Date")
show(US_SP_2023_01_03_df[1:15,])
# We also add the column *Months.to.Maturity* [resp. *Years.to.Maturity*], which accounts for the number of months
# [resp. years] from January 1st, 2023, to the Maturity Date.
US_SP_2023_01_03_df <- add_column(US_SP_2023_01_03_df,
                                  Months.to.Maturity=as.vector(US_SP_2023_01_03_df$Days.to.Maturity/30.4369),
                                  Years.to.Maturity=as.vector(US_SP_2023_01_03_df$Days.to.Maturity/365.2425),
                                  .after="Days.to.Maturity")
show(US_SP_2023_01_03_df[1:15,])
#
# We restrict ourselves to consider only MARKET BASED BILLS.
US_BILLS_SP_2023_01_03_df <- subset(US_SP_2023_01_03_df, US_SP_2023_01_03_df$Security.Type=="MARKET BASED BILL")
head(US_BILLS_SP_2023_01_03_df)
tail(US_BILLS_SP_2023_01_03_df)
#
# Equivalently
# US_SP_2023_01_03_Rate_0_df <- subset(US_SP_2023_01_03_df, US_SP_2023_01_03_df$Rate==0)
# head(US_SP_2023_01_03_Rate_0_df)
# tail(US_SP_2023_01_03_Rate_0_df)
#
# We add a column *Rate.of.Return.at.Maturity* and *Perc.Rate.of.Return.at.Maturity*
US_BILLS_SP_2023_01_03_df <- add_column(US_BILLS_SP_2023_01_03_df,
                                  Rate.of.Ret.at.Maturity=(100-US_BILLS_SP_2023_01_03_df$End.of.Day)/US_BILLS_SP_2023_01_03_df$End.of.Day,
                                  Perc.Rate.of.Ret.at.Maturity=label_percent(accuracy = 0.001)((100-US_BILLS_SP_2023_01_03_df$End.of.Day)/US_BILLS_SP_2023_01_03_df$End.of.Day),
                                  .after="End.of.Day")
show(US_BILLS_SP_2023_01_03_df[1:15,])
#
# We compute the annual rate of return according to the formulas
# (1+r_a)^t=1+r_t; r_a=(1+r_t)^(1/t)-1
# where r_a=annual rate of return, t=time to maturity (in years), r_t=rate of return in the period t.
# or
# (1+r_a)^(t/365.2425)=1+r_t; r_a=(1+r_t)^(365.2425/t)-1
# where r_a=annual rate of return, t=time to maturity (in days), r_t=rate of return in the period t.
#
Annual.Rate.of.Ret_01 <- (1+US_BILLS_SP_2023_01_03_df$Rate.of.Ret.at.Maturity)^(1/US_BILLS_SP_2023_01_03_df$Years.to.Maturity)-1
show(Annual.Rate.of.Ret_01)
Annual.Rate.of.Ret_02 <- (1+US_BILLS_SP_2023_01_03_df$Rate.of.Ret.at.Maturity)^(365.2425/US_BILLS_SP_2023_01_03_df$Days.to.Maturity)-1
show(Annual.Rate.of.Ret_02)
Annual.Rate.of.Ret_01==Annual.Rate.of.Ret_02
# We add a column *Ann.Rate.of.Return* and *Perc.Ann.Rate.of.Return*
US_BILLS_SP_2023_01_03_df <- add_column(US_BILLS_SP_2023_01_03_df,
                                  Ann.Rate.of.Ret=(1+US_BILLS_SP_2023_01_03_df$Rate.of.Ret.at.Maturity)^(1/US_BILLS_SP_2023_01_03_df$Years.to.Maturity)-1,
                                  Perc.Ann.Rate.of.Ret=label_percent(accuracy = 0.001)((1+US_BILLS_SP_2023_01_03_df$Rate.of.Ret.at.Maturity)^(1/US_BILLS_SP_2023_01_03_df$Years.to.Maturity)-1),
                                  .after="Perc.Rate.of.Ret.at.Maturity")
show(US_BILLS_SP_2023_01_03_df[1:15,])
#
save(US_BILLS_SP_2023_01_03_df, file="US_BILLS_SP_2023_01_03_df.RData")
# rm(US_BILLS_SP_2023_01_03_df)
head(US_BILLS_SP_2023_01_03_df)
#
# LOAD!
load("US_BILLS_SP_2023_01_03_df.RData")
head(US_BILLS_SP_2023_01_03_df)
tail(US_BILLS_SP_2023_01_03_df)
#
# We draw a plot of the U.S. Market Based Bill Yield Curve Rates on Jan, 3rd, 2023.
# library(ggplot2)
Data_df <- US_BILLS_SP_2023_01_03_df
DS_length <- nrow(Data_df)
First_Day <- as.character(Data_df$Maturity.Date[1])
Last_Day <- as.character(last(Data_df$Maturity.Date))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Line Plot of the U.S. Market Based Bill Yield Curve Rates on Jan, 3rd, 2023. Maturities from ", .(First_Day), " to ", .(Last_Day))))
link <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate"
subtitle_content <- bquote(paste("path length ", .(DS_length), " sample points. Data by courtesy of  FedInvest, U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("maturity dates (from Jan, 3rd, 2023)")
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(DS_length)
x_breaks_num <- 10
x_breaks_low <- first(Data_df$Index)
x_breaks_up <- last(Data_df$Index)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Maturity.Date[x_breaks])
J <- 0.5
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("annualized yield rate (from Jan, 3rd, 2023 to maturity date)")
y_breaks_num <- 10
y_max <- max(na.rm(as.numeric(sub("%","", Data_df$Perc.Ann.Rate.of.Ret))))
y_min <- min(na.rm(as.numeric(sub("%","", Data_df$Perc.Ann.Rate.of.Ret))))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- paste(format(y_breaks, scientific=FALSE),"%",sep="")
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_b <- bquote("annualized yield rate")
leg_line_labs   <- c(line_col_b)
leg_line_cols   <- c("line_col_b"="blue")
leg_line_breaks <- c("line_col_b")
US_SP_2023_01_03_BILLS_Yield_Rate_lp <- ggplot(Data_df, aes(x=Index)) + 
  geom_line(aes(y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="line_col_b"), linewidth=0.8, linetype="solid") +
  scale_x_continuous(breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("maturity dates (from Jan, 3rd, 2023)") + 
  ylab("annualized yield rate (from Jan, 3rd, 2023 to maturity date)") +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(color="black", size=10, face="italic", angle=0, vjust=0),
        axis.text.y=element_text(color="black", size=11, face="italic", angle=0, vjust=0),
        axis.title.x=element_text(color="black", size=13, face="bold.italic", angle=0, vjust=-2),
        axis.title.y=element_text(color="black", size=13, face="bold.italic", angle=90, vjust=0),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(US_SP_2023_01_03_BILLS_Yield_Rate_lp)
#
##############################################################################################################################
# Still, we compute the Treasury Yield Rates from the prices of Market Based Bills.
# https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate
# We build a data frame with data from the "securityprice-2023-04-11.csv" file.
US_SP_2023_04_11_df <- read.csv("securityprice-2023-04-11.csv", header=FALSE)
show(US_SP_2023_04_11_df[1:15,])
# We rename the columns according to the terminology in https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.
US_SP_2023_04_11_df <- rename(US_SP_2023_04_11_df, CUSIP=V1, Security.Type=V2, Rate=V3, Maturity.Date=V4,
                              Call.Date=V5, Buy=V6, Sell=V7, End.of.Day=V8)
head(US_SP_2023_04_11_df)
# We check whether the Maturity.Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_04_11_df$Maturity.Date)
US_SP_2023_04_11_df$Maturity.Date <- as.Date(US_SP_2023_04_11_df$Maturity.Date,  format="%m/%d/%Y")
show(US_SP_2023_04_11_df[1:15,])
class(US_SP_2023_04_11_df$Maturity.Date)
# We add an Index column to help in plots.
US_SP_2023_04_11_df <- add_column(US_SP_2023_04_11_df, Index=1:nrow(US_SP_2023_04_11_df), .before="CUSIP")
show(US_SP_2023_04_11_df[1:15,])
# We add a column *Days.to.Maturity*, which accounts for the number of days from January 1st, 2023, to the Maturity Date
US_SP_2023_04_11_df <- add_column(US_SP_2023_04_11_df, 
                                  Days.to.Maturity=as.vector(difftime(US_SP_2023_04_11_df$Maturity.Date, as.Date(as.character("2023-04-11")), units="days")), 
                                  .after="Maturity.Date")
show(US_SP_2023_04_11_df[1:15,])
# We also add the column *Months.to.Maturity* [resp. *Years.to.Maturity*], which accounts for the number of months 
# [resp. years] from January 1st, 2023, to the Maturity Date.
US_SP_2023_04_11_df <- add_column(US_SP_2023_04_11_df, 
                                  Months.to.Maturity=as.vector(US_SP_2023_04_11_df$Days.to.Maturity/30.4369),
                                  Years.to.Maturity=as.vector(US_SP_2023_04_11_df$Days.to.Maturity/365.2425),
                                  .after="Days.to.Maturity")
show(US_SP_2023_04_11_df[1:15,])
#
# We restrict ourselves to consider only MARKET BASED BILL.
US_BILLS_SP_2023_04_11_df <- subset(US_SP_2023_04_11_df, US_SP_2023_04_11_df$Security.Type=="MARKET BASED BILL")
head(US_BILLS_SP_2023_04_11_df)
tail(US_BILLS_SP_2023_04_11_df)
#
# Equivalently
# US_SP_2023_04_11_Rate_0_df <- subset(US_SP_2023_04_11_df, US_SP_2023_04_11_df$Rate==0)
# head(US_SP_2023_04_11_Rate_0_df)
# tail(US_SP_2023_04_11_Rate_0_df)
#
# We add a column *Rate.of.Return.at.Maturity* and *Perc.Rate.of.Return.at.Maturity*
US_BILLS_SP_2023_04_11_df <- add_column(US_BILLS_SP_2023_04_11_df, 
                                  Rate.of.Ret.at.Maturity=(100-US_BILLS_SP_2023_04_11_df$End.of.Day)/US_BILLS_SP_2023_04_11_df$End.of.Day,
                                  Perc.Rate.of.Ret.at.Maturity=label_percent(accuracy = 0.001)((100-US_BILLS_SP_2023_04_11_df$End.of.Day)/US_BILLS_SP_2023_04_11_df$End.of.Day),
                                  .after="End.of.Day")
show(US_BILLS_SP_2023_04_11_df[1:15,])
#
# We compute the annual rate of return according to the formulas
# (1+r_a)^t = 1+r_t; r_a = (1+r_t)^(1/t)-1
# where r_a=annual rate of return, t=time to maturity (in years), r_t=rate of return in the period t.
# or 
# (1+r_a)^(t/365.2425) = 1+r_t; r_a = (1+r_t)^(365.2425/t)-1
# where r_a=annual rate of return, t=time to maturity (in days), r_t=rate of return in the period t.
#
Annual.Rate.of.Ret_01 <- (1+US_BILLS_SP_2023_04_11_df$Rate.of.Ret.at.Maturity)^(1/US_BILLS_SP_2023_04_11_df$Years.to.Maturity)-1
show(Annual.Rate.of.Ret_01)
Annual.Rate.of.Ret_02 <- (1+US_BILLS_SP_2023_04_11_df$Rate.of.Ret.at.Maturity)^(365.2425/US_BILLS_SP_2023_04_11_df$Days.to.Maturity)-1
show(Annual.Rate.of.Ret_02)
Annual.Rate.of.Ret_01==Annual.Rate.of.Ret_02
round(Annual.Rate.of.Ret_01,16)==round(Annual.Rate.of.Ret_02,16)
round(Annual.Rate.of.Ret_01,15)==round(Annual.Rate.of.Ret_02,15)
#
# We add a column *Ann.Rate.of.Return* and *Perc.Ann.Rate.of.Return*
US_BILLS_SP_2023_04_11_df <- add_column(US_BILLS_SP_2023_04_11_df, 
                                  Ann.Rate.of.Ret=(1+US_BILLS_SP_2023_04_11_df$Rate.of.Ret.at.Maturity)^(1/US_BILLS_SP_2023_04_11_df$Years.to.Maturity)-1,
                                  Perc.Ann.Rate.of.Ret=label_percent(accuracy = 0.001)((1+US_BILLS_SP_2023_04_11_df$Rate.of.Ret.at.Maturity)^(1/US_BILLS_SP_2023_04_11_df$Years.to.Maturity)-1),
                                  .after="Perc.Rate.of.Ret.at.Maturity")
show(US_BILLS_SP_2023_04_11_df[1:30,])
#
save(US_BILLS_SP_2023_04_11_df, file="US_BILLS_SP_2023_04_11_df.RData")
# rm(US_BILLS_SP_2023_04_11_df)
head(US_BILLS_SP_2023_04_11_df)
#
# LOAD!
load("US_BILLS_SP_2023_04_11_df.RData")
head(US_BILLS_SP_2023_04_11_df)
tail(US_BILLS_SP_2023_04_11_df)
#
# We draw a plot of the U.S. Market Based Bill Yield Curve Rates on Apr, 11th, 2023.
# library(ggplot2)
Data_df <- US_BILLS_SP_2023_04_11_df
DS_length <- nrow(Data_df)
First_Day <- as.character(Data_df$Maturity.Date[1])
Last_Day <- as.character(last(Data_df$Maturity.Date))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Line Plot of the U.S. Market Based Bill Yield Curve Rates on Apr, 11th, 2023. Maturities from ", .(First_Day), " to ", .(Last_Day))))
link <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate"
subtitle_content <- bquote(paste("path length ", .(DS_length), " sample points. Data by courtesy of  FedInvest, U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
x_name <- bquote("maturity dates (from Apr, 11th, 2023)")
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(DS_length)
x_breaks_num <- 10
x_breaks_low <- first(Data_df$Index)
x_breaks_up <- last(Data_df$Index)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Maturity.Date[x_breaks])
J <- 0.5
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("annualized yield rate (from Jan, 3rd, 2023 to maturity date)")
y_breaks_num <- 10
y_max <- max(na.rm(as.numeric(sub("%","", Data_df$Perc.Ann.Rate.of.Ret))))
y_min <- min(na.rm(as.numeric(sub("%","", Data_df$Perc.Ann.Rate.of.Ret))))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- paste(format(y_breaks, scientific=FALSE),"%",sep="")
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_b <- bquote("annualized yield rate")
leg_line_labs   <- c(line_col_b)
leg_line_cols   <- c("line_col_b"="blue")
leg_line_breaks <- c("line_col_b")
# leg_vals <- rainbow(length(levels(factor(Data_df$Index))))[as.numeric(levels(factor(Data_df$Index)))]
US_BILLS_SP_2023_04_11_Yield_Rate_lp <- ggplot(Data_df, aes(x=Index)) + 
  geom_line(aes(y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="line_col_b"), linewidth=0.8, linetype="solid") +
  scale_x_continuous(breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("maturity dates (from Apr, 11th, 2023)") + 
  ylab("annualized yield rate (from Apr, 11th, 2023 to maturity date)") +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0),
        axis.text.x=element_text(color="black", size=10, face="italic", angle=0, vjust=0),
        axis.text.y=element_text(color="black", size=11, face="italic", angle=0, vjust=0),
        axis.title.x=element_text(color="black", size=13, face="bold.italic", angle=0, vjust=-2),
        axis.title.y=element_text(color="black", size=13, face="bold.italic", angle=90, vjust=0),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(US_BILLS_SP_2023_04_11_Yield_Rate_lp)
#
##############################################################################################################################
# We use Dr. Simone Nicosanti's Python script to extract data from https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate
# and compare the U.S. Market Based Bill Yield Curve Rates from March, 31st, 2023 to Apr, 11th, 2023 with the corresponding
# Treasury Yield Curve Rates from https://home.treasury.gov/policy-issues/financing-the-government/interest-rate-statistics.
# Nicosanti's script builds the file "TotalOutput_2023-03-31_2023-04-11" containing the price data of market-based Treasury 
# special securities from March, 31st to April 11th 2023. 
# Then, we build a data frame with data from the "TotalOutput_2023-03-31_2023-04-11.csv" file.
US_SP_2023_03_31_2023_04_11_df <- read.csv("TotalOutput_2023-03-31_2023-04-11.csv", header=TRUE)
nrow(US_SP_2023_03_31_2023_04_11_df)
head(US_SP_2023_03_31_2023_04_11_df,15)
show(US_SP_2023_03_31_2023_04_11_df[100:115,])
tail(US_SP_2023_03_31_2023_04_11_df,15)
#
# We rename the columns according to the terminology in https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate.
US_SP_2023_03_31_2023_04_11_df <- rename(US_SP_2023_03_31_2023_04_11_df, Security.Type=SecurityType, 
                                         Maturity.Date=MaturityDate, Call.Date=CallDate, End.of.Day=EndOfDay)
head(US_SP_2023_03_31_2023_04_11_df)
#
# We check whether the Maturity.Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_03_31_2023_04_11_df$MaturityDate)
US_SP_2023_03_31_2023_04_11_df$Maturity.Date <- as.Date(US_SP_2023_03_31_2023_04_11_df$Maturity.Date,  format="%Y-%m-%d")
class(US_SP_2023_03_31_2023_04_11_df$Maturity.Date)
head(US_SP_2023_03_31_2023_04_11_df,15)
#
# We check whether the Date column is in "Date" format. In case it is not, we change the format to "Date".
class(US_SP_2023_03_31_2023_04_11_df$Date)
US_SP_2023_03_31_2023_04_11_df$Date <- as.Date(US_SP_2023_03_31_2023_04_11_df$Date,  format="%Y-%m-%d")
class(US_SP_2023_03_31_2023_04_11_df$Date)
show(US_SP_2023_03_31_2023_04_11_df[1:15,])
#
# We restrict ourselves to consider only MARKET BASED BILL.
US_SP_BILLS_2023_03_31_2023_04_11_df <- subset(US_SP_2023_03_31_2023_04_11_df, US_SP_2023_03_31_2023_04_11_df$Security.Type=="MARKET BASED BILL")
head(US_SP_BILLS_2023_03_31_2023_04_11_df)
tail(US_SP_BILLS_2023_03_31_2023_04_11_df)
nrow(US_SP_BILLS_2023_03_31_2023_04_11_df)
#
# We rename the rows 
rownames(US_SP_BILLS_2023_03_31_2023_04_11_df) <- NULL
tail(US_SP_BILLS_2023_03_31_2023_04_11_df)
#
Data_df <- US_SP_BILLS_2023_03_31_2023_04_11_df
Uniq_dates <- unique(Data_df$Date)
length(Uniq_dates)
mat_dates_subsets <- list()
for(j in 1:length(Uniq_dates)){
  mat_dates_subsets[[j]] <- Data_df$Maturity.Date[which(Data_df$Date==Uniq_dates[j])]
}
show(mat_dates_subsets)
mat_dates_cup <- mat_dates_subsets[[1]]
for(j in 2:length(Uniq_dates)){
  mat_dates_cup <- as.Date(union(mat_dates_cup, mat_dates_subsets[[j]]))
}
show(mat_dates_cup)
class(mat_dates_cup)
length(mat_dates_cup)
#
mat_dates_cap <- mat_dates_subsets[[1]]
for(j in 2:length(Uniq_dates)){
  mat_dates_cap <- as.Date(intersect(mat_dates_cap, mat_dates_subsets[[j]]))
}
show(mat_dates_cap)
class(mat_dates_cap)
length(mat_dates_cap)
#
mat_date_pos <- grep("Maturity.Date", colnames(Data_df))
US_SP_BILLS_2023_03_31_2023_04_11_ls <- list()
for(j in 1:length(Uniq_dates)){
  US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]] <- subset(Data_df, Data_df$Date==Uniq_dates[j])
  rownames(US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]]) <- NULL
  for(k in 1:length(mat_dates_cup)){
    if(mat_dates_cup[k] %in% US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]]$Maturity.Date){}
    else{US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]] <- arrange(rbind(US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]], c(rep(NA,(mat_date_pos-1)),mat_dates_cup[k],rep(NA,(ncol(Data_df)-mat_date_pos)-1),Uniq_dates[j])), Maturity.Date)}
  }
}
#
for(j in 1:length(Uniq_dates)){
  US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]] <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]], Loc_Index=1:nrow(US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]]),
                                                          .before="Cusip")}
#
US_SP_BILLS_2023_03_31_2023_04_11_ls[[1]]
nrow(US_SP_BILLS_2023_03_31_2023_04_11_ls[[1]])
US_SP_BILLS_2023_03_31_2023_04_11_ls[[3]]
nrow(US_SP_BILLS_2023_03_31_2023_04_11_ls[[3]])
US_SP_BILLS_2023_03_31_2023_04_11_ls[[7]]
nrow(US_SP_BILLS_2023_03_31_2023_04_11_ls[[7]])
#
US_SP_BILLS_2023_03_31_2023_04_11_df <- US_SP_BILLS_2023_03_31_2023_04_11_ls[[1]]
nrow(US_SP_BILLS_2023_03_31_2023_04_11_df)
for(j in 2:length(Uniq_dates)){
  US_SP_BILLS_2023_03_31_2023_04_11_df <- rbind(US_SP_BILLS_2023_03_31_2023_04_11_df, US_SP_BILLS_2023_03_31_2023_04_11_ls[[j]])
}
nrow(US_SP_BILLS_2023_03_31_2023_04_11_df)
head(US_SP_BILLS_2023_03_31_2023_04_11_df,15)
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
tail(US_SP_BILLS_2023_03_31_2023_04_11_df,15)
#
# We add an index column
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                                   Index=1:nrow(US_SP_BILLS_2023_03_31_2023_04_11_df),
                                                   .before="Cusip")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
# We add a column *Days.to.Maturity*, which accounts for the number of days from January 1st, 2023, to the Maturity Date
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                             Days.to.Maturity=as.vector(difftime(US_SP_BILLS_2023_03_31_2023_04_11_df$Maturity.Date, 
                                                                                 US_SP_BILLS_2023_03_31_2023_04_11_df$Date, units="days")), 
                                             .after="Maturity.Date")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
# We also add the column *Months.to.Maturity* [resp. *Years.to.Maturity*], which accounts for the number of months 
# [resp. years] from the Date, to the Maturity Date.
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                             Months.to.Maturity=as.vector(US_SP_BILLS_2023_03_31_2023_04_11_df$Days.to.Maturity/30.4369),
                                             Years.to.Maturity=as.vector(US_SP_BILLS_2023_03_31_2023_04_11_df$Days.to.Maturity/365.2425),
                                             .after="Days.to.Maturity")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
# We add a column *Rate.of.Return.at.Maturity* and *Perc.Rate.of.Return.at.Maturity*
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                       Rate.of.Ret.at.Maturity=(100-US_SP_BILLS_2023_03_31_2023_04_11_df$End.of.Day)/US_SP_BILLS_2023_03_31_2023_04_11_df$End.of.Day,
                                       Perc.Rate.of.Ret.at.Maturity=label_percent(accuracy = 0.001)((100-US_SP_BILLS_2023_03_31_2023_04_11_df$End.of.Day)/US_SP_BILLS_2023_03_31_2023_04_11_df$End.of.Day),
                                       .after="End.of.Day")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
#
# We compute the annual rate of return according to the formulas
# (1+r_a)^t = 1+r_t; r_a = (1+r_t)^(1/t)-1
# where r_a=annual rate of return, t=time to maturity (in years), r_t=rate of return in the period t.
# or 
# (1+r_a)^(t/365.2425) = 1+r_t; r_a = (1+r_t)^(365.2425/t)-1
# where r_a=annual rate of return, t=time to maturity (in days), r_t=rate of return in the period t.
#
Ann.Rate.of.Ret_01 <- (1+US_SP_BILLS_2023_03_31_2023_04_11_df$Rate.of.Ret.at.Maturity)^(1/US_SP_BILLS_2023_03_31_2023_04_11_df$Years.to.Maturity)-1
show(Ann.Rate.of.Ret_01)
Ann.Rate.of.Ret_02 <- (1+US_SP_BILLS_2023_03_31_2023_04_11_df$Rate.of.Ret.at.Maturity)^(365.2425/US_SP_BILLS_2023_03_31_2023_04_11_df$Days.to.Maturity)-1
show(Ann.Rate.of.Ret_02)
Ann.Rate.of.Ret_01==Ann.Rate.of.Ret_02
round(Ann.Rate.of.Ret_01,16)==round(Ann.Rate.of.Ret_02,16)
round(Ann.Rate.of.Ret_01,15)==round(Ann.Rate.of.Ret_02,15)
round(Ann.Rate.of.Ret_01,14)==round(Ann.Rate.of.Ret_02,14)
identical(round(Ann.Rate.of.Ret_01,14),round(Ann.Rate.of.Ret_02,14))
#
# We add a column *Ann.Rate.of.Return* and *Perc.Ann.Rate.of.Return*
US_SP_BILLS_2023_03_31_2023_04_11_df <- add_column(US_SP_BILLS_2023_03_31_2023_04_11_df, 
                                                   Ann.Rate.of.Ret=Ann.Rate.of.Ret_01, 
                                                   Perc.Ann.Rate.of.Ret=label_percent(accuracy = 0.001)(Ann.Rate.of.Ret_01),
                                                   .after="Perc.Rate.of.Ret.at.Maturity")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
#
save(US_SP_BILLS_2023_03_31_2023_04_11_df, file="US_SP_BILLS_2023_03_31_2023_04_11_df.RData")
# rm(US_SP_BILLS_2023_03_31_2023_04_11_df)
head(US_SP_BILLS_2023_03_31_2023_04_11_df)
#
# LOAD!
load("US_SP_BILLS_2023_03_31_2023_04_11_df.RData")
show(US_SP_BILLS_2023_03_31_2023_04_11_df[200:240,])
#
Data_df <- US_SP_BILLS_2023_03_31_2023_04_11_df
Uniq_dates <- unique(Data_df$Date)
First_Day <- as.character(first(Uniq_dates))
Last_Day <- as.character(last(Uniq_dates))
path_length <- c()
for(j in 1:length(Uniq_dates)){
path_length[j] <- length(na.rm(Data_df$Cusip[which(Data_df$Date==Uniq_dates[j])]))
}
min_path_length <- min(path_length)
max_path_length <- max(path_length)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Line Plot of the U.S. Market Based Bill Yield Curve Rates. Business Days from ", .(First_Day), " to ", .(Last_Day))))
link <- "https://www.treasurydirect.gov/GA-FI/FedInvest/selectSecurityPriceDate"
subtitle_content <- bquote(paste("path lengths from ", .(min_path_length), " to ", .(max_path_length), " sample points. Data by courtesy of  FedInvest, U.S. Department of the Treasure  -  ", .(link)))
caption_content <- "Author: Roberto Monte"
# primeFactors(min_path_length)
x_breaks_num <- 12
x_breaks_low <- first(Data_df$Loc_Index)
x_breaks_up <- last(Data_df$Loc_Index)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Maturity.Date[x_breaks])
J <- 0.0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_breaks_num <- 10
path_max <- c()
for(j in 1:length(Uniq_dates)){
  path_max[j] <- max(na.rm(as.numeric(sub("%","", subset(Data_df, Data_df$Date == Uniq_dates[j])$Perc.Ann.Rate.of.Ret))))
}
y_max <- max(path_max)
path_min <- c()
for(j in 1:length(Uniq_dates)){
  path_min[j] <- min(na.rm(as.numeric(sub("%","", subset(Data_df, Data_df$Date == Uniq_dates[j])$Perc.Ann.Rate.of.Ret))))
}
y_min <- min(path_min)
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- paste(format(y_breaks, scientific=FALSE),"%",sep="")
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_cols <- rainbow(length(Uniq_dates))
leg_line_1 <- as.character(Uniq_dates)[1]
leg_line_2 <- as.character(Uniq_dates)[2]
leg_line_3 <- as.character(Uniq_dates)[3]
leg_line_4 <- as.character(Uniq_dates)[4]
leg_line_5 <- as.character(Uniq_dates)[5]
leg_line_6 <- as.character(Uniq_dates)[6]
leg_line_7 <- as.character(Uniq_dates)[7]
leg_line_8 <- as.character(Uniq_dates)[8]
leg_line_labs <- c(leg_line_1, leg_line_2, leg_line_3, leg_line_4, leg_line_5, leg_line_6, leg_line_7, leg_line_8)
leg_line_cols <- c("leg_line_1"="#FF0000", "leg_line_2"="#FFBF00", "leg_line_3"="#80FF00", "leg_line_4"="#00FF40",
                   "leg_line_5"="#00FFFF", "leg_line_6"="#0040FF", "leg_line_7"="#8000FF", "leg_line_8"="#FF00BF")
leg_line_breaks <- c("leg_line_1", "leg_line_2", "leg_line_3", "leg_line_4", "leg_line_5", "leg_line_6", "leg_line_7",
                     "leg_line_8")
# leg_line_labs_bis <- as.character(Uniq_dates)
# leg_line_cols_bis <- c(leg_line_bis=line_cols)
# leg_line_breaks_bis <- names(leg_line_cols_bis)
US_SP_BILLS_2023_03_31_2023_04_11_lp <- ggplot(Data_df) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[1]), alpha=1, linewidth=0.8, linetype="solid", 
              aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="leg_line_1", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[2]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="leg_line_2", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[3]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="leg_line_3", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[4]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="leg_line_4", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[5]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="leg_line_5", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[6]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="leg_line_6", group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[7]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="leg_line_7",  group=1)) +
   geom_line(data=subset(Data_df, Data_df$Date == Uniq_dates[8]), alpha=1, linewidth=0.8, linetype="solid",
             aes(x=Loc_Index, y=as.numeric(sub("%","", Perc.Ann.Rate.of.Ret)), color="leg_line_8",  group=1)) +
   annotate(geom = "text", size=3.0, x=x_breaks, y=y_breaks_low, label=Data_df$Days.to.Maturity[x_breaks], vjust=3.0) +
   annotate(geom = "text", size=3.0, x=x_breaks[6], y=y_breaks_low, label="days to maturity from 2023-03-31", 
            hjust=-0.45, vjust=1.0) +
   scale_x_continuous(breaks=x_breaks, labels=x_labs, limits=x_lims) +
   scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                      sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
   ggtitle(title_content) +
   labs(subtitle=subtitle_content, caption=caption_content) +
   xlab("maturity dates") + ylab("annualized yield rates") +
   scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols) +
   theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
         axis.text.x=element_text(color="black", size=10, face="italic", angle=0, vjust=0),
         axis.text.y=element_text(color="black", size=11, face="italic", angle=0, vjust=0),
         axis.title.x=element_text(color="black", size=13, face="bold.italic", angle=0, vjust=-2),
         axis.title.y=element_text(color="black", size=13, face="bold.italic", angle=90, vjust=0),
         legend.key.width = unit(0.80,"cm"), legend.position="right")
plot(US_SP_BILLS_2023_03_31_2023_04_11_lp)
#
##############################################################################################################################
# Futures on Standard & Poor 500 maturity Jun 16th 2023 (Yahoo Finance - ES=F)
# Consider the future on S&P 500 E-Mini Jun 23 (ES=F) or (ESM23.CME)
# https://finance.yahoo.com/quote/%5EGSPC?p=^GSPC&.tsrc=fin-srch
# https://finance.yahoo.com/quote/ES%3DF?p=ES%3DF
# https://www.barchart.com/futures/quotes/ESM23/options/jun-23
# library(lubridate)
# We introduce a couple of variables to be adjusted in terms of days, referring to the current date, to download data 
# up to April 11th, 2023, back of one year.
To <- Sys.Date()-days(38)
From <- To-years(x=1)
# To download data we use the function *getSymbols.yahoo* of the *quantmod* library
# library(quantmod)
ESM23.CME_df <- getSymbols.yahoo("ESM23.CME", from=From, to=To, periodicity="daily",
                             base.currency="USD",  return.class="data.frame", env = .GlobalEnv, 
                             verbose = FALSE, warning = TRUE, auto.assign = FALSE)
class(ESM23.CME_df)
head(ESM23.CME_df)
tail(ESM23.CME_df)
# We remove the rows with NA from the data frame *ESM23.CME_df*.
ESM23.CME_df <- na.omit(ESM23.CME_df)
tail(ESM23.CME_df)
# We add a Date column to the data frame *ESM23.CME_df*.
ESM23.CME_df <- add_column(.data=ESM23.CME_df, Date=as.Date(rownames(ESM23.CME_df), format="%Y-%m-%d"), 
                           .before="ESM23.CME.Open")
head(ESM23.CME_df)
tail(ESM23.CME_df)
# We set the row names of the data frame *ESM23.CME_df* to the default values.
rownames(ESM23.CME_df) <- NULL
head(ESM23.CME_df)
tail(ESM23.CME_df)
# We add an Index column to the data frame *ESM23.CME_df*.
ESM23.CME_df <- add_column(.data=ESM23.CME_df, Index=1:nrow(ESM23.CME_df), .before="Date")
head(ESM23.CME_df)
tail(ESM23.CME_df)
# We save, remove, and restore the downloaded file
save(ESM23.CME_df, file="ESM23.CME_2022_04_12__2023_04_11_df.RData")
# rm(ESM23.CME_df)
head(ESM23.CME_df)
#
# LOAD
load("ESM23.CME_2022_04_12__2023_04_11_df.RData")
head(ESM23.CME_df)
tail(ESM23.CME_df)
#
# the adjusted price is the strike price K of the future contract at the end of day and we know that in arbitrage-free
# markets the Spot futures Parity Theorem holds true
# K = S_0(1+r_f)
# from which it follows
# K/S_0 - 1 = r_f
# We download also the spot prices of Standard & Poor 500 Index (^GSCP)
# https://finance.yahoo.com/quote/%5EGSPC?p=%5EGSPC
# see also Standard & Poor 500 Index (^SPX)
# https://it.finance.yahoo.com/quote/%5ESPX/history?period1=1649721600&period2=1681171200&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true
# see also 
GSPC_df <- getSymbols.yahoo("^GSPC", from=From, to=To, periodicity="daily",
                                 base.currency="USD",  return.class="data.frame", env = .GlobalEnv, 
                                 verbose = FALSE, warning = TRUE, auto.assign = FALSE)
class(GSPC_df)
head(GSPC_df)
tail(GSPC_df)
#
# We add a Date column to the data frame *GSPC_df*.
GSPC_df <- add_column(.data=GSPC_df, Date=as.Date(rownames(GSPC_df), format="%Y-%m-%d"), .before="GSPC.Open")
head(GSPC_df)
tail(GSPC_df)
# We set the row names of the data frame *GSPC_df* to the default values.
rownames(GSPC_df) <- NULL
head(GSPC_df)
tail(GSPC_df)
# We add an Index column to the data frame *ESM23.CME_df*.
GSPC_df <- add_column(.data=GSPC_df, Index=1:nrow(GSPC_df), .before="Date")
head(GSPC_df)
tail(GSPC_df)
# We save, remove, and restore the downloaded file
save(GSPC_df, file="GSPC_2022_04_12__2023_04_11_df.RData")
# rm(GSPC_df)
head(GSPC_df)
#
# LOAD
load("GSPC_2022_04_12__2023_04_11_df.RData")
head(GSPC_df)
tail(GSPC_df)
#
# Note that the numbers of rows in data frames *ESM23.CME_df* and *GSPC_df* are different.
# More precisely, the data frame *ESM23.CME_df* contains one more row than *GSPC_df*.
# Hence, there are rows in *ESM23.CME_df* which are not in *GSPC_df*. The converse also being possible.
# To know what row of *ESM23.CME_df* is not in *GSPC_df* we use the following function
ESM23.CME_add_row <- which(!(ESM23.CME_df$Date %in% GSPC_df$Date))
class(ESM23.CME_add_row)
show(ESM23.CME_add_row)
# 192
#
# The *ESM23.CME_add_row* corresponds to the date
show(ESM23.CME_df$Date[ESM23.CME_add_row])
# "2023-01-16"
#
# Since only one row of *ESM23.CME_df* is not in *GSPC_df*, and *ESM23.CME_df* contains only an additional row,
# all rows in *GSPC_df* must be in *ESM23.CME_df*.
# However, if we check the possibility that some rows of *ESM23.CME_df* are not in *ESM23.CME_df*, we find
GSPC_add_row <- which(!(GSPC_df$Date %in% ESM23.CME_df$Date))
show(GSPC_add_row)
# integer(0)
# which confirms that all rows of *GSPC_df* are in *ESM23.CME_df*.
# Hence, we check the data frames ESM23.CME_df and GSPC_df$Date around the the missed row.
show(ESM23.CME_df[(ESM23.CME_add_row-5):(ESM23.CME_add_row+5),])
show(GSPC_df[(ESM23.CME_add_row-5):(ESM23.CME_add_row+5),])
# A quick investigation on the web shows that wall Street was closed on January 16th, 2023
# https://finance.yahoo.com/news/stock-market-news-jan-17-143102176.html?guccounter=1&guce_referrer=aHR0cHM6Ly93d3cuZ29vZ2xlLmNvbS8&guce_referrer_sig=AQAAAAOBZFEnSAfCF1b_7ScWnp1uOGaRQ6sa8EpkTXHx-Wnb-ZyYH4JrWg5jC7RsNw1dd2TU3GgzwiAohGABwZxg41ZyLsabaJdPDlmeKCqOAE3f-kCaaeOnIojVO6UebBoUQfq_Ijuwl65ShRaQ5-vWw17JbcspTXH_K0p7mYtU2WdR
# Consider that the future ESM23.CME is traded on CME at Chicago.
# In the end, to homogenize the data frames *ESM23.CME_df* and *GSPC_df*, we delete the *ESM23.CME_add_row* and We 
# reset the row names and the Index column of the data frame *ESM23.CME_df* to the default values. 
ESM23.CME_df <- ESM23.CME_df[-ESM23.CME_add_row,]
show(ESM23.CME_df[(ESM23.CME_add_row-5):(ESM23.CME_add_row+5),])
rownames(ESM23.CME_df) <- NULL
ESM23.CME_df$Index <- 1:nrow(ESM23.CME_df)
show(ESM23.CME_df[(ESM23.CME_add_row-5):(ESM23.CME_add_row+5),])
nrow(ESM23.CME_df)
# 250
# Now, we add the *GSPC_df$GSPC.Adjusted* column to the data frame *ESM23.CME_df*
Ext_ESM23.CME_df <- add_column(.data=ESM23.CME_df, GSPC.Adjusted=GSPC_df$GSPC.Adjusted, .after="ESM23.CME.Adjusted" )
head(Ext_ESM23.CME_df)
# We add a column *Days.to.Maturity*, which accounts for the number of days from the rows of the column 
# *ESM23.CME_df$Date*, to the Future Maturity or Expiration Date *2023-06-16*
Ext_ESM23.CME_df <- add_column(.data=Ext_ESM23.CME_df, 
                            Days.to.Maturity=as.vector(difftime(as.Date(as.character("2023-06-16")), Ext_ESM23.CME_df$Date, units="days")), 
                            .after="GSPC.Adjusted")
head(Ext_ESM23.CME_df)
tail(Ext_ESM23.CME_df)
# We also add the column *Months.to.Maturity* [resp. *Years.to.Maturity*], which accounts for the number of months 
# [resp. years] from the rows of the column *ESM23.CME_df$Date*, to the Future Maturity Date *2023-06-23*.
Ext_ESM23.CME_df <- add_column(.data=Ext_ESM23.CME_df, 
                               Months.to.Maturity=as.vector(Ext_ESM23.CME_df$Days.to.Maturity/30.4369),
                               Years.to.Maturity=as.vector(Ext_ESM23.CME_df$Days.to.Maturity/365.2425),
                               .after="Days.to.Maturity")
head(Ext_ESM23.CME_df)
tail(Ext_ESM23.CME_df)
#
# In the end, we add four columns with the estimated r_t_f and percentage r_t_f, from the formula
# r_t_f = K/S_t - 1,
# and the annualized r_t_f_a and percentage r_t_f_a, from the formula
# r_t_f_a = (1+r_t_f)^(365.2425/t)-1.
#
Ext_ESM23.CME_df <- add_column(.data=Ext_ESM23.CME_df, 
                               Rate.of.Ret.at.Maturity=(Ext_ESM23.CME_df$ESM23.CME.Adjusted/Ext_ESM23.CME_df$GSPC.Adjusted-1),
                               Perc.Rate.of.Ret.at.Maturity=label_percent(accuracy = 0.001)(Ext_ESM23.CME_df$ESM23.CME.Adjusted/Ext_ESM23.CME_df$GSPC.Adjusted-1),
                               Ann.Rate.of.Ret.at.Maturity=((Ext_ESM23.CME_df$ESM23.CME.Adjusted/Ext_ESM23.CME_df$GSPC.Adjusted)^(365.2425/Ext_ESM23.CME_df$Days.to.Maturity)-1),
                               Perc.Ann.Rate.of.Ret.at.Maturity=label_percent(accuracy = 0.001)((Ext_ESM23.CME_df$ESM23.CME.Adjusted/Ext_ESM23.CME_df$GSPC.Adjusted)^(365.2425/Ext_ESM23.CME_df$Days.to.Maturity)-1),
                               .after="Years.to.Maturity")
head(Ext_ESM23.CME_df)
tail(Ext_ESM23.CME_df)
#
save(Ext_ESM23.CME_df, file="Ext_ESM23.CME_2022_04_12__2023_04_11_df.RData")
# rm(Ext_ESM23.CME_df)
head(Ext_ESM23.CME_df)
#
# LOAD
load("Ext_ESM23.CME_2022_04_12__2023_04_11_df.RData")
head(Ext_ESM23.CME_df)
tail(Ext_ESM23.CME_df)
#
# We draw a line plot of the S&P 500 Adjusted price and S&P 500 E-mini Strike Price.
# library(ggplot2)
Data_df <- Ext_ESM23.CME_df
DS_length <- nrow(Data_df)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(last(Data_df$Date))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Line Plot of S&P 500 Adjusted Prices and S&P 500 E-Mini Jun 23 Strike Price, from ", .(First_Day), " to ", .(Last_Day))))
link_GSPC <- "https://finance.yahoo.com/quote/%5EGSPC?p=^GSPC&.tsrc=fin-srch"
link_Ext_ESM23 <- "https://finance.yahoo.com/quote/ES%3DF?p=ES%3DF"
caption_content <- "Author: Roberto Monte"
subtitle_content <- bquote(paste("path length ", .(DS_length), " sample points. Data by courtesy of Yahoo Finance -  ", .(link_GSPC),~~"-"~~.(link_Ext_ESM23)))
x_name <- bquote()
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(DS_length)
x_breaks_num <- 25
x_breaks_low <- first(Data_df$Index)
x_breaks_up <- last(Data_df$Index)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- as.character(Data_df$Date[x_breaks])
J <- 0.0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("S&P 500 - E-Mini Jun 23 - Adj. Prices")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$GSPC.Adjusted),na.rm(Data_df$ESM23.CME.Adjusted))
y_min <- min(na.rm(Data_df$GSPC.Adjusted),na.rm(Data_df$ESM23.CME.Adjusted))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_b <- bquote("S&P 500 Adj. Price")
line_col_r <- bquote("S&P 500 E-Mini Jun 23 Strike Price")
leg_line_labs   <- c(line_col_b, line_col_r)
leg_line_cols   <- c("line_col_b"="blue", "line_col_r"="red")
leg_line_breaks <- c("line_col_b", "line_col_r")
SP_500_EMini_Adj_Prices_lp <- ggplot(Data_df, aes(x=Index)) + 
  geom_line(aes(y=GSPC.Adjusted, color="line_col_b"), linewidth=0.8, linetype="solid") +
  geom_line(aes(y=ESM23.CME.Adjusted, color="line_col_r"), linewidth=0.8, linetype="solid") +
  scale_x_continuous(breaks=x_breaks, labels=x_labs, limits=x_lims) +
  scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
#  xlab("maturity dates (from Apr, 11th, 2023)") + 
  ylab("S&P 500 Adj. Price - E-Mini Jun 23 Strike Price") +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(color="black", size=10, face="italic", angle=-45, vjust=-1),
        axis.text.y=element_text(color="black", size=11, face="italic", angle=0, vjust=0),
        axis.title.x=element_blank(),
#        axis.title.x=element_text(color="black", size=13, face="bold.italic", angle=0, vjust=-2),
        axis.title.y=element_text(color="black", size=13, face="bold.italic", angle=90, vjust=0),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(SP_500_EMini_Adj_Prices_lp)
#
# We draw also a plot of the estimated annualized risk free rate based on spot-futures parity equation with maturity on Jun, 16th, 2023.
# library(ggplot2)
Data_df <- Ext_ESM23.CME_df
DS_length <- nrow(Data_df)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(last(Data_df$Date))
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Line Plot of the Estimated Risk Free Rate from the Spot Futures Parity Equation with Maturity on Jun, 16th, 2023, from ", .(First_Day), " to ", .(Last_Day))))
link_GSPC <- "https://finance.yahoo.com/quote/%5EGSPC?p=^GSPC&.tsrc=fin-srch"
link_Ext_ESM23 <- "https://finance.yahoo.com/quote/ES%3DF?p=ES%3DF"
caption_content <- "Author: Roberto Monte"
subtitle_content <- bquote(paste("path length ", .(DS_length), " sample points. Data by courtesy of Yahoo Finance -  ", .(link_GSPC),~~"-"~~.(link_Ext_ESM23)))
x_name <- bquote()
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(DS_length)
x_breaks_num <- 25
x_breaks_low <- first(Data_df$Index)
x_breaks_up <- last(Data_df$Index)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs_1 <- formatC(Data_df$Months.to.Maturity[x_breaks], format="f", digits=2)
x_labs_2 <- as.character(Data_df$Date[x_breaks])
J <- 0.5
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("annualized yield rate (from Jan, 3rd, 2023 to maturity date)")
y_breaks_num <- 10
y_max <- max(na.rm(as.numeric(sub("%","", Data_df$Perc.Rate.of.Ret.at.Maturity))))
y_min <- min(na.rm(as.numeric(sub("%","", Data_df$Perc.Rate.of.Ret.at.Maturity))))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- floor(y_min/y_binwidth)*y_binwidth
y_breaks_up <- ceiling(y_max/y_binwidth)*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- paste(format(y_breaks, scientific=FALSE),"%",sep="")
K <- 0
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
line_col_b <- bquote("annualized yield rate")
leg_line_labs   <- c(line_col_b)
leg_line_cols   <- c("line_col_b"="blue")
leg_line_breaks <- c("line_col_b")
Risk_Free_Rate_06_16_2023_lp <- ggplot(Data_df, aes(x=Index)) + 
  geom_line(aes(y=as.numeric(sub("%","", Perc.Rate.of.Ret.at.Maturity)), color="line_col_b"), linewidth=0.8, linetype="solid") +
  scale_x_continuous(breaks=x_breaks, labels=x_labs_2, limits=x_lims) +
  scale_y_continuous(breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis= sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("months to maturity Jun, 16th, 2023") + 
  ylab("annual. perc. risk free rate") +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="Legend", labels=leg_line_labs, values=leg_line_cols, breaks=leg_line_breaks) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(color="red", size=10, face="italic", angle=-45, vjust=-1),
        axis.text.y=element_text(color="black", size=10, face="italic", angle=0, vjust=0),
        axis.title.x = element_blank(),
#        axis.title.x=element_text(color="black", size=10, face="bold.italic", angle=0, vjust=3),
        axis.title.y=element_text(color="black", size=10, face="bold.italic", angle=90, vjust=0),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom") +
  coord_cartesian(clip = "off") +
  annotate(geom = "text", size=3.0, x=x_breaks, y=y_breaks_low, label=x_labs_1, vjust=0.7)+
  annotate(geom = "text", size=3.0, x=x_breaks[13], y=y_breaks_low, label="months to maturity Jun, 16th, 2023", 
           hjust=0.0, vjust=-1.5)
plot(Risk_Free_Rate_06_16_2023_lp)
#
##############################################################################################################################
# European Options on Standard & Poor 500 (Yahoo Finance - ^SPX)
# library(quantmod)
# SPX_Opt_2023_05_12_06_16 <- getOptionChain("^SPX", Exp="2023-06-16", src='yahoo')
# class(SPX_Opt_2023_05_12_06_16)
# length(SPX_Opt_2023_05_12_06_16)
# show(SPX_Opt_2023_05_12_06_16[[1]])
# class(SPX_Opt_2023_05_12_06_16[[1]])
# nrow(SPX_Opt_2023_05_12_06_16[[1]])
# show(SPX_Opt_2023_05_12_06_16[[2]])
# class(SPX_Opt_2023_05_12_06_16[[2]])
# nrow(SPX_Opt_2023_05_12_06_16[[2]])
# show(SPX_Opt_2023_05_12_06_16[[1]]$Strike)
# show(SPX_Opt_2023_05_12_06_16[[2]]$Strike)
# Strike <- sort(union(SPX_Opt_2023_05_12_06_16[[1]]$Strike, SPX_Opt_2023_05_12_06_16[[2]]$Strike))
# show(Strike)
# length(Strike)
# Call_Indx <- sapply(Strike, function(x) which(SPX_Opt_2023_05_12_06_16[[1]]$Strike==x)[1])
# Put_Indx <- sapply(Strike, function(x) which(SPX_Opt_2023_05_12_06_16[[2]]$Strike==x)[1])
# 
# SPX_Opt_2023_05_12_06_16_df <- data.frame(Indx=1:length(Strike),
#                                     Call_ContractID=SPX_Opt_2023_05_12_06_16[[1]]$ContractID[Call_Indx], 
#                                     Call_Bid=SPX_Opt_2023_05_12_06_16[[1]]$Bid[Call_Indx],
#                                     Call_Ask=SPX_Opt_2023_05_12_06_16[[1]]$Ask[Call_Indx],
#                                     Call_Vol=SPX_Opt_2023_05_12_06_16[[1]]$Vol[Call_Indx],
#                                     Call_OI=SPX_Opt_2023_05_12_06_16[[1]]$OI[Call_Indx],
#                                     Call_PrChg=SPX_Opt_2023_05_12_06_16[[1]]$Chg[Call_Indx],
#                                     Call_PrChgPct=SPX_Opt_2023_05_12_06_16[[1]]$ChgPct[Call_Indx],
#                                     Call_LastTrTime=SPX_Opt_2023_05_12_06_16[[1]]$LastTradeTime[Call_Indx],
#                                     Call_LastPr=SPX_Opt_2023_05_12_06_16[[1]]$Last[Call_Indx],
#                                     Call_ImplVol=SPX_Opt_2023_05_12_06_16[[1]]$IV[Call_Indx],
#                                     Call_ITM=SPX_Opt_2023_05_12_06_16[[1]]$ITM[Call_Indx],
#                                     Strike=Strike,
#                                     Put_ITM=SPX_Opt_2023_05_12_06_16[[2]]$ITM[Put_Indx],
#                                     Put_ImplVol=SPX_Opt_2023_05_12_06_16[[2]]$IV[Put_Indx],
#                                     Put_LastPr=SPX_Opt_2023_05_12_06_16[[2]]$Last[Put_Indx],
#                                     Put_LastTrTime=SPX_Opt_2023_05_12_06_16[[2]]$LastTradeTime[Put_Indx],
#                                     Put_PrChgPct=SPX_Opt_2023_05_12_06_16[[2]]$ChgPct[Put_Indx],
#                                     Put_PrChg=SPX_Opt_2023_05_12_06_16[[2]]$Chg[Put_Indx],
#                                     Put_OI=SPX_Opt_2023_05_12_06_16[[2]]$OI[Put_Indx],
#                                     Put_Vol=SPX_Opt_2023_05_12_06_16[[2]]$Vol[Put_Indx],
#                                     Put_Ask=SPX_Opt_2023_05_12_06_16[[2]]$Ask[Put_Indx],
#                                     Put_Bid=SPX_Opt_2023_05_12_06_16[[2]]$Bid[Put_Indx],
#                                     Put_ContractID=SPX_Opt_2023_05_12_06_16[[2]]$ContractID[Put_Indx])
# head(SPX_Opt_2023_05_12_06_16_df,10)                                   
# tail(SPX_Opt_2023_05_12_06_16_df,10)
# write.csv(SPX_Opt_2023_05_12_06_16_df,"C:/Users/rober/My Documents - Notebook (local)/My Classes/MPSMF/R-Scripts & Data/SPX_Option_Chain_2023_05_12_06_16.csv")
# dir("C:/Users/rober/My Documents - Notebook (local)/My Classes/MPSMF/R-Scripts & Data")
# rm(SPX_Opt_2023_05_12_06_16_df)
# head(SPX_Opt_2023_05_12_06_16_df)
#
SPX_Opt_2023_04_11_06_16_df <- read.csv("SPX_Option_Chain_2023_04_11_06_16.csv")
class(SPX_Opt_2023_04_11_06_16_df)
head(SPX_Opt_2023_04_11_06_16_df,10)
tail(SPX_Opt_2023_04_11_06_16_df,10)
#
Call_LastTrDate_df <- data.frame(Call_LastTrDate=as.Date(SPX_Opt_2023_04_11_06_16_df$Call_LastTrTime, format="%Y-%m-%d"))
class(Call_LastTrDate_df)
head(Call_LastTrDate_df,20)
nrow(Call_LastTrDate_df)
Call_LastTrDate_tb <- table(Call_LastTrDate_df)   
class(Call_LastTrDate_tb)
show(Call_LastTrDate_tb)
#
Put_LastTrDate_df <- data.frame(Put_LastTrDate=as.Date(SPX_Opt_2023_04_11_06_16_df$Put_LastTrTime, format="%Y-%m-%d"))
class(Put_LastTrDate_df)
head(Put_LastTrDate_df,20)
nrow(Put_LastTrDate_df)
Put_LastTrDate_tb <- table(Put_LastTrDate_df)   
class(Put_LastTrDate_tb)
show(Put_LastTrDate_tb)
#
Call_LastTrDate_2023_04_11_Indx <- SPX_Opt_2023_04_11_06_16_df$Indx[which(Call_LastTrDate_df$Call_LastTrDate=="2023-04-11")]
show(Call_LastTrDate_2023_04_11_Indx)
length(Call_LastTrDate_2023_04_11_Indx)
Put_LastTrDate_2023_04_11_Indx <- SPX_Opt_2023_04_11_06_16_df$Indx[which(Put_LastTrDate_df$Put_LastTrDate=="2023-04-11")]
show(Put_LastTrDate_2023_04_11_Indx)
length(Put_LastTrDate_2023_04_11_Indx)
Call_Put_2023_04_11_Indx <- intersect(Call_LastTrDate_2023_04_11_Indx, Put_LastTrDate_2023_04_11_Indx)
show(Call_Put_2023_04_11_Indx)
length(Call_Put_2023_04_11_Indx)
# 34
#
Call_LastTrDate_2023_04_10_Indx <- SPX_Opt_2023_04_11_06_16_df$Indx[which(Call_LastTrDate_df$Call_LastTrDate=="2023-04-10")]
show(Call_LastTrDate_2023_04_10_Indx)
length(Call_LastTrDate_2023_04_10_Indx)
Put_LastTrDate_2023_04_10_Indx <- SPX_Opt_2023_04_11_06_16_df$Indx[which(Put_LastTrDate_df$Put_LastTrDate=="2023-04-10")]
show(Put_LastTrDate_2023_04_10_Indx)
length(Put_LastTrDate_2023_04_10_Indx)
Call_Put_2023_04_10_Indx <- intersect(Call_LastTrDate_2023_04_10_Indx, Put_LastTrDate_2023_04_10_Indx)
show(Call_Put_2023_04_10_Indx)
length(Call_Put_2023_04_10_Indx)
# 5
#
# Put-Call parity
# P_0 = C_0 - S_0 + K/(1+r_f)
# C_0 - P_0 = S_0 - K/(1+r_f)
#
x <- SPX_Opt_2023_04_11_06_16_df$Strike[Call_Put_2023_04_11_Indx]
show(x)
length(x)
y <- SPX_Opt_2023_04_11_06_16_df$Call_LastPr[Call_Put_2023_04_11_Indx]-SPX_Opt_2023_04_11_06_16_df$Put_LastPr[Call_Put_2023_04_11_Indx]
show(y)
length(y)
#
Data_df <- data.frame(x,y)
nrow(Data_df)
Data_df <- na.omit(Data_df)
nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
rownames(Data_df) <- 1:nrow(Data_df)
nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
n <- nrow(Data_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plot of the Call-Put Difference Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-04-11;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte" 
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(n)
x_breaks_num <- 8
x_breaks_low <- min(Data_df$x)
x_breaks_up <- max(Data_df$x)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0.2
x_lims <- c(x_breaks_low-J*x_binwidth,x_breaks_up+J*x_binwidth)
x_name <- bquote("strike")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- y_min
y_breaks_up <- y_max
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth)
if((y_breaks_up-max(y_breaks))>y_binwidth/2){y_breaks <- c(y_breaks,y_breaks_up)}
y_labs <- format(y_breaks, scientific=FALSE)
y_name <- bquote("call-put difference")
K <- 0.2
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Strike_Pr_2023_04_11_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE, fullrange = FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=FALSE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_ord,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(1.0,"cm"), legend.position="bottom")
plot(Call_Put_Strike_Pr_2023_04_11_06_16_sp)
#
PutCall_par_lm <- lm(y~x)
summary(PutCall_par_lm)
#
S_0 <- PutCall_par_lm$coefficients[1]
show(S_0)
# 4094.027
#
# SPX Market Price 4,108.94 -0.17 (-0.00%) At close: April 11 04:55PM EDT
#
# -1/(1+r_f)=c, -1/c=1+r_f, -(1/c+1)=r_f, 
#
r_f <- -(1/PutCall_par_lm$coefficients[2]+1)
show(r_f)
# 0.01144599
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-04-11"))
show(Days_to_Mat)
# 66
#
r_f_a=(1+r_f)^(365.2425/Days_to_Mat)-1
show(r_f_a)
# 0.06500779
#
label_percent(accuracy = 0.001)(r_f_a)
# 6.501%
#
# Put-Call parity
# P_0 = C_0 - S_0 + K/(1+r_f)
# P_0 - C_0 + S_0 = K/(1+r_f)
# SPX Market Price 4,108.94 -0.17 (-0.00%) At close: April 11 04:55PM EDT
#
S_0 <- 4108.94
#
x <- SPX_Opt_2023_04_11_06_16_df$Strike[Call_Put_2023_04_11_Indx]
show(x)
length(x)
y <- SPX_Opt_2023_04_11_06_16_df$Put_LastPr[Call_Put_2023_04_11_Indx]-SPX_Opt_2023_04_11_06_16_df$Call_LastPr[Call_Put_2023_04_11_Indx]+S_0
show(y)
length(y)
#
Data_df <- data.frame(x,y)
nrow(Data_df)
Data_df <- na.omit(Data_df)
nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
rownames(Data_df) <- 1:nrow(Data_df)
nrow(Data_df)
head(Data_df,10)
tail(Data_df,10)
n <- nrow(Data_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plot of the Put-Call+Price Combination Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-04-11;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte" 
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(n)
x_breaks_num <- 8
x_breaks_low <- min(Data_df$x)
x_breaks_up <- max(Data_df$x)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0.2
x_lims <- c(x_breaks_low-J*x_binwidth,x_breaks_up+J*x_binwidth)
x_name <- bquote("strike")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- y_min
y_breaks_up <- y_max
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth)
if((y_breaks_up-max(y_breaks))>y_binwidth/2){y_breaks <- c(y_breaks,y_breaks_up)}
y_labs <- format(y_breaks, scientific=FALSE)
y_name <- bquote("call-put difference")
K <- 0.2
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Price_Strike_Pr_2023_04_11_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE, fullrange = FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=FALSE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_ord,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(1.0,"cm"), legend.position="bottom")
plot(Call_Put_Price_Strike_Pr_2023_04_11_06_16_sp)
#
PutCallPrice_par_lm <- lm(y~0+x)
summary(PutCallPrice_par_lm)
#
# 1/(1+r_f)=c, 1/c=1+r_f, 1/c-1=r_f, 
#
r_f <- 1/PutCallPrice_par_lm$coefficients[1]-1
show(r_f)
# 0.007666662
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-04-11"))
show(Days_to_Mat)
# 66
#
r_f_a=(1+r_f)^(365.2425/Days_to_Mat)-1
show(r_f_a)
# 0.04317122
#
label_percent(accuracy = 0.001)(r_f_a)
# 4.317%
#
SPX_Opt_2023_05_18_06_16_df <- read.csv("SPX_Option_Chain_2023_05_18_06_16.csv")
class(SPX_Opt_2023_05_18_06_16_df)
head(SPX_Opt_2023_05_18_06_16_df,10)
tail(SPX_Opt_2023_05_18_06_16_df,10)
#
Call_LastTrDate_df <- data.frame(Call_LastTrDate=as.Date(SPX_Opt_2023_05_18_06_16_df$Call_LastTrTime))
class(Call_LastTrDate_df)
head(Call_LastTrDate_df,20)
nrow(Call_LastTrDate_df)
Call_LastTrDate_tb <- table(Call_LastTrDate_df)   
class(Call_LastTrDate_tb)
show(Call_LastTrDate_tb)
#
Put_LastTrDate_df <- data.frame(Put_LastTrDate=as.Date(SPX_Opt_2023_05_18_06_16_df$Put_LastTrTime))
class(Put_LastTrDate_df)
head(Put_LastTrDate_df,20)
nrow(Put_LastTrDate_df)
Put_LastTrDate_tb <- table(Put_LastTrDate_df)   
class(Put_LastTrDate_tb)
show(Put_LastTrDate_tb)
#
Call_LastTrDate_2023_05_18_Indx <- SPX_Opt_2023_05_18_06_16_df$Indx[which(Call_LastTrDate_df$Call_LastTrDate=="2023-05-18")]
show(Call_LastTrDate_2023_05_18_Indx)
length(Call_LastTrDate_2023_05_18_Indx)
Put_LastTrDate_2023_05_18_Indx <- SPX_Opt_2023_05_18_06_16_df$Indx[which(Put_LastTrDate_df$Put_LastTrDate=="2023-05-18")]
show(Put_LastTrDate_2023_05_18_Indx)
length(Put_LastTrDate_2023_05_18_Indx)
Call_Put_2023_05_18_Indx <- intersect(Call_LastTrDate_2023_05_18_Indx, Put_LastTrDate_2023_05_18_Indx)
show(Call_Put_2023_05_18_Indx)
length(Call_Put_2023_05_18_Indx)
# 69
#
Call_LastTrDate_2023_05_17_Indx <- SPX_Opt_2023_05_18_06_16_df$Indx[which(Call_LastTrDate_df$Call_LastTrDate=="2023-05-17")]
show(Call_LastTrDate_2023_05_17_Indx)
length(Call_LastTrDate_2023_05_17_Indx)
Put_LastTrDate_2023_05_17_Indx <- SPX_Opt_2023_05_18_06_16_df$Indx[which(Put_LastTrDate_df$Put_LastTrDate=="2023-05-17")]
show(Put_LastTrDate_2023_05_17_Indx)
length(Put_LastTrDate_2023_05_17_Indx)
Call_Put_2023_05_17_Indx <- intersect(Call_LastTrDate_2023_05_17_Indx, Put_LastTrDate_2023_05_17_Indx)
show(Call_Put_2023_05_17_Indx)
length(Call_Put_2023_05_17_Indx)
# 1
#
# Put-Call parity
# P_0 = C_0 - S_0 + K/(1+r_f)
# C_0-P_0 = S_0 - K/(1+r_f)
#
x <- SPX_Opt_2023_05_18_06_16_df$Strike[Call_Put_2023_05_18_Indx]
show(x)
length(x)
y <- SPX_Opt_2023_05_18_06_16_df$Call_LastPr[Call_Put_2023_05_18_Indx]-SPX_Opt_2023_05_18_06_16_df$Put_LastPr[Call_Put_2023_05_18_Indx]
show(y)
length(y)
#
Data_df <- data.frame(x,y)
n <- nrow(Data_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plot of the Call-Put Difference Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-05-18;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte" 
# To obtain the submultiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(n)
x_breaks_num <- 23
x_breaks_low <- min(Data_df$x)
x_breaks_up <- max(Data_df$x)
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth,x_breaks_up+J*x_binwidth)
x_name <- bquote("strike")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- y_min
y_breaks_up <- y_max
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth)
if((y_breaks_up-max(y_breaks))>y_binwidth/2){y_breaks <- c(y_breaks,y_breaks_up)}
y_labs <- format(y_breaks, scientific=FALSE)
y_name <- bquote("call-put difference")
K <- 1
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Strike_Pr_2023_05_18_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=TRUE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_ord,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(1.0,"cm"), legend.position="bottom")
plot(Call_Put_Strike_Pr_2023_05_18_06_16_sp)
#
PutCall_par_lm <- lm(y~x)
summary(PutCall_par_lm)
#
S_0 <- PutCall_par_lm$coefficients[1]
show(S_0)
# 4189.735
# SPX Market Price 4,198.05 At close: May 18 :55PM EDT
#
r_f <- -(1/PutCall_par_lm$coefficients[2]+1)
show(r_f)
# 0.00211723
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-05-18"))
show(Days_to_Mat)
# 29
#
r_f_a=(1+r_f)^(365.2425/Days_to_Mat)-1
show(r_f_a)
# 0.02699536
#
label_percent(accuracy = 0.001)(r_f_a)
# 2.700%
#
# Put-Call parity
# P_0 = C_0 - S_0 + K/(1+r_f)
# P_0 - C_0 + S_0 = K/(1+r_f)
#
S_0 <- 4198.05
#
x <- SPX_Opt_2023_05_18_06_16_df$Strike[Call_Put_2023_05_18_Indx]
show(x)
length(x)
y <- SPX_Opt_2023_05_18_06_16_df$Put_LastPr[Call_Put_2023_05_18_Indx]-SPX_Opt_2023_05_18_06_16_df$Call_LastPr[Call_Put_2023_05_18_Indx]+S_0
show(y)
length(y)
#
Data_df <- data.frame(x,y)
n <- nrow(Data_df)
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023", 
                             paste("Scatter Plot of the Call-Put Difference Adjusted by the Stock Price Against the Strike Price")))
subtitle_content <- bquote(paste("Data set size",~~.(n),~~"sample points;    Evaluation Date 2023-05-18;   Maturity Date 2023-06-16"))
caption_content <- "Author: Roberto Monte" 
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use
# library(numbers)
# primeFactors(n)
x_breaks_num <- 13
x_breaks_low <- Data_df$x[1]
x_breaks_up <- Data_df$x[n]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((x_breaks_up-max(x_breaks))>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- format(x_breaks, scientific=FALSE)
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth,x_breaks_up+J*x_binwidth)
x_name <- bquote("strike")
y_breaks_num <- 10
y_max <- max(na.rm(Data_df$y))
y_min <- min(na.rm(Data_df$y))
y_binwidth <- round((y_max-y_min)/y_breaks_num, digits=3)
y_breaks_low <- y_min
y_breaks_up <- y_max
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth)
if((y_breaks_up-max(y_breaks))>y_binwidth/2){y_breaks <- c(y_breaks,y_breaks_up)}
y_labs <- format(y_breaks, scientific=FALSE)
y_name <- bquote("call-put difference")
K <- 1
y_lims <- c((y_breaks_low-K*y_binwidth), (y_breaks_up+K*y_binwidth))
col_1 <- bquote("data set sample points")
col_2 <- bquote("regression line")
col_3 <- bquote("LOESS curve")
leg_labs <- c(col_1, col_2, col_3)
leg_cols <- c("col_1"="blue", "col_2"="green", "col_3"="red")
leg_ord <- c("col_1", "col_2", "col_3")
Call_Put_Price_Strike_Pr_2023_05_18_06_16_sp <- ggplot(Data_df, aes(x=x, y=y)) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="dashed", aes(color="col_3"),
              method="loess", formula=y ~ x, se=FALSE) +
  geom_smooth(alpha=1, linewidth=0.8, linetype="solid", aes(color="col_2"),
              method="lm" , formula=y ~ x, se=FALSE, fullrange=TRUE) +
  geom_point(alpha=1, size=1.0, shape=19, aes(color="col_1")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis=sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_ord,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=0, vjust=1),
        legend.key.width=unit(1.0,"cm"), legend.position="bottom")
plot(Call_Put_Price_Strike_Pr_2023_05_18_06_16_sp)
#
PutCallPrice_par_lm <- lm(y~0+x)
summary(PutCallPrice_par_lm)
#
r_f <- 1/PutCallPrice_par_lm$coefficients[1]-1
show(r_f)
# 8.067738e-05
#
Days_to_Mat <- as.vector(difftime("2023-06-16", "2023-05-18"))
show(Days_to_Mat)
# 29
#
r_f_a=(1+r_f)^(365.2425/Days_to_Mat)-1
show(r_f_a)
# 0.001016572
#
label_percent(accuracy = 0.001)(r_f_a)
# 0.102%
#
##############################################################################################################################
##############################################################################################################################