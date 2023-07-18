#### References ################################################################################################################################
################################################################################################################################################
#
# Marek Rutkowski - The CRR Market Model
# Slides for MATH3075/3975 Financial Mathematics - Semester 2, 2016 
# School of Mathematics and Statistics - Univerity of Sidney
#
# Rob J Hyndman and George Athanasopoulos - Forecasting: Principles and Practice
# Monash Univeristy, Australia
# https://otexts.com/fpp2/

# Robert H. Shumway, David S. Stoffer - Time Series Analysis and Its AP_POlications (with R Examples) 4th Edition
# SPringer Texts in Statistics - SPringer Verlag
# https://www.stat.pitt.edu/stoffer/tsa4/tsa4.pdf

# R - Residual Diagnostic
# https://cran.r-project.org/web/packages/olsrr/vignettes/residual_diagnostics.html
################################################################################################################################################
# System setting
################################################################################################################################################

# Removing all items in Global Environment
rm(list=ls())

# Clearing all Plots
# try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)

# Clearing the Console
cat(rep("\n",100))

# Loading the system directory
Sys.getenv('PATH')

# Setting the working directory
WD <- dirname(rstudioapi::getSourceEditorContext()$path)
show(WD)
setwd(WD)
dir()
################################################################################################################################################
# Libraries
################################################################################################################################################

# Reading Libraries

library(timeDate)
library(timeSeries)
library(fBasics)
library(fOptions)
library(tibble)
library("data.table")
library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(urca)
library(moments)
library(lmtest)
################################################################################################################################################

################################################################################################################################################
# Model Setting
################################################################################################################################################
r <- 0.05
u <- 1 + 0.15
d <- 1 - 0.05
p <- (1+r-d)/(u-d)
q <- (u-(1+r))/(u-d)
S_0 <- 100
K <- 100
N <- 5

################################################################################################################################################
S <- matrix(NA, nrow=N+1, ncol = N+1)
S[1,1] <- S_0
show(S)

# First Procedure
for(n in 1:N){
  for(k in 0:n){S[k+1,n+1] <- S_0*u^(n-k)*d^k}
}
show(S)

BinomialTreePlot(S)

# Second Procedure
S <- matrix(NA, nrow=N+1, ncol = N+1)
S[1,1] <- S_0
for(n in 1:N){
  for(k in 0:n){S[n+1,k+1] <- round(S_0*u^k*d^(n-k),3)}
}
show(S)
# S_mat <- cbind(seq(0,N, by=1),S)
# show(S_mat)
S_df <- as.data.frame(S)
# S_df <- add_column(S_df, Index=seq(0,N, by=1), .before="V1")
# S_vec <- rep(NA, (N+1)*(N+2)/2)
# S_vec[1] <- S_0
# j <-1 
# for(n in 1:N){
#   for(k in 0:n){S_vec[(j*n)+k+1] <- S_0*u^(n-k)*d^k}
# j=j+1}
# show(S_vec)
# S_vec <- as.vector(na.omit(S_vec))
# show(S_vec)

# library("data.table")
S_tb <- setDT(S_df)   
class(S_tb)
head(S_tb)

# library(reshape2)
S_rsh_df <- melt(S_tb, na.rm=FALSE)
show(S_rsh_df[1:20,])

# We add an Index identifying variable to the data frame S_rsh_df
# library(tidyverse)
# library(dplyr)
S_mod_rsh_df <- subset(S_rsh_df, select = -variable)
S_mod_rsh_df <- rename(S_mod_rsh_df, S_value = value)
head(S_mod_rsh_df,15)
S_mod_rsh_df <- add_column(S_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(S_df)), .before="S_value")
head(S_mod_rsh_df,15)
# We are finally in a position to draw a draft plot of the price lattice
# library(ggplot2)

Data_df <- S_mod_rsh_df
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for Stock Price in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),")."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put current payoffs")
leg_labs <- c(y1_txt)
leg_vals <- c("y1_txt"="black")
leg_sort <- c("y1_txt")
S_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(S_lattice_sp)

################################################################################################################################################
# Assume K=S_0=100 and consider the pay off of an American put option we have

AP_PO <- matrix(NA, nrow=N+1, ncol = N+1)
AP_PO[1,1] <- 0
for(n in 1:N){
  for(k in 0:N){AP_PO[n+1,k+1] <- round(max(100-S[n+1,k+1],0),3)}
}
show(AP_PO)

AP_PO_df <- as.data.frame(AP_PO)
# library("data.table")
AP_PO_tb <- setDT(AP_PO_df)   
class(AP_PO_tb)
head(AP_PO_tb)
# library(reshape2)
AP_PO_rsh_df <- melt(AP_PO_tb, na.rm=FALSE)
show(AP_PO_rsh_df[1:20,])
AP_PO_mod_rsh_df <- subset(AP_PO_rsh_df, select = -variable)
AP_PO_mod_rsh_df <- rename(AP_PO_mod_rsh_df, AP_PO_value=value)
head(AP_PO_mod_rsh_df,15)
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(AP_PO_df)), .before="AP_PO_value")
head(AP_PO_mod_rsh_df,15)
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, S_value=S_mod_rsh_df$S_value, .before="AP_PO_value")
head(AP_PO_mod_rsh_df,15)

Data_df <- AP_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Put Option - Current Payoffs in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put current payoffs")
leg_labs <- c(y1_txt, y2_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red")
leg_sort <- c("y1_txt", "y2_txt")
S_AP_PO_lattice_sp <- ggplot(Data_df, aes(Index, S_value, group=factor(Index))) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE) + 
  geom_text(aes(label=round(AP_PO_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(S_AP_PO_lattice_sp)
################################################################################################################################################
AP_EP <- matrix(NA, nrow=N+1, ncol = N+1)
AP_EP[N+1,] <- AP_PO[N+1,]
for(n in N:1){
  for(k in 0:n){AP_EP[n,k] <- round((1/(1+r))*(q*AP_EP[n+1,k]+p*AP_EP[n+1,k+1]),3)}
}
AP_EP[N+1,] <- rep(0,N+1)
show(AP_EP)

AP_EP_df <- as.data.frame(AP_EP)
# library("data.table")
AP_EP_tb <- setDT(AP_EP_df)   
class(AP_EP_tb)
head(AP_EP_tb)
# library(reshape2)
AP_EP_rsh_df <- melt(AP_EP_tb, na.rm=FALSE)
show(AP_EP_rsh_df[1:20,])
AP_EP_mod_rsh_df <- subset(AP_EP_rsh_df, select = -variable)
AP_EP_mod_rsh_df <- rename(AP_EP_mod_rsh_df, AP_EP_value=value)
AP_EP_mod_rsh_df <- add_column(AP_EP_mod_rsh_df, Index=rep(0:(nrow(AP_EP_df)-1), times=ncol(AP_EP_df)), .before="AP_EP_value")
show(AP_EP_mod_rsh_df[1:20,])

AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, AP_EP_value=AP_EP_mod_rsh_df$AP_EP_value, .after="AP_PO_value")
show(AP_PO_mod_rsh_df[1:20,])

# library(ggplot2)
Data_df <- AP_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Put Option - Current Payoffs and Expected Payoffs in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put current payoffs")
y3_txt <- bquote("put expected payoffs")
leg_labs <- c(y1_txt, y2_txt, y3_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt")
S_AP_PO_AP_EP_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE) + 
  geom_text(aes(label=round(AP_PO_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE) + 
  geom_text(aes(label=round(AP_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm = TRUE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(S_AP_PO_AP_EP_lattice_sp)
#
################################################################################################################################################
AP_PO_mod_rsh_df <- add_column(AP_PO_mod_rsh_df, AP_MV_value=pmax(AP_PO_mod_rsh_df$AP_PO_value, AP_PO_mod_rsh_df$AP_EP_value, na.rm=TRUE), .after="AP_EP_value")
show(AP_PO_mod_rsh_df[1:20,])

Data_df <- AP_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Put Option - Current Payoffs, Expected Payoffs, and Market Values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put current payoffs")
y3_txt <- bquote("put expected payoffs")
y4_txt <- bquote("put market values")
leg_labs <- c(y1_txt, y2_txt, y3_txt, y4_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue", "y4_txt"="magenta")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt", "y4_txt")
S_AP_PO_AP_EP_AP_MV_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm=TRUE) + 
  geom_text(aes(label=round(AP_PO_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm=TRUE) + 
  geom_text(aes(label=round(AP_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm=TRUE) + 
  geom_text(aes(label=round(AP_MV_value,3), colour="y4_txt"), hjust=-0.2, vjust=-0.7, na.rm = TRUE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(S_AP_PO_AP_EP_AP_MV_lattice_sp)
################################################################################################################################################

# Still assume K=S_0=100 and consider an American call option we have

ACP <- matrix(NA, nrow=N+1, ncol = N+1)
ACP[1,1] <- 0
for(n in 1:N){
  for(k in 0:N){ACP[n+1,k+1] <- round(max(S[n+1,k+1]-100,0),3)}
}
show(ACP)

ACP_df <- as.data.frame(ACP)
# library("data.table")
ACP_tb <- setDT(ACP_df)   
class(ACP_tb)
head(ACP_tb)
# library(reshape2)
ACP_rsh_df <- melt(ACP_tb, na.rm=FALSE)
show(ACP_rsh_df[1:20,])
ACP_mod_rsh_df <- subset(ACP_rsh_df, select = -variable)
ACP_mod_rsh_df <- rename(ACP_mod_rsh_df, ACP_value=value)
show(ACP_mod_rsh_df[1:20,])
ACP_mod_rsh_df <- add_column(ACP_mod_rsh_df, Index=rep(0:(nrow(S_df)-1), times=ncol(ACP_df)), .before="ACP_value")
show(ACP_mod_rsh_df[1:20,])
ACP_mod_rsh_df <- add_column(ACP_mod_rsh_df, S_value=S_mod_rsh_df$S_value, .before="ACP_value")
show(ACP_mod_rsh_df[1:20,])

Data_df <- ACP_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Call Option - Current Payoffs in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("call current payoffs")
leg_labs <- c(y1_txt, y2_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red")
leg_sort <- c("y1_txt", "y2_txt")
S_ACP_lattice_sp <- ggplot(Data_df, aes(Index, S_value, group=factor(Index))) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE) + 
  geom_text(aes(label=round(ACP_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(S_ACP_lattice_sp)
#
################################################################################################################################################
AC_EP <- matrix(NA, nrow=N+1, ncol = N+1)
AC_EP[N+1,] <- ACP[N+1,]
for(n in N:1){
  for(k in 0:n){AC_EP[n,k] <- round((1/(1+r))*(q*AC_EP[n+1,k]+p*AC_EP[n+1,k+1]),3)}
}
show(AC_EP)

AC_EP_df <- as.data.frame(AC_EP)
# library("data.table")
AC_EP_tb <- setDT(AC_EP_df)   
class(AC_EP_tb)
head(AC_EP_tb)
# library(reshape2)
AC_EP_rsh_df <- melt(AC_EP_tb, na.rm=FALSE)
show(AC_EP_rsh_df[1:20,])
AC_EP_mod_rsh_df <- subset(AC_EP_rsh_df, select = -variable)
AC_EP_mod_rsh_df <- rename(AC_EP_mod_rsh_df, AC_EP_value=value)
AC_EP_mod_rsh_df <- add_column(AC_EP_mod_rsh_df, Index=rep(0:(nrow(AC_EP_df)-1), times=ncol(AC_EP_df)), .before="AC_EP_value")
show(AC_EP_mod_rsh_df[1:20,])

ACP_mod_rsh_df <- add_column(ACP_mod_rsh_df, AC_EP_value=AC_EP_mod_rsh_df$AC_EP_value, .after="ACP_value")
show(ACP_mod_rsh_df[1:20,])

# library(ggplot2)
Data_df <- ACP_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Call Option - Current Payoffs and Expected Payoffs in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("call current payoffs")
y3_txt <- bquote("call expected payoffs")
leg_labs <- c(y1_txt, y2_txt, y3_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt")
S_ACP_AC_EP_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm = TRUE) + 
  geom_text(aes(label=round(ACP_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm = TRUE) + 
  geom_text(aes(label=round(AC_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm = TRUE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(S_ACP_AC_EP_lattice_sp)

ACP_mod_rsh_df <- add_column(ACP_mod_rsh_df, ACMV_value=pmax(ACP_mod_rsh_df$ACP_value, ACP_mod_rsh_df$AC_EP_value, na.rm=TRUE), .after="AC_EP_value")
show(AP_PO_mod_rsh_df[1:20,])

Data_df <- ACP_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Call Option - Current Payoffs, Expected Payoffs, and Market Values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put current payoffs")
y3_txt <- bquote("put expected payoffs")
y4_txt <- bquote("put market values")
leg_labs <- c(y1_txt, y2_txt, y3_txt, y4_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue", "y4_txt"="magenta")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt", "y4_txt")
S_ACP_AC_EP_ACMV_lattice_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.7, na.rm=TRUE) + 
  geom_text(aes(label=round(ACP_value,3), colour="y2_txt"), hjust=1.0, vjust=1.3, na.rm=TRUE) + 
  geom_text(aes(label=round(AC_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.3, na.rm=TRUE) + 
  geom_text(aes(label=round(ACMV_value,3), colour="y4_txt"), hjust=-0.2, vjust=-0.7, na.rm = TRUE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  xlab("time") + 
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="bottom")
plot(S_ACP_AC_EP_ACMV_lattice_sp)


################################################################################################################################################

Data_df <- AP_PO_mod_rsh_df
length <- N
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Corso di Metodi Probabilistici e Statistici per i Mercati Finanziari", 
                             "Example of Lattice Plot for American Put and Call Options - Current Payoffs, Expected Payoffs, and Market values in CRR Model"))
subtitle_content <- bquote(paste("market periods N = ", .(N), ", risk free rate r = ", .(r), ", up factor u = ",.(u), ", down factor d = ",.(d), ", risk neutral probability distribution (p,q) = (",.(p),",",.(q),"), exercise price K = ",.(K),"."))
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
x_name <- bquote("time")
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put current payoffs")
y3_txt <- bquote("put expected payoffs")
y4_txt <- bquote("put market values")
leg_labs <- c(y1_txt, y2_txt, y3_txt, y4_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue", "y4_txt"="magenta")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt", "y4_txt")
S_AP_PO_AP_EP_AP_MV_lattice_bis_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.3, na.rm=TRUE) + 
  geom_text(aes(label=round(AP_PO_value,3), colour="y2_txt"), hjust=1.0, vjust=1.2, na.rm=TRUE) + 
  geom_text(aes(label=round(AP_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.2, na.rm=TRUE) + 
  geom_text(aes(label=round(AP_MV_value,3), colour="y4_txt"), hjust=-0.2, vjust=-0.3, na.rm = TRUE) + 
  ggtitle(title_content) +
  labs(subtitle=subtitle_content) +
  scale_x_continuous(name="", breaks=waiver(), labels=NULL, limits=c(0,5.2)) + 
  scale_y_continuous(name=y_name, breaks=waiver(), labels=waiver()
                     # labels=NULL,
                     #sec.axis = sec_axis(~.)
                     , limits=c(70,210)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
# plot(S_AP_PO_AP_EP_AP_MV_lattice_bis_sp)

Data_df <- ACP_mod_rsh_df
length <- N
caption_content <- "Author: Roberto Monte"
y_breaks_num <- 4
y_margin <- 5
y_breaks_low <- floor(min(Data_df$S_value, na.rm =TRUE))-y_margin
y_breaks_up <- ceiling(max(Data_df$S_value, na.rm =TRUE))+y_margin
y_breaks <- seq(from=y_breaks_low, to=y_breaks_up, length.out=y_breaks_num)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0
y_lims <- c((y_breaks_low-K*y_margin), (y_breaks_up+K*y_margin))
x_name <- bquote("time")
y_name <- bquote("stock values")
y1_txt <- bquote("stock values")
y2_txt <- bquote("put current payoffs")
y3_txt <- bquote("put expected payoffs")
y4_txt <- bquote("put market values")
leg_labs <- c(y1_txt, y2_txt, y3_txt, y4_txt)
leg_vals <- c("y1_txt"="black", "y2_txt"="red", "y3_txt"="blue", "y4_txt"="magenta")
leg_sort <- c("y1_txt", "y2_txt", "y3_txt", "y4_txt")
S_ACP_AC_EP_ACMV_lattice_bis_sp <- ggplot(Data_df, aes(Index, S_value)) + 
  geom_point(na.rm = TRUE, colour="black") +
  geom_text(aes(label=round(S_value,3), colour="y1_txt"), hjust=1.0, vjust=-0.3, na.rm=TRUE) + 
  geom_text(aes(label=round(ACP_value,3), colour="y2_txt"), hjust=1.0, vjust=1.2, na.rm=TRUE) + 
  geom_text(aes(label=round(AC_EP_value,3), colour="y3_txt"), hjust=-0.2, vjust=1.2, na.rm=TRUE) + 
  geom_text(aes(label=round(ACMV_value,3), colour="y4_txt"), hjust=-0.2, vjust=-0.3, na.rm = TRUE) + 
  labs(caption=caption_content) +
  scale_x_continuous(name=x_name, breaks=waiver(), labels=waiver(), limits=c(0,5.2)) + 
  scale_y_continuous(name=y_name, breaks=waiver(), labels=waiver()
                     # labels=NULL,
                     #sec.axis = sec_axis(~.)
                     , limits=c(70,210)) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_vals, breaks=leg_sort) +
  theme(axis.text.x = element_text(angle=0, vjust=1),
        legend.key.width = unit(0.80,"cm"), legend.position="right")
# plot(S_ACP_AC_EP_ACMV_lattice_bis_sp)

# library(gridExtra)
grid.arrange(S_AP_PO_AP_EP_AP_MV_lattice_bis_sp,S_ACP_AC_EP_ACMV_lattice_bis_sp, nrow=2, ncol=1, heights=c(1.5,1.2))
# grid.arrange(S_AP_PO_AP_EP_AP_MV_lattice_bis_sp,S_ACP_AC_EP_ACMV_lattice_bis_sp, nrow=2, ncol=1, heights=c(1.5,1.2))
# grid.arrange(S_AP_PO_AP_EP_AP_MV_lattice_bis_sp,S_ACP_AC_EP_ACMV_lattice_bis_sp, nrow=2, ncol=1)

# library(cowplot)
plot_grid(S_AP_PO_AP_EP_AP_MV_lattice_bis_sp,S_ACP_AC_EP_ACMV_lattice_bis_sp, nrow=2, ncol=1, rel_heights=c(0.55,0.45))

# library(gtable)
#################################################################################################################################################

Omega <- matrix(NA, nrow=N, ncol = 2^N)
for(n in 1:N){
  for(k in 1:2^N){if ((k-1) %% (2^n) < 2^(n-1)) {Omega[N+1-n,k] <- 0} else {Omega[N+1-n,k] <- 1} 
  }}
show(Omega)

AP_MV <- pmax(AP_PO, AP_EP, na.rm=TRUE)
show(AP_MV)

OS <- matrix(NA, nrow=N+1, ncol=2^N)
OS[N+1,] <- rep(N,2^N)
for(n in N:2){
  for(m in 1:2^N){if(AP_MV[n,(sum(Omega[,m][1:(n-1)])+1)]==max(AP_PO[n,(sum(Omega[,m][1:(n-1)])+1)],0)){OS[n,m]<-n-1}
  else {OS[n,m]<- OS[n+1,m]}}
}
if(AP_MV[1,1]==AP_PO[1,1]){OS[1,]<-rep(0,2^N)} else {OS[1,]<-OS[2,]}
show(OS)

#################################################################################################################################################
