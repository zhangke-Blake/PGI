 
library(haven)#for reading dta file
library(readr)#for reading csv file
library(readxl)
library(openxlsx)
library(xlsx)
library(dplyr)
library(plyr) # 不等长合并
library(tidyverse)
library(reshape2)
library(lubridate)#for deal with time information
library(base)#for dealing with string 
library(car)#for diagnose reg model 
library(openxlsx)
library(ggplot2)
# library(ggtree)
library(ggrepel)
library(RColorBrewer)
library(ggpubr)# for add p value and signif
# library(rgr)
library(data.table)
library(lightgbm)
library(pROC)
library(R.matlab)
library(mlogit)
library(pheatmap)
library(Hmisc)
library(vegan)



rm(list = ls())



##--------------------------------------------- Postprandial glycemic responses (2h) ---------------------------------------------##

##--------------------------------------------- Postprandial glycemic responses (2h) ---------------------------------------------##



##  Postprandial glycemic responses (2h) 
##--------------------------------------------------------------------- 
output_glu1 <- data.frame() 
output_glu2 <- data.frame()

for(i in 1:nrow(rawdata_list)){
  
  cat(paste("############################ indiv", i, "##############################\n"))
  
  filepath <- paste(rawdir, rawdata_list$filename[i], sep="/")
  raw <- read.table(filepath, sep="\t",header=F, encoding='UTF-8', fill = T)
  
  # sort raw dataset
  name <- raw[1,1]
  id <- substr(rawdata_list$filename[i],1,6)
  raw <- raw[-c(1:2),-c(1,3)]
  names(raw) <- c("time","glu")
  
  startdate <- standID$date[standID$id==id]
   
  
    if(length(startdate)!=0){
      
      # step1: reform the date and time into uniform day and timepoints ---------------------------------------------------------------
      a <- data.frame(t(data.frame(strsplit(raw$time, split = " ")))); names(a) <- c("date","point")
      b <- data.frame(t(data.frame(strsplit(a$point, split = ":")))); names(b) <- c("hour","min")
      b$hour <- as.numeric(b$hour); b$min <- as.numeric(b$min)
      
      raw$date <- as.Date(a$date,"%Y/%m/%d")
      raw$start <- startdate
      raw$date_tag <- as.numeric(difftime(raw$date, raw$start,units="days")+1) 
      raw$point_tag <- as.numeric(round((b$hour*60+b$min)/15)+1) 
      raw$tag <- paste(id, raw$date_tag, sep = "_")
      raw$glu <- as.numeric(raw$glu)
    

      # remove the data on the first day and of which TBR > 99%
      point.check <- data.frame(table(raw$date))
      rmlist.date <- as.character(point.check$Var1[which(point.check$Freq<96)][1])
      
      raw$check[raw$glu<3.9] <- 1
      LPT.check <- aggregate(raw$check, by=list(raw$date), sum, na.rm=T)
      LPT.check$n_date <- table(raw$date_tag)
      LPT.check$ratio <- LPT.check$x/LPT.check$n_date
      rmlist.date2 <- as.character(LPT.check$Group.1[LPT.check$ratio>0.99])
      
      rmlist <- c(rmlist.date,rmlist.date2)
      rmlist <- as.Date(rmlist, "%Y-%m-%d")
      
      if (length(rmlist)>0){
        raw <- raw[-which(raw$date %in% rmlist),]
      }
      
      
      # step2: label for blood glucose amplitude ranges -------------------------------------------------------------------------- 
      glu_matr <- raw[,c("tag","glu","date_tag","point_tag")]
      glu_matr$diff <- NA
      glu_matr$diff[1] <- -1
      for (g in 2:nrow(glu_matr)){
        diff <- glu_matr$glu[g]-glu_matr$glu[g-1]
        if (diff>=0){
          glu_matr$diff[g] <- 1
        }else if (diff<0){
          glu_matr$diff[g] <- -1 
        }
      }
      
      glu_mealtime <- mealtime_break[mealtime_break$id==id,]
      glu_mealtime <- glu_mealtime[!duplicated(glu_mealtime$tag),]
      
      glu_mix <- merge(glu_matr, glu_mealtime[,c("tag","break_time")], by.x = 'tag', by.y = 'tag', all.x = T)
      glu_mix <- glu_mix[order(glu_mix$point_tag),]
      glu_mix <- glu_mix[order(glu_mix$date_tag),]
      
      
      # separate the date for OGTT 
      datelist <- c(1,2,3,4,5,6,7,9,10,11,12)
      glu_mix <- glu_mix[which(glu_mix$date_tag %in% datelist),]
      max_row <- nrow(glu_mix)
      
      
      # step3: find for blood glucose amplitude ranges  --------------------------------------------------------------------------
      # refill the eating time
      breaktime.mean <- round(mean(glu_mix$break_time, na.rm=T)) 
      if (is.na(breaktime.mean)){
        breaktime.mean <- 30 # 7:30 as start 
      }  
      glu_mix$break_time[is.na(glu_mix$break_time)] <- breaktime.mean
      
    
          # narrow down the range according to the time for first bite --------------------------------------------------------------------------
          rowlist <- which(glu_mix$point_tag==glu_mix$break_time)
           
          for(j in 1:length(rowlist)){ 
            cat(paste("##------- day", j, "-------##\n")) 
              
            startpoint <- rowlist[j]
            endpoint <- rowlist[j]+8
             
            if (endpoint <= max_row){
              glu <- glu_mix[c(startpoint:endpoint),]  
              row_peak <- which(glu$glu==max(glu$glu)) 
              
              peak_final <- glu[row_peak[1],]
              
              
              #######################################################
              # check the position of peak point
              peak1 <- glu$glu[row_peak[1]]
              row_peak1 <- which(glu_mix$tag==glu$tag[row_peak[1]] & glu_mix$point_tag==glu$point_tag[row_peak[1]])
              peak2 <- glu$glu[row_peak[length(row_peak)]]
              row_peak2 <- which(glu_mix$tag==glu$tag[row_peak[length(row_peak)]] & glu_mix$point_tag==glu$point_tag[row_peak[length(row_peak)]])
              
              check1 <- row_peak1-1; if (check1==0) {check1=1}
              check2 <- row_peak2+1
              
              if (check1==1){
                condition1=T
              }else{
                condition1 <- peak1>glu_mix[check1,"glu"]
              } 
              condition2 <- peak2>glu_mix[check2,"glu"]
              
              # if not, enlarge the range
              if (condition1==F){ 
                startpoint_re <- startpoint-4
                endpoint_re <- startpoint_re+7
                glu_re <- glu_mix[c(startpoint_re:endpoint_re),]  
                row_peak_re <- which(glu_re$glu==max(glu_re$glu))  
                
                peak_final <- glu_re[row_peak_re[1],] 
              }
              
              if (condition2==F){ 
                endpoint_re <- endpoint+4
                if (endpoint_re>max_row){
                  endpoint_re <- max_row
                }
                
                startpoint_re <- endpoint_re-7
                glu_re <- glu_mix[c(startpoint_re:endpoint_re),] 
                row_peak_re <- which(glu_re$glu==max(glu_re$glu)) 
                
                peak_final <- glu_re[row_peak_re[length(row_peak_re)],]
              }
              #######################################################
              # find the lowest point according to the location of peak point
              row_peakfinal <- which(glu_mix$tag==peak_final$tag & glu_mix$point_tag==peak_final$point_tag)
              if (row_peakfinal>4){
                startpoint_low <- row_peakfinal-4
              } else if (row_peakfinal<=4){
                startpoint_low <- 1
              }  
              
              glu <- glu_mix[c(startpoint_low:row_peakfinal),] 
              row_low <- which(glu$glu==min(glu$glu))
              low_final <- glu[row_low[length(row_low)],]
              row_lowfinal <- which(glu_mix$tag==low_final$tag & glu_mix$point_tag==low_final$point_tag)
              
              
              
              #----------------- part1: 2 hour range ------------------------#
              startpoint_fluc <- row_lowfinal
              endpoint_fluc <- startpoint_fluc+8
              if (endpoint_fluc>max_row){
                endpoint_fluc <- max_row
              }
              
              glu_fluc <- glu_mix[c(startpoint_fluc:endpoint_fluc),]
              
              tag <- unique(glu_fluc$tag)
              break_start <- glu_fluc$point_tag[1]
              break_end <- glu_fluc$point_tag[nrow(glu_fluc)] 
              
              glu_val <- data.frame(t(as.numeric(glu_fluc$glu)))
              names(glu_val) <- paste("glu", c(1:ncol(glu_val)), sep = "")
              peak <- max(glu_val[1,])
              
              output1 <- data.frame(tag=tag, break_start=break_start, break_end=break_end,  peak)
              output1 <- cbind(output1, glu_val)
              output_glu1 <- rbind.fill(output_glu1, output1)
              
              
              
              
              #----------------- part2: the whole range ------------------------#
              startpoint_fluc <- row_lowfinal
              
              #######################################################
              # check: 
              condition3 <- glu_mix$diff[startpoint_fluc]==1
              if (condition3==F){
                startpoint_fluc.re <- startpoint_fluc + which(glu_mix$diff[startpoint_fluc:nrow(glu_mix)]==1)[1]-1 
                startpoint_fluc <- startpoint_fluc.re
              }
              
              #######################################################
              
              a <- which(glu_mix$diff[startpoint_fluc:nrow(glu_mix)]==-1)
              peakpoint_fluc <- startpoint_fluc + a[1]-1
              if(is.na(peakpoint_fluc)){
                peakpoint_fluc <- max_row
              }
               
              start <- glu_mix[startpoint_fluc,"glu"]
              minpoint_fluc <- peakpoint_fluc + which(glu_mix$glu[peakpoint_fluc:nrow(glu_mix)]<=start)[1]-1 
              turnpoint_fluc <- peakpoint_fluc + which(glu_mix$diff[peakpoint_fluc:nrow(glu_mix)]==1)[1]-2
              
              comp <- c(minpoint_fluc, turnpoint_fluc); comp <- comp[!is.na(comp)]
              if(length(comp)==0){
                endpoint_fluc <- nrow(glu_mix)
              } else {
                endpoint_fluc <- min(comp)
              }
              
              
              glu_fluc <- glu_mix[c(startpoint_fluc:endpoint_fluc),]
              
              tag <- unique(glu_fluc$tag)
              break_start <- glu_fluc$point_tag[1]
              break_end <- glu_fluc$point_tag[nrow(glu_fluc)]
              duration <- (break_end - break_start)/4
              
              glu_val <- data.frame(t(as.numeric(glu_fluc$glu)))
              names(glu_val) <- paste("glu", c(1:ncol(glu_val)), sep = "")
              peak <- max(glu_val[1,])
              
              output2 <- data.frame(tag=tag, break_start=break_start, break_end=break_end, duration=duration, peak)
              output2 <- cbind(output2, glu_val)
              output_glu2 <- rbind.fill(output_glu2, output2)
            }

          }
          
          
      }
}
##--------------------------------------------------------------------- 



## calculate iAUC， AGE， IR, and DR
##--------------------------------------------------------------------- 
cgm_data <- output_final1 
cgm_data2 <- cgm_data[,apply(is.na(cgm_data),2,sum)<nrow(cgm_data)]

output_feat <- data.frame()
output <- data.frame()

g <- grep("glu", colnames(cgm_data2))
for (i in 1:nrow(cgm_data2)){
  submatr <- cgm_data2[i,]
  n <- length(submatr)
  glu <- as.numeric(submatr[,g])
  
  peak=submatr$peak
  peak.num=max(which(glu==peak))
  
  start.num=which.min(glu[1:peak.num])
  start=glu[start.num]
  
  ppge=peak-start
  if (peak.num-start.num>0){
    glu_acc=ppge*4/(peak.num-start.num) 
  } else { glu_acc <- 0 }
  
  
  # iAUC-0-2h----------------------------
  glu2 <- glu[!is.na(glu)]
  glu2 <- glu2[c(start.num:length(glu2))]
   
  check <- which(glu2<glu2[1])
  if(length(check)>0) {glu2 <- glu2[1:(check[1]-1)]}
  
  
  n2=length(glu2)
  area.sum <- 0
  for (j in 1:(n2-1)) {
    area.j <- ((glu2[j]+glu2[j+1])*1/4)/2 
    area.sum <- area.sum + area.j
  }
  
  area.sum2 <- area.sum - (n2-1)*glu2[1]*1/4
  ppgr <- area.sum2
  
  # output features 
  output <- data.frame(tag=submatr$tag, peak=peak, ppgr=ppgr, ppge=ppge, glu_acc)
  output_feat <- rbind.fill(output_feat, output)
}

##--------------------------------------------------------------------- 








##--------------------------------------------- Daily traits ---------------------------------------------##

##--------------------------------------------- Daily traits ---------------------------------------------##

rm(list = ls())

library(devtools)
library(dplyr) 
library(CGMTSA)
library(imputeTS)
library(TSA)
 



## extract daily traits-1
##------------------------------------------------------------------------------------
#' Preprocess CGM data.
#' @param inputdir Path of input directory containing CGM files.
#' @param outputdir Path of output directory where preprocessed CGM data will be stored.
#' @param outlierdet Logical. If TRUE the outliers will be detected.
#' @param interval The interval of CGM data. (every 15 min record a value)
#' @param imputation Logical. If TRUE the missing data will be imputed.
#' @param imethod The method that will be used to impute missing data.
#' @param maxgap If the missing gap is greater than max gap, the missing gap will be ignore when imputed.
#' @param compeleteday Logical. If TRUE the day with missing data will be filtered.
#' @param removeday Logical. If TRUE the day with missing gap greater than maxgap will be filtered.
#' @param device Device type: 0 (manual format); 1 (Abbott libre freestyle); 2 (Medtronic ipro2); 3 (Dexcom G6), default 0.
#' @param transunits Logical. If TURE the glucose values will be divided by 18.
#' @param removeflday Logical. If TRUE the data of first and last day will be filter.

outputdir="path_outputdir"
dir.create(outputdir)

prepro(inputdir="path_inputdir", 
       outputdir=outputdir, 
       outlierdet = TRUE, 
       interval = 15, 
       imputation = TRUE, 
       immethod = "linear", 
       maxgap = 60, 
       compeleteday = F, 
       removeday = FALSE, 
       device = 1, 
       transunits = FALSE, 
       removeflday = TRUE)



#' Calculate the metrics of CGM data.
#' @param inputdir Path of input directory containing CGM files.
#' @param outputdir Path of output directory where metrics will be stored.
#' @param useig Logical. If TRUE the imputed data will be used.
#' @param diffnum The number of differencing.
#' @param threshold The threshold of sd used in MAGE calculation.
#' @param bthreshold The minimum glucose of tir range.
#' @param athreshold The maximum glucose of tir range.
#' @param interval The interval of CGM data.

outputdir="path_output"
dir.create(outputdir)

cgmmetrics(inputdir="path_inputdir", 
           outputdir=outputdir ,
           useig = TRUE, 
           threshold =1, 
           bthreshold = 3.9, 
           athreshold = 10, 
           interval = 15)



## sorting the CGM metrics
rawdata_list <- data.frame(dir(rawdir)); names(rawdata_list) <- "filename"

cgmfeat_daily <- c()
for (i in 1:nrow(rawdata_list)){
  
  cat(paste("############################ indiv", i, "##############################\n"))
  
  raw <- read.csv(rawdata_list[i,1])
  id <- substr(rawdata_list[i,1],1,6)
  
  raw$Day <- as.Date(raw$Day,"%Y-%m-%d")
  raw$startdate <- standID$date[standID$id==id]
  raw$date_tag <- as.numeric(difftime(raw$Day, raw$startdate,units="days")+1)
  
  raw$tag <- paste(id, raw$date_tag, sep = "_")
  
  cgmfeat_daily <- rbind(cgmfeat_daily, raw)
  
}

cgmfeat_daily <- cgmfeat_daily[,-1]
 
##------------------------------------------------------------------------------------




## extract daily traits-2
##------------------------------------------------------------------------------------
output_daily <- data.frame()

for(i in 1:nrow(rawdata_list)){
  
  cat(paste("############################ indiv", i, "##############################\n"))
  
  filepath <- paste(rawdir, rawdata_list$filename[i], sep="/")
  raw <- read.table(filepath, sep="\t",header=F, encoding='UTF-8', fill = T)
  
  # 整理数据集
  name <- raw[1,1]
  id <- substr(rawdata_list$filename[i],1,6)
  raw <- raw[-c(1:2),-c(1,3)]
  names(raw) <- c("time","glu")
  
  startdate <- standID$date[standID$id==id]
   
  if(length(startdate)!=0){
     
    a <- data.frame(t(data.frame(strsplit(raw$time, split = " ")))); names(a) <- c("date","point")
    b <- data.frame(t(data.frame(strsplit(a$point, split = ":")))); names(b) <- c("hour","min")
    b$hour <- as.numeric(b$hour); b$min <- as.numeric(b$min)
    
    raw$date <- as.Date(a$date,"%Y/%m/%d")
    raw$start <- startdate
    raw$date_tag <- as.numeric(difftime(raw$date, raw$start,units="days")+1)  
    raw$point_tag <- as.numeric(round((b$hour*60+b$min)/15)+1)  
    raw$tag <- paste(id, raw$date_tag, sep = "_")
    raw$glu <- as.numeric(raw$glu)
     
    point.check <- data.frame(table(raw$date))
    rmlist.date <- as.character(point.check$Var1[which(point.check$Freq<96)][1])
    
    raw$check[raw$glu<3.9] <- 1
    LPT.check <- aggregate(raw$check, by=list(raw$date), sum, na.rm=T)
    LPT.check$n_date <- table(raw$date_tag)
    LPT.check$ratio <- LPT.check$x/LPT.check$n_date
    rmlist.date2 <- as.character(LPT.check$Group.1[LPT.check$ratio>0.99])
    
    rmlist <- c(rmlist.date,rmlist.date2)
    rmlist <- as.Date(rmlist, "%Y-%m-%d")
    
    if (length(rmlist)>0){
      raw <- raw[-which(raw$date %in% rmlist),]
    }
    
     
    raw2 <- raw[,c("tag","glu","date_tag")]
    daylist <- unique(raw2$date_tag)
    
    for (d in 1:length(daylist)){
      submatr <- raw2[raw2$date_tag==daylist[d],]
      
      tpt <- nrow(submatr)
      
      hpt <- sum(submatr$glu>10)
      mpt <- sum(submatr$glu>=3.9 & submatr$glu<=10)
      lpt <- sum(submatr$glu<3.9)
       
      HPT <- hpt/tpt
      MPT <- mpt/tpt
      LPT <- lpt/tpt
      
      result <- data.frame(tag=unique(submatr$tag), 
                           HPT=HPT, 
                           MPT=MPT,
                           LPT=LPT,
                           date_tag=daylist[d]) 
      output_daily <- rbind.fill(output_daily, result)
    } 
  }
}

##------------------------------------------------------------------------------------































