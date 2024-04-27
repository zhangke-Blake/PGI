

rm(list = ls()); gc() 

setwd(paste0(root,"/0_database"))
##----------------------------------------------------------
# data------------------------------------------------------ 
meal_post2h <- openxlsx::read.xlsx("WePrecision_pheno_cgmdata.xlsx", sheet="CGM_post2hour_mealtest", rowNames=F)    
meal_post2h <- meal_post2h[meal_post2h$interv!="OGTT",]
a <- data.frame(table(meal_post2h$id, meal_post2h$interv))
idlist1 <- as.character(a$Var1)[a$Var2=="refined" & a$Freq>1]
idlist2 <- as.character(a$Var1)[a$Var2=="whole" & a$Freq>1]


# distance calculation------------------------------------------------- 
input_data <- data.frame(meal_post2h[,grep("^glu", colnames(meal_post2h))])
rownames(input_data) <- meal_post2h$tag 
dist_total <- as.matrix(vegdist(input_data, method='euclidean', na.rm = T))
dist_total[upper.tri(dist_total)] <- NA

dist_total2 <- reshape2::melt(dist_total)
dist_total2 <- dist_total2[!is.na(dist_total2$value),]
dist_total2 <- dist_total2[dist_total2$value!=0,]
dist_total2$type <- ifelse(substr(dist_total2$Var1,1,6)==substr(dist_total2$Var2,1,6),"intra","inter") 

dist_total2$interv1 <- ifelse(substr(dist_total2$Var1,8,8) %in% c(2,5,6),"RG","WG")
dist_total2$interv2 <- ifelse(substr(dist_total2$Var2,8,8) %in% c(2,5,6),"RG","WG")
dist_total2 <- dist_total2[dist_total2$interv1==dist_total2$interv2,]

dist_RG <- dist_total2[dist_total2$interv1=="RG",]
dist_WG <- dist_total2[dist_total2$interv1=="WG",]

distance_RG <- data.frame() 
for (i in 1:length(idlist1)){
  tar <- idlist1[i]   
  submatr <- dist_RG[substr(dist_RG$Var1,1,6)==tar | substr(dist_RG$Var2,1,6)==tar,]
     
  dist_mean <- aggregate(submatr$value, by=list(submatr$type), mean)
  dist_mean <- data.frame(t(dist_mean))
  names(dist_mean) <- dist_mean[1,]
  dist_mean <- dist_mean[-1,]
  result <- data.frame(id=tar, dist_mean, "interv"="RG")
  distance_RG <- rbind.fill(distance_RG, result)
} 


distance_WG <- data.frame() 
for (i in 1:length(idlist2)){
  tar <- idlist2[i]   
  submatr <- dist_WG[substr(dist_WG$Var1,1,6)==tar | substr(dist_WG$Var2,1,6)==tar,]
  
  dist_mean <- aggregate(submatr$value, by=list(submatr$type), mean)
  dist_mean <- data.frame(t(dist_mean))
  names(dist_mean) <- dist_mean[1,]
  dist_mean <- dist_mean[-1,] 
  result <- data.frame(id=tar, dist_mean, "interv"="WG")
  distance_WG <- rbind.fill(distance_WG, result)
}  

database_distance <- rbind(distance_RG, distance_WG)
database_distance$inter <- as.numeric(database_distance$inter)
database_distance$intra <- as.numeric(database_distance$intra)

database_dist.RG <- database_distance[database_distance$interv=="RG",]
database_dist.WG <- database_distance[database_distance$interv=="WG",]


# stat: intra-/inter-distance-------------------------------------
stat <- wilcox.test(database_dist.RG$inter, database_dist.RG$intra, paired=F)
result1 <- data.frame(interv="RG", pval=stat$p.value, 
                      mean_inter=mean(database_dist.RG$inter), sd_inter=sd(database_dist.RG$inter),
                      mean_intra=mean(database_dist.RG$intra), sd_intra=sd(database_dist.RG$intra), 
                      method=stat$method)
stat <- wilcox.test(database_dist.WG$inter, database_dist.WG$intra, paired=F)
result2 <- data.frame(interv="WG", pval=stat$p.value, 
                      mean_inter=mean(database_dist.WG$inter), sd_inter=sd(database_dist.WG$inter),
                      mean_intra=mean(database_dist.WG$intra), sd_intra=sd(database_dist.WG$intra), 
                      method=stat$method)
output_stat <- rbind(result1, result2)

##---------------------------------------------------------- 



## Comparison of glucose responses to foods  (Increasing rate, decreasing rate, iAUC)
setwd(paste0(root,"/0_database"))
##----------------------------------------------------------------------------
# data---------------------- 
input_data1 <- openxlsx::read.xlsx("WePrecision_pheno_cgmdata.xlsx", sheet="CGM_postwhole_mealtest", rowNames=F)
meal_feat <- openxlsx::read.xlsx("WePrecision_pheno_cgmdata.xlsx", sheet="CGM_feature_mealtest", rowNames=F)
input_data2 <- openxlsx::read.xlsx("WePrecision_pheno_cgmdata.xlsx", sheet="OGTT_postprandial", rowNames=F)
ogtt_feat <- openxlsx::read.xlsx("WePrecision_pheno_cgmdata.xlsx", sheet="OGTT_feature", rowNames=F)


input_data1$IR <- NA # increasing rate per hour
input_data1$DR <- NA # decreasing rate per hour
for(i in 1:nrow(input_data1)){
  sub <- input_data1[i,!is.na(input_data1[i,])]
  g <- grep("glu", colnames(sub))
  
  start <- sub$glu1
  end <- sub[,paste0("glu",length(g))]
  
  max <- max(sub[1,g])
  max.pos <- which(sub[1,g]==max)
  IR <- (max-start)/((max.pos[1]-1)*15/60)
  DR <- (max-end)/((length(g)-max.pos[length(max.pos)])*15/60)
  
  input_data1$IR[i] <- IR
  input_data1$DR[i] <- DR
  
}
output_mealfeat <- merge(input_data1[,c("tag","id","IR","DR")], meal_feat[,c("tag","ppgr","ppge")], by="tag", all.x=T)


input_data2$IR <- NA # increasing rate per hour
input_data2$DR <- NA # increasing rate per hour
for(i in 1:nrow(input_data2)){
  sub <- input_data2[i,!is.na(input_data2[i,])]
  g <- grep("glu", colnames(sub))
  
  start <- sub$glu1
  end <- sub[,paste0("glu",length(g))]
  
  max <- max(sub[1,g])
  max.pos <- which(sub[1,g]==max)
  IR <- (max-start)/((max.pos[1]-1)*15/60)
  DR <- (max-end)/((length(g)-max.pos[length(max.pos)])*15/60)
  
  input_data2$IR[i] <- IR
  input_data2$DR[i] <- DR
  
}
output_ogttfeat <- merge(input_data2[,c("tag","id","IR","DR")], ogtt_feat[,c("tag","ppgr","ppge")], by="tag", all.x=T)


output_glufeat <- rbind(output_mealfeat, output_ogttfeat)
output_glufeat$date_tag <- substr(output_glufeat$tag, 8, str_length(output_glufeat$tag))

output_glufeat$interv <- NA
output_glufeat$interv[output_glufeat$date_tag=="2"|output_glufeat$date_tag=="5"|output_glufeat$date_tag=="6"] <- "RG"
output_glufeat$interv[output_glufeat$date_tag=="3"|output_glufeat$date_tag=="4"|output_glufeat$date_tag=="7"] <- "WG"
output_glufeat$interv[output_glufeat$date_tag=="8"|output_glufeat$date_tag=="13"] <- "OGTT"
output_glufeat$interv[output_glufeat$date_tag=="9"|output_glufeat$date_tag=="10"|output_glufeat$date_tag=="11"|output_glufeat$date_tag=="12"] <- "toast"

output_glufeat <- output_glufeat[!is.na(output_glufeat$interv),] 

# stat----------------------
output_comp_meal <- data.frame()
idlist <- unique(output_glufeat$id)

for(i in 1:length(idlist)){
  submatr <- output_glufeat[output_glufeat$id==idlist[i],]  
  RG <- submatr[submatr$interv=="RG",]
  WG <- submatr[submatr$interv=="WG",]
  
  result <- data.frame(id=idlist[i], mean_IR.RG=NA, mean_IR.WG=NA, pval.IR=NA, resp.IR=NA,
                       mean_DR.RG=NA, mean_DR.WG=NA, pval.DR=NA, resp.DR=NA,
                       mean_ppge.RG=NA, mean_ppge.WG=NA, pval.ppge=NA, resp.ppge=NA,
                       mean_ppgr.RG=NA, mean_ppgr.WG=NA, pval.ppgr=NA, resp.ppgr=NA)
  
  # for IR
  result$mean_IR.RG <- mean(RG$IR, na.rm=T); n1 <- sum(!is.na(RG$IR))
  result$mean_IR.WG <- mean(WG$IR, na.rm=T); n2 <- sum(!is.na(WG$IR))
  result$resp.IR <- ifelse(result$mean_IR.RG>result$mean_IR.WG, "RG","WG")
  
  if(n1>1 & n2>1){
    stat.IR <- t.test(RG$IR, WG$IR)
    result$pval.IR <- stat.IR$p.value
  }
  
  # for DR 
  result$mean_DR.RG <- mean(RG$DR, na.rm=T); n1 <- sum(!is.na(RG$DR))
  result$mean_DR.WG <- mean(WG$DR, na.rm=T); n2 <- sum(!is.na(WG$DR))
  result$resp.DR <- ifelse(result$mean_DR.RG>result$mean_DR.WG, "RG","WG")
  
  if(n1>1 & n2>1){
    stat.DR <- t.test(RG$DR, WG$DR)
    result$pval.DR <- stat.DR$p.value
  }
  
  # for PPGE
  result$mean_ppge.RG <- mean(RG$ppge, na.rm=T); n1 <- sum(!is.na(RG$ppge))
  result$mean_ppge.WG <- mean(WG$ppge, na.rm=T); n2 <- sum(!is.na(WG$ppge))
  result$resp.ppge <- ifelse(result$mean_ppge.RG>result$mean_ppge.WG, "RG","WG")
  
  if(n1>1 & n2>1){
    stat.ppge <- t.test(RG$ppge, WG$ppge)
    result$pval.ppge <- stat.ppge$p.value
  }
  
  # for PPGR
  result$mean_ppgr.RG <- mean(RG$ppgr, na.rm=T); n1 <- sum(!is.na(RG$ppgr))
  result$mean_ppgr.WG <- mean(WG$ppgr, na.rm=T); n2 <- sum(!is.na(WG$ppgr))
  result$resp.ppgr <- ifelse(result$mean_ppgr.RG>result$mean_ppgr.WG, "RG","WG")
  
  if(n1>1 & n2>1){
    stat.ppgr <- t.test(RG$ppgr, WG$ppgr)
    result$pval.ppgr <- stat.ppgr$p.value
  }
  
  output_comp_meal <- rbind.fill(output_comp_meal, result)
}

output_comp_meal$resp.IR[output_comp_meal$pval.IR>0.05 | is.na(output_comp_meal$pval.IR)] <- NA
output_comp_meal$resp.DR[output_comp_meal$pval.DR>0.05 | is.na(output_comp_meal$pval.DR)] <- NA
output_comp_meal$resp.ppge[output_comp_meal$pval.ppge>0.05 | is.na(output_comp_meal$pval.ppge)] <- NA
output_comp_meal$resp.ppgr[output_comp_meal$pval.ppgr>0.05 | is.na(output_comp_meal$pval.ppgr)] <- NA


##---------------------------------------------------------------------------- 



## Create PGI and model comparison in WPN1
setwd(paste0(root,"/0_database"))
##------------------------------------------------------------- 
# data ------------------------------------------
cgmfeat <- openxlsx::read.xlsx("WePrecision_pheno_cgmdata.xlsx", sheet="CGM_feature_mealtest", rowNames=F)
Food_nutrient <- openxlsx::read.xlsx("WePrecision_pheno_cgmdata.xlsx", sheet="meal_nutrient", rowNames=F)
ogtt_feat <- openxlsx::read.xlsx("WePrecision_pheno_cgmdata.xlsx", sheet="OGTT_feature", rowNames=F)

# 以GL和PPGR构建Personalized拟合函数
fx_ppgr <- data.frame()
GLlist <- as.numeric(Food_nutrient$GL_meal)

matrix1 <- cgmfeat[cgmfeat$date %in% c(2:7,9:12),c("tag","ppgr")]
matrix2 <- ogtt_feat[,c("tag","ppgr")]
matrix <- rbind(matrix1,matrix2)
matrix$date <- substr(matrix$tag,8,length(matrix$tag))
matrix$id <- substr(matrix$tag,1,6)

# 去除轻断食后的OGTT
matrix <- matrix[matrix$date!="13",]

# 分组
matrix$point <- NA
matrix$point[matrix$date==2|matrix$date==5|matrix$date==6] <- 3
matrix$point[matrix$date==3|matrix$date==4|matrix$date==7] <- 2
matrix$point[matrix$date %in% c(as.character(9:12))] <- 1
matrix$point[matrix$date %in% c("8")] <- 4

idlist <- unique(matrix$id)
for (i in 1:length(idlist)){
  submatrix <- matrix[matrix$id==idlist[i],]
  p1 <- grep(1, submatrix$point)
  if(length(p1)>0){point1 <- data.frame(t(submatrix$ppgr[submatrix$point==1])); names(point1) <- paste("p1_",c(1:ncol(point1)), sep="")
  }else{point1 <- data.frame(p1_1=NA)}
  
  p2 <- grep(2, submatrix$point)
  if(length(p2)>0){point2 <- data.frame(t(submatrix$ppgr[submatrix$point==2])); names(point2) <- paste("p2_",c(1:ncol(point2)), sep="")
  }else{point2 <- data.frame(p2_1=NA)}
  
  p3 <- grep(3, submatrix$point)
  if(length(p3)>0){point3 <- data.frame(t(submatrix$ppgr[submatrix$point==3])); names(point3) <- paste("p3_",c(1:ncol(point3)), sep="")
  }else{point3 <- data.frame(p3_1=NA)}
  
  p4 <- grep(4, submatrix$point)
  if(length(p4)>0){point4 <- data.frame(t(submatrix$ppgr[submatrix$point==4])); names(point4) <- paste("p4_",c(1:ncol(point4)), sep="")
  }else{point4 <- data.frame(p4_1=NA)}
  
  sub2 <- data.frame(id=idlist[i])
  sub2 <- cbind(sub2,point1,point2, point3, point4)
  
  fx_ppgr <- rbind.fill(fx_ppgr, sub2)
}


p0 <- data.frame(id=rep(fx_ppgr$id,2))
p0$variable <- "p0"
p0$value <- 0

input_data <- reshape2::melt(fx_ppgr)
input_data <- rbind(input_data,p0)

input_data$group <- substr(input_data$variable,1,2)

# 匹配 meal_GL
input_data$gl <- NA
input_data$gl[input_data$group=="p0"] <- 0
input_data$gl[input_data$group=="p1"] <- GLlist[6]
input_data$gl[input_data$group=="p2"] <- GLlist[3]
input_data$gl[input_data$group=="p3"] <- GLlist[1]
input_data$gl[input_data$group=="p4"] <- GLlist[5]

input_data <- input_data[!is.na(input_data$value),]


## 根据个体数据拟合不同函数
fx_ppgr$p0_1 <- 0
fx_ppgr$p0_2 <- 0
output_line_index <- data.frame()
# stat for linear model----------------------------
idlist <- unique(fx_ppgr$id)
for (i in 1:length(idlist)){
  submatrix <- fx_ppgr[fx_ppgr$id==idlist[i],]
  sub2 <- reshape::melt(submatrix); sub2 <- sub2[,-1]
  sub2$variable <- substr(sub2$variable,1,2)
  
  if (sum(!is.na(sub2$value))>5){
    # 转换成PPGR~GL
    sub2$gl[sub2$variable=="p0"] <- 0
    sub2$gl[sub2$variable=="p1"] <- GLlist[6]
    sub2$gl[sub2$variable=="p2"] <- GLlist[3]
    sub2$gl[sub2$variable=="p3"] <- GLlist[1]
    sub2$gl[sub2$variable=="p4"] <- GLlist[5]
    sub2 <- sub2[order(sub2$variable),]
    
    y <- sub2$value
    x <- sub2$gl
    fit_mod <- lm(y ~ x-1, data=sub2) # 拟合直线，“-1”表示强制过原点
    
    fit_mod2 <- summary(fit_mod)
    result <- data.frame(id=idlist[i], a=fit_mod2$coefficients[1,"Estimate"], a_pval=fit_mod2$coefficients[1,4], 
                         rsqua=fit_mod2$r.squared, df=fit_mod2$df[2])
    
    output_line_index <- rbind(output_line_index, result)
  }
}

output_line_index$model <- "line2P"

 
##-------------------------------------------------------------  





 






























