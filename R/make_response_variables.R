####PELAGIC RESPONSE----

#' read pelagic size for response
#'
#' @return dat
#' @export
#'
#' @examples
#' 
read_size_pelagic <- function(){
  
  dat <- read.csv(here::here("outputs", "merged_fl_pelagic_meta.csv"), row.names = 1, header = TRUE)
  dat <- na.omit(dat)
  
  #return(dat)

} #end of function



#' Make mean, median and maximum pelagic response
#'
#'
#' @param dat body size
#' @return mean median and maximum values for pelagic sizes
#' @export 
#'
#' @examples
#' 
#' 
#' 
mean_median_max_pelagic <- function(dat){

exp <- unique(dat$Exped)
taille <- 0

for (i in exp) {
  
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  taille=taille+length(day)
}

maxsize = as.data.frame(matrix(NA,nrow=taille,ncol=9))
colnames(maxsize) = c("Exped", "Date", "mean_lat","mean_long","mean_maxsize","std_maxsize","cv_maxsize","median_maxsize","string_number")

meansize=as.data.frame(matrix(NA,nrow=taille,ncol=9))
colnames(meansize) = c("Exped", "Date", "mean_lat","mean_long","mean_meansize","std_meansize","cv_meansize","median_meansize","string_number")

mediansize=as.data.frame(matrix(NA,nrow=taille,ncol=9))
colnames(mediansize)=c("Exped", "Date", "mean_lat","mean_long","mean_mediansize","std_mediansize","cv_mediansize","median_mediansize","string_number")

# sub=dat[dat$String=="AIG_01",]

j=0

for (i in exp) {
  
  cat(i, "\n")
  
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  for (l in day) {
    sub2=sub[sub$Date==l,]
    
    j=j+1
    
    if(dim(sub2)[[1]]>=1)
    {
      
      cam=unique(sub2$string)
      
      m=0
      x=0
      y=0
      lat=0
      long=0
      n=0
      
      for (k in cam)
      {
        
        sub3=sub2[sub2$string==k,]
        
        if(dim(sub3)[[1]]>=1)
        {
          n=n+1
          m=c(m,max(sub3$weight_kg))
          x=c(x,median(sub3$weight_kg))
          y=c(y,mean(sub3$weight_kg))
          lat=c(lat,sub3$lat_in)
          long=c(long,sub3$lon_in)
        }
        
      }
      
      m=m[-1]
      x=x[-1]
      y=y[-1]
      
      lat=lat[-1]
      long=long[-1]
      
      maxsize[j,1] = i
      maxsize[j,2] = l
      maxsize[j,3]=mean(lat)
      maxsize[j,4]=mean(long)
      maxsize[j,5]=mean(m)
      maxsize[j,6]=sd(m)
      maxsize[j,7]=maxsize[j,6]/maxsize[j,5]
      maxsize[j,8]=median(m)
      maxsize[j,9]=n
      
      
      mediansize[j,1] = i
      mediansize[j,2] = l
      mediansize[j,3]=mean(lat)
      mediansize[j,4]=mean(long)
      mediansize[j,5]=mean(x)
      mediansize[j,6]=sd(x)
      mediansize[j,7]=mediansize[j,6]/mediansize[j,5]
      mediansize[j,8]=median(x)
      mediansize[j,9]=n
      
      meansize[j,1] = i
      meansize[j,2] = l
      meansize[j,3]=mean(lat)
      meansize[j,4]=mean(long)
      meansize[j,5]=mean(y)
      meansize[j,6]=sd(y)
      meansize[j,7]=meansize[j,6]/meansize[j,5]
      meansize[j,8]=median(y)
      meansize[j,9]=n
      
      
  
    } # end of if
    
  } #end of day 
  
} # end of exp


maxsize$Date <- as.character(as.Date(as.character(maxsize$Date), format = "%Y-%m-%d"))
maxsize$key <- paste(maxsize$Exped, maxsize$Date, sep = "__")

meansize$Date <- as.character(as.Date(as.character(meansize$Date), format = "%Y-%m-%d"))
meansize$key <- paste(meansize$Exped, meansize$Date, sep = "__")

mediansize$Date <- as.character(as.Date(as.character(mediansize$Date), format = "%Y-%m-%d"))
mediansize$key <- paste(mediansize$Exped, mediansize$Date, sep = "__")

meansize   <- meansize[ , c(5:8, 10)]
mediansize <- mediansize[ , c(5:8, 10)]


dat2 <- merge(maxsize, meansize, by = "key", all = TRUE)
dat2 <- merge(dat2, mediansize, by = "key", all = TRUE)

return(dat2)

}#end of function






#' Make pelagic modes
#'
#' @param dat 
#'
#' @return dat3
#' @export
#' @import multimode tidyr
#'
#' @examples
#' 
#' 
#' 

modes_pelagic <- function(dat, dat2){

exp=unique(dat$Exped)

length(exp)

day=unique(dat$Date)

length(day)

taille=0

for (i in exp)
{
  
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  taille=taille+length(day)
  
}

modes=as.data.frame(matrix(NA,nrow=taille,ncol=9))

# rownames(maxsize)=day

colnames(modes)=c('Exped', 'Date', "mean_lat","mean_long", "excess_mass", "p_value","unimode", "bimode1","bimode2")

j=0

for (i in exp)
{
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  for (l in day)
  {
    
    sub2=sub[sub$Date==l,]
    
    j=j+1
    
    if(dim(sub2)[[1]]>=20)
    {
      a=modetest(sub2$weight_kg)
      
      b=locmodes(sub2$weight_kg,1)
      uni=b$locations[1]
      
      if(a$p.value<0.05)
      {  
        c=locmodes(sub2$weight_kg,2)
        modes[j,8]=c$locations[1]
        modes[j,9]=c$locations[3]
        
      }
      modes[j,1] <- i
      modes[j,2] <- l
      modes[j,3]=mean(sub2$lat_in)
      modes[j,4]=mean(sub2$lon_in)
      modes[j,5]=a$statistic
      modes[j,6]=a$p.value
      modes[j,7]=uni
      
    }
    
  }
  
}


#dim(modes)
#summary(modes)
#hist(modes[,"unimode"])
#hist(log10(modes[,"unimode"]+1))
#hist(modes[,"bimode1"])
#hist(modes[,"bimode2"])
#write.table(modes,file="modes_20indmin_pelagic.txt")

modes <- modes %>% drop_na(Exped)

#return(modes)

modes$Date <- as.character(as.Date(as.character(modes$Date), format = "%Y-%m-%d"))
modes$key <- paste(modes$Exped, modes$Date, sep = "__")

modes      <- modes[ , c(5:10)]

colnames(modes) = c('excess_mass', 'modes_p_value','unimode','first_mode', 'second_mode', 'key')

dat3 <- merge(dat2, modes, by = "key", all = TRUE)

return(dat3)

}#end of function


#' Make beta slope
#' @param dat mean median and max responses
#' @return dat3
#' @import tidyr
#' @export
#'
#' @examples
#' 
#' 
#' 
beta_slope_pelagic <- function(dat, dat2){


day=unique(dat$Date)

taille <- 0

res=as.data.frame(matrix(NA,nrow=taille,ncol=8))

colnames(res)=c('Exped', 'Date', "mean_lat","mean_long","betaslope","inf CI","sup CI","String Number")

# sub=dat[dat$String=="AIG_01",]

# eightMethodsMEE(sub$weight_kg,b.only = T)

exp <- unique(dat$Exped)

j=0

for (i in exp)
{
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  for (l in day)
  {
    
    sub2=sub[sub$Date==l,]
    
    j=j+1
    
    if(dim(sub2)[[1]]>=20)
    {
      mod = eightMethodsMEE_mod(sub2$weight_kg,b.only = TRUE)
      slope = mod$hMLE
      
      res[j,1] <- i
      res[j,2] <- l
      res[j,3]=mean(sub2$lat_in)
      res[j,4]=mean(sub2$lon_in)
      res[j,5]=slope[1]
      res[j,6]=slope[2]
      res[j,7]=slope[3]
      
      strn=unique(sub2$string)
      res[j,8]=length(strn)
    }
  }
}

res = na.omit(res)
res = res[res$betaslope <  1, ]
res = res[res$betaslope > -4, ]

res$Date <- as.character(as.Date(as.character(res$Date), format = "%Y-%m-%d"))
res$key <- paste(res$Exped, res$Date, sep = "__")

res <- res[ , c(5:7, 9)]

colnames(res) = c('betaslope', 'betaslope_ci_inf', 'betaslope_ci_sup', 'key')

dat3 <- merge(dat2, res, by = "key", all = TRUE)

dat3 <- dat3 %>% drop_na(Date)

return(dat3)

}#end of function






# BENTHIC RESPONSE----


#' Read benthic sizes
#'
#' @return dat
#' @export
#'
#' @examples
#' 
read_size_benthic <- function(dat){

dat <- read.csv(here::here("outputs", "merged_fl_benthic_meta.csv"),header=TRUE,row.names=1)

dat=na.omit(dat)

}


#' Title
#' @param dat benthic sizes
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 
mean_median_max_benthic <- function(dat){

#dim(dat)
length(unique((dat$Binomial)))
exp=unique(dat$Exped)
#length(exp)
day=unique(dat$Date)
#length(day)

taille=0

for (i in exp)
{
  
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  taille=taille+length(day)
  
}

maxsize=as.data.frame(matrix(NA,nrow=taille,ncol=9))

# rownames(maxsize)=day

colnames(maxsize) = c("Exped", "Date", "mean_lat","mean_long","mean_maxsize","std_maxsize","cv_maxsize","median_maxsize","BRUVS_number")

meansize=as.data.frame(matrix(NA,nrow=taille,ncol=9))

# rownames(meansize)=day

colnames(meansize) = c("Exped", "Date", "mean_lat","mean_long","mean_meansize","std_meansize","cv_meansize","median_meansize","BRUVS_number")

# rownames(mediansize)=day

mediansize=as.data.frame(matrix(NA,nrow=taille,ncol=9))

colnames(mediansize)=c("Exped", "Date", "mean_lat","mean_long","mean_mediansize","std_mediansize","cv_mediansize","median_mediansize","BRUVS_number")




j=0

for (i in exp)
{
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  for (l in day)
  {
    sub2=sub[sub$Date==l,]
    
    j=j+1
    
    if(dim(sub2)[[1]]>=1)
    {
      
      cam=unique(sub2$NewOpCode)
      
      m=0
      x=0
      y=0
      lat=0
      long=0
      
      for (k in cam)
      {
        
        sub3=sub2[sub2$NewOpCode==k,]
        
        if(dim(sub3)[[1]]>=1)
        {
          m=c(m,max(sub3$weight_kg))
          x=c(x,median(sub3$weight_kg))
          y=c(y,mean(sub3$weight_kg))
          lat=c(lat,sub3$lat_in)
          long=c(long,sub3$lon_in)
          
        }
        
      }
      
      m=m[-1]
      x=x[-1]
      y=y[-1]
      lat=lat[-1]
      long=long[-1]
      
      maxsize[j,1]=i
      maxsize[j,2]=l 
      maxsize[j,3]=mean(lat)
      maxsize[j,4]=mean(long)
      maxsize[j,5]=mean(m)
      maxsize[j,6]=sd(m)
      maxsize[j,7]=maxsize[j,6]/maxsize[j,5]
      maxsize[j,8]=median(m)
      maxsize[j,9]=length(cam)
      
      mediansize[j,1]=i
      mediansize[j,2]=l
      mediansize[j,3]=mean(lat)
      mediansize[j,4]=mean(long)
      mediansize[j,5]=mean(x)
      mediansize[j,6]=sd(x)
      mediansize[j,7]=mediansize[j,6]/mediansize[j,5]
      mediansize[j,8]=median(x)
      mediansize[j,9]=length(cam)
      
      meansize[j,1]=i
      meansize[j,2]=l
      meansize[j,3]=mean(lat)
      meansize[j,4]=mean(long)
      meansize[j,5]=mean(x)
      meansize[j,6]=sd(x)
      meansize[j,7]=meansize[j,6]/meansize[j,5]
      meansize[j,8]=median(x)
      meansize[j,9]=length(cam)
      

      
    } #end of if 
    
  } #end of day 
  
} # end of exp


maxsize$Date <- as.character(as.Date(as.character(maxsize$Date), format = "%Y-%m-%d"))
maxsize$key <- paste(maxsize$Exped, maxsize$Date, sep = "__")

meansize$Date <- as.character(as.Date(as.character(meansize$Date), format = "%Y-%m-%d"))
meansize$key <- paste(meansize$Exped, meansize$Date, sep = "__")

mediansize$Date <- as.character(as.Date(as.character(mediansize$Date), format = "%Y-%m-%d"))
mediansize$key <- paste(mediansize$Exped, mediansize$Date, sep = "__")

meansize   <- meansize[ , c(5:8, 10)]
mediansize <- mediansize[ , c(5:8, 10)]

dat2 <- merge(maxsize, meansize, by = "key", all = TRUE)
dat2 <- merge(dat2, mediansize, by = "key", all = TRUE)

return(dat2)

#summary(maxsize)
#summary(meansize)

#write.table(maxsize,file="medianmaxsize_benthic.txt")
#write.table(meansize,file="medianmeansize_benthic.txt")

}#end of function


#' Make benthic modes
#'
#' @return
#' @export
#' @import multimode tidyr

#'
#'
#' 
modes_benthic <- function(dat, dat2){

#dim(dat)
length(unique((dat$Binomial)))
exp=unique(dat$Exped)
#length(exp)
day=unique(dat$Date)
#length(day)
  
taille=0
  
modes=as.data.frame(matrix(NA,nrow=taille,ncol=9))

colnames(modes)=c('Exped', 'Date', "mean_lat","mean_long", "excess_mass", "p_value","unimode","bimode1","bimode2")


j=0

for (i in exp)
{
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  for (l in day)
  {
    
    sub2=sub[sub$Date==l,]
    
    j=j+1
    
    if(dim(sub2)[[1]]>=20)
    {
      a=multimode::modetest(sub2$weight_kg)
      
      b=multimode::locmodes(sub2$weight_kg,1)
      uni=b$locations[1]
      
      if(a$p.value<0.05)
      {  
        c=multimode::locmodes(sub2$weight_kg,2)
        
        modes[j,8]=c$locations[1]
        modes[j,9]=c$locations[3]
      }
      
      modes[j,1] <- i
      modes[j,2] <- l
      modes[j,3]=mean(sub2$lat_in)
      modes[j,4]=mean(sub2$lon_in)
      modes[j,5]=a$statistic
      modes[j,6]=a$p.value
      modes[j,7]=uni
    }
    
  }
  
}


modes <- modes %>% tidyr::drop_na(Exped)

modes$Date <- as.character(as.Date(as.character(modes$Date), format = "%Y-%m-%d"))
modes$key <- paste(modes$Exped, modes$Date, sep = "__")

modes      <- modes[ , c(5:10)]
colnames(modes) = c('excess_mass', 'modes_p_value','unimode','first_mode', 'second_mode', 'key')



dat3 <- merge(dat2, modes, by = "key", all = TRUE)

dat3 <- dat3 %>% drop_na(Date)


return(dat3)
#write.table(modes,file="modes_20indmin_benthic.txt")

#hist(modes[,"unimode"])

#hist(modes[,"bimode1"])

#hist(modes[,"bimode2"])

}#end of function



#' beta_slope_benthic
#'
#' @param dat 
#' @param dat2 
#'
#' @return data_to_export
#' @export
#'
#' @examples
#' 
beta_slope_benthic <- function(dat, dat2) {

  
#dim(dat)
length(unique((dat$Binomial)))
exp=unique(dat$Exped)
#length(exp)
day=unique(dat$Date)
#length(day)
  
taille=0
  
# eightMethodsMEE(dat$Length..cm.,b.only = T)$hMLE

res=as.data.frame(matrix(NA,nrow=taille,ncol=8))

colnames(res)=c('Exped', 'Date', "mean_lat","mean_long","betaslope","betaslope_ci_inf","betaslope_ci_sup","BRUVS_number")


j=0

for (i in exp)
{
  sub=dat[dat$Exped==i,]
  
  day=unique(sub$Date)
  
  for (l in day)
  {
    
    sub2=sub[sub$Date==l,]
    
    j=j+1
    
    if(dim(sub2)[[1]]>=20)
    {
      slope=eightMethodsMEE_mod(sub2$weight_kg,b.only = T)$hMLE
      
      res[j,1] <- i
      res[j,2] <- l
      res[j,3]=mean(sub2$lat_in)
      res[j,4]=mean(sub2$lon_in)
      res[j,5]=slope[1]
      res[j,6]=slope[2]
      res[j,7]=slope[3]
      
      strn=unique(sub2$NewOpCode)
      res[j,8]=length(strn)
    }
    
  }
  
}

res=na.omit(res)

dim(res)
summary(res)

res = res[res$betaslope <  1, ]
res = res[res$betaslope > -4, ]


#write.table(res,file="slope_MLE_by_day_20indmin_benthic.txt")

hist(res[,"betaslope"])

res$Date <- as.character(as.Date(as.character(res$Date), format = "%Y-%m-%d"))
res$key <- paste(res$Exped, res$Date, sep = "__")

res        <- res[ , c(5:7, 9)]

colnames(res) = c('betaslope', 'betaslope_ci_inf', 'betaslope_ci_sup', 'key')


dat3 <- merge(dat2, res, by = "key", all = TRUE)


dat3  <- dat3 %>% drop_na(Date)

return(dat3)

}#end of function






# # MATURITY ----
# 
# # importation des donnes
# 
# load("BRUV_pelagic_maturity2.Rdata")
# summary(BRUV_pelagic_maturity)
# dim(BRUV_pelagic_maturity)
# head(BRUV_pelagic_maturity)
# 
# mat=na.omit(BRUV_pelagic_maturity)
# dim(mat)
# 
# length(unique((mat$Date)))
# 
# # median maturity per day
# 
# exp=unique(mat$Exped)
# 
# length(exp)
# 
# day=unique(mat$Date)
# 
# length(day)
# 
# taille=0
# 
# for (i in exp)
# {
#   
#   sub=mat[mat$Exped==i,]
#   
#   day=unique(sub$Date)
#   
#   taille=taille+length(day)
#   
# }
# 
# maturity=matrix(NA,nrow=taille,ncol=7)
# 
# colnames(maturity)=c("mean lat","mean long","mean maturity","STD","CV","median maturity","string number")
# 
# j=0
# 
# for (i in exp)
# {
#   
#   sub=mat[mat$Exped==i,]
#   
#   day=unique(sub$Date)
#   
#   for (l in day)
#   {
#     
#     j=j+1
#     
#     sub2=sub[sub$Date==l,]
#     
#     if(dim(sub2)[[1]]>=1)
#     {
#       
#       cam=unique(sub2$string)
#       
#       m=0
#       lat=0
#       long=0
#       n=0
#       
#       for (k in cam)
#       {
#         
#         sub3=sub2[sub2$string==k,]
#         
#         if(dim(sub3)[[1]]>=1)
#         {
#           m=c(m,sub3$Mature)
#           lat=c(lat,sub3$lat_in)
#           long=c(long,sub3$lon_in)
#           n=n+1
#         }
#         
#       }
#       
#       m=m[-1]
#       lat=lat[-1]
#       long=long[-1]
#       
#       maturity[j,1]=mean(lat)
#       maturity[j,2]=mean(long)
#       maturity[j,3]=mean(m)
#       maturity[j,4]=sd(m)
#       maturity[j,5]=maturity[j,4]/maturity[j,3]
#       maturity[j,6]=median(m)
#       maturity[j,7]=n
#       
#     }
#     
#   }
#   
# }
# 
# summary(maturity)
# 
# write.table(maturity,file="maturity_pelagic.txt")
# 
# 
# # importation et transformation des donnes
# load("BRUV_benthic_maturity2.Rdata")
# summary(BRUV_benthic_maturity)
# head(BRUV_benthic_maturity)
# dim(BRUV_benthic_maturity)
# 
# mat=na.omit(BRUV_benthic_maturity)
# dim(mat)
# 
# # mean maturity per day
# 
# exp=unique(mat$Exped)
# 
# length(exp)
# 
# day=unique(mat$Date)
# 
# length(day)
# 
# taille=0
# 
# for (i in exp)
# {
#   
#   sub=mat[mat$Exped==i,]
#   
#   day=unique(sub$Date)
#   
#   taille=taille+length(day)
#   
# }
# 
# maturity=matrix(NA,nrow=taille,ncol=7)
# 
# colnames(maturity)=c("mean lat","mean long","mean maturity","STD","CV","median maturity","string number")
# 
# 
# j=0
# 
# for (i in exp)
# {
#   
#   sub=mat[mat$Exped==i,]
#   
#   day=unique(sub$Date)
#   
#   for (l in day)
#   {
#     
#     j=j+1
#     
#     sub2=sub[sub$Date==l,]
#     
#     if(dim(sub2)[[1]]>=1)
#     {
#       
#       cam=unique(sub2$NewOpCode)
#       
#       m=0
#       lat=0
#       long=0
#       n=0
#       
#       for (k in cam)
#       {
#         
#         sub3=sub2[sub2$NewOpCode==k,]
#         
#         if(dim(sub3)[[1]]>=1)
#         {
#           m=c(m,sub3$Mature)
#           lat=c(lat,sub3$lat_in)
#           long=c(long,sub3$lon_in)
#           n=n+1
#         }
#         
#       }
#       
#       m=m[-1]
#       lat=lat[-1]
#       long=long[-1]
#       
#       maturity[j,1]=mean(lat)
#       maturity[j,2]=mean(long)
#       maturity[j,3]=mean(m)
#       maturity[j,4]=sd(m)
#       maturity[j,5]=maturity[j,4]/maturity[j,3]
#       maturity[j,6]=median(m)
#       maturity[j,7]=n
#       
#     }
#     
#   }
#   
# }
# 
# summary(maturity)
# 
# write.table(maturity,file="maturity_benthic.txt")
# 
# 

