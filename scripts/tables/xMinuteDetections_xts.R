########################################################################################
###########  new count30
### 2022 02 16 - fix xts loop, remove overcounts

count.dat<-as_tibble(photo.dat) %>% 
  mutate(count30 = NA) %>% 
  arrange(cell_site, photo_date, photo_time)

#count.dat<-count.dat[order(count.dat$cell_site,count.dat$photo_date,count.dat$photo_time),]

count.dat<-count.dat %>% 
  mutate(timestamp = paste(photo_date, " ", photo_time))

# count.xts<-function(dat,d.dt,d.tm){
# 	xts(dat,as.POSIXct(paste(d.dt,d.tm,sep=' '),format='%Y-%m-%d %H:%M:%S',tz="America/Los_Angeles"))
# }

count.xts<-function(dat,dt.tm){
  dt.tm.in<-ymd_hms(dt.tm)
  xts(dat,dt.tm.in)
}


## need to make a function that:
###          takes a table with a time stamp and a count
###          adds a count at x interval (not just 30min..)

#count.dat$count30<-rep(NA,nrow(count.dat))
a<-as.character(unique(count.dat$cell_site))

# i = 1
# j = 1

for(i in 1:length(a)){
  # have to iterate over species
  c.s<-count.dat$cell_site==a[i]
  s.p<-as.character(unique(count.dat$species[c.s]))
  for(j in 1:length(s.p)){
    c.s.i<-which(count.dat$cell_site==a[i] & count.dat$species == s.p[j])
    c.xts<-count.xts(count.dat$count[c.s.i],count.dat$timestamp[c.s.i])
    ## need to pull out vals less than 30 from endpoints fn:
    tm.diff<-diff(.index(c.xts)%/%60%/%30+1)
    ## *** remove diffs less than 30 ***
    tm.diff[tm.diff <= 30]<-0
    
    c.ends<-c(0,which(tm.diff != 0), NROW(c.xts))
    
    c.max<-period.apply(c.xts,c.ends,max)
    
    count.dat$count30[c.s.i[c.ends]]<-c.max
  }
}



#Save count data
# out.path<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2019"
out.path<-'./data/out'
#out.path<-"C:/Users/bhatfield/Desktop"
out.file<-"photodata_meta_30MinDetections.txt"

write.table(count.dat,file.path(out.path,out.file))
# count.dat<-read.table(file.path(out.path,out.file))
# View(count.dat)

## save as csv
out.file<-"photodata_covs_30MinDetections_update2022.csv"
write.csv(count.dat,file.path(out.path,out.file))
# count.dat<-read.csv(file.path(out.path,out.file))
#####################################################



##################################################################
## some summaries:


#count.30.dat<-count.dat[which(count.dat$count30 ==1),]
# View(count.30.dat)

count.30.dat<-count.dat %>% 
  filter(!grepl('other', species),  ## remove other 
         count30 == 1)              ## colapse on count 30


coy.dat<-count.30.dat %>% 
  filter(species == 'Coyote')

dates.dat<-count.30.dat %>% 
  group_by(cell_site) %>% 
  summarize(min.dt = min(photo_date),
            max.dt = max(photo_date)
            
            )


## count sum

count.sum <- count.30.dat %>% 
  group_by(sa, cell, cell_site, species) %>% 
  summarize( count30 = sum(count30, na.rm = T))

## totals by species


count.sum.totals <- count.30.dat %>% 
  group_by(species) %>% 
  summarize (count30 = sum(count30, na.rm=T),
             cell.count=length(unique(cell)))

View(count.sum.totals)