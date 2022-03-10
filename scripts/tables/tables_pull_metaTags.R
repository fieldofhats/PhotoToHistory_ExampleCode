

##############################################################  working  --- pull metadata           ###############################################


pull.met<-function(x) system2("exiftool", paste('-s -s -s ','-DateTimeOriginal -AmbientTemperatureFahrenheit -MoonPhase ',x),stdout = T) ## slow!

photos.full<-file.path(in.dir,photos)

photos.path.only<-dirname(photos.full)

photos.path.only<-unique(photos.path.only)

photo.met<-sapply(photos.path.only,pull.met) ## process by directory, not individule file, should be faster

str(photo.met)

photo.met.bak<-photo.met
#  photo.met<-photo.met.bak
photo.met<-unlist(photo.met,use.names=F)
tail(photo.met)
head(photo.met)
length(photo.met)
photo.met[2]
out.met<-matrix(NA,nrow=length(photos.full),ncol=4)
j=1
for(i in 1:length(photo.met)){
  if (substring(photo.met[i],1,4) == '====') {
    out.met[j,1]<-photo.met[i]
    out.met[j,2]<-photo.met[i+1]
    out.met[j,3]<-photo.met[i+2]
    out.met[j,4]<-photo.met[i+3]
    j=j+1
  } 	
}

head(out.met)
View(out.met)
View(photo.meta)

####################################################################################################################################
photo.met.dat<-data.frame(moon=out.met[,4])
photo.met.dat$temp_f<-substring(out.met[,3],1,2)
photo.met.dat$photo_date<-substring(out.met[,2],1,10)
photo.met.dat$photo_time<-substring(out.met[,2],12,19)

View(photo.met.dat)

## combine data and meta
photo.dat.bak<-photo.dat
photo.dat<-cbind(photo.dat,photo.met.dat)
View(photo.dat)
View(photo.dat[,c('file','photo_date','photo_time')])

####################################################
######################  convert factors to char

i <- sapply(photo.dat, is.factor)
photo.dat[i] <- lapply(photo.dat[i], as.character)



#################################################
############ fix cell site, . to _
unique(photo.dat$cell_site)
photo.dat$cell_site<-gsub('\\.' , '_' , photo.dat$cell_site)


########################## save data tables


#out.path<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2019"
#out.path<-"C:/Users/bhatfield/Desktop"
out.path<-'./data/output'
out.file<-"2020_photodata_date_time_moon_temp.txt"

write.table(photo.dat,file.path(out.path,out.file))
photo.dat<-read.table(file.path(out.path,out.file))
View(photo.dat)

## save as csv
out.file<-"2019_photodata_date_time_moon_temp.csv"
# write.csv(photo.dat,file.path(out.path,out.file))
photo.dat<-read.csv(file.path(out.path,out.file))
head (photo.dat)