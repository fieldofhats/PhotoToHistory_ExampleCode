##########################################################################
################################  Load file List and create raw Data Frame:

#photo direcory
in.dir<-"./data/in/photos"

photos<-list.files(in.dir,recursive=T)

# View(photos)
# head(photos)

photos<-grep('.jpg',photos,ignore.case=T,value=T)

## split photo list on backslash (/)
## list of vectors from file structure:
photo.split<-strsplit(photos,'/')

# length(photo.split)
# str(photo.split)

#### convert photo.split to data frame:
## even length vectors:
# from stack overflow

photo.dat.raw <-do.call(rbind.data.frame, photo.split)

#View(photo.dat.raw)

#str(photo.dat.raw)

#################################################
####################### col Names:
photo.dat<-photo.dat.raw

#  head(photo.dat)

dat.names<-c('sa','cell','cell_site','species','count','file')

# sum(duplicated(photo.dat$file))
colnames(photo.dat)<-dat.names

#str(photo.dat)


## other photos
## best: include # folder in dir structure
## if not fix the problem
## 
photo.dat<-photo.dat %>% 
  mutate(file = ifelse(grepl('other', species), count, file),
         count = ifelse(grepl('other', species), 0, count),
         )

## tests:
photo.dat$cell_site[which(is.na(photo.dat$count))]
unique(photo.dat$count)
unique(photo.dat$sa)
unique(photo.dat$cell)
unique(photo.dat$class)
unique(photo.dat$species)



## convert all to char:
photo.dat[]<-lapply(photo.dat, as.character)
## convert count to #
photo.dat$count<-as.numeric(photo.dat$count)

View(photo.dat)
############# add file path:

## add photo path to table
# photo.full.path<-paste0(in.dir,'/',photos)
# head(photo.full.path)
# photo.dat$path<-photo.full.path

########## save output table
#out.path<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2020"
# out.path<-"./data/output"
# out.file<-"example.out"
# write.table(photo.dat,file.path(out.path,out.file))
# photo.dat<-read.table(file.path(out.path,out.file))