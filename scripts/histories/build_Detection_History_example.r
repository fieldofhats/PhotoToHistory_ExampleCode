##############
########## 2020 build detection History

if (!require(plyr)){
install.packages('plyr')
require(plyr)
}

if (!require(tidyverse)){
  install.packages('tidyverse')
  require(tidyverse)
}

if (!require(lubridate)){
  install.packages('lubridate')
  require(lubridate)
}

source("./scripts/histories/meso_occupancy_functions_plusExamples.r")

# Blank Detection history using dailyDetection0
st.inpath<-"./data/input"
 st.infile<-"2020OperationalDates.csv"
 st.dat<-read.csv(file.path(st.inpath, st.infile),header=T, stringsAsFactors = F)
 # st.dat<-st.dat[-which(st.dat$cell_site=='20851_1'),]
 # View(st.dat)
# dailyDetection0(stData, detectionYear, minField='stStart',  maxField='stStop', dtFormat='%Y/%m/%d', minDt = NA, maxDt = NA)

 
 ### Set year
 
 yr = 2020
 
 
 
 
 
#i <- sapply(st.dat, is.factor)
#st.dat[i] <- lapply(st.dat[i], as.character)
blank.detection <- dailyDetection0(	stData=st.dat, 
									detectionYear=yr, 
									minField='MinOfDate', 
									maxField='MaxOfDate', 
									dtFormat='%m/%d/%Y', 
									minDt = NA,
									maxDt = NA
								)
								
# View(blank.detection)


### function to add non operational Dates:


st.inpath<-"./data/input"
 st.infile<-"2020StationOffDates.csv"
 st.off.dat<-read.csv(file.path(st.inpath, st.infile),header=T, stringsAsFactors = F)
#  View(st.off.dat)
#  str(st.off.dat)
 
 
 ## blanks to NA
 ##  excell made blanks = ""  (blank space)
 st.off.dat$offStart[1]
 
 st.off.dat$offStart[which(st.off.dat$offStart == "")]<-NA
 st.off.dat$offStop[which(st.off.dat$offStop== "")]<-NA
 
 ## remove Nas
 
 st.off.dat<-st.off.dat[!is.na(st.off.dat$offStart),]
 
 # i <- sapply(st.off.dat, is.factor)
# st.off.dat[i] <- lapply(st.off.dat[i], as.character)
 

blank.detection<-detectionOffDts( blankDetectionHist = blank.detection,	
							blCountField='count',
							blCsField='cell_site',
							blDtField='dtDate',
							blankDtFormat='%Y-%m-%d',
							stData= st.off.dat, 
							csField='cell_site',
							startField='offStart', 
							stopField='offStop', 
							stDtFormat='%m/%d/%Y'
)
							
# View(blank.detection)
## length(blank.detection$count[blank.detection$count == 0])

###################################add Lure
# View(lure.days)
## example:
lure.path<-"./data/input"
lure.file<-"2020_lure_dts.csv"
lure.date<-read.csv(file.path(lure.path,lure.file),header=T, stringsAsFactors=F)
# View(lure.date)


blank.detection$lureDays <- getLureDays(detection.history =blank.detection,
										lure.data = lure.date,
										lure.dt.field = 'dt',
										lure.cs.field= 'cell_site',
										dtFormat = '%m/%d/%Y', 
										addNA=FALSE
)

# View(blank.detection)

# blank.detection$lureDays<-NA

## trapping days
# nrow(blank.detection[blank.detection$count == 0,])  ## 
# nrow(blank.detection)
# length(blank.detection$count[is.na(blank.detection$count)])
# length(blank.detection$count[!is.na(blank.detection$count)])

# length(unique(blank.detection$cell_site))


#       No Subsites 2018
#############################################################
####### If Subsites are included in the detection history:
####### reduce detection history from cell site subsite, to cell site:
#######  	this works if end date of site (xxxxx_1) corresponds with the start date 
#######  	of the replacement camera (xxxxx_1.1), another words the new camera, subsite 1,
#######  	repaces the original camera, subsite 0, on the same date and the new cam takes ver
#######  	detections from that date.  This has to be set up in the cam op date input 
#######  	table

#######     duplicate values of cell site then have to be removed, if there is a value, then remove NA

## create a cell site subsite field, then remove subsite from the cell site field
#blank.detection$cell_site_sub<-blank.detection$cell_site
#blank.detection$cell_site<-substring(blank.detection$cell_site_sub,1,7)

############# testing: 
# test<-blank.detection[order(blank.detection$cell_site,blank.detection$dtDate,blank.detection$count),]
# View(test)
# test = test[!duplicated(data.frame(test$cell_site, test$dtDate)),]
# test<-test[order(test$cell_site,test$dtDate,test$count),]
# View(test2)


## from web, order and remove duplicates, this should remove NA value, when there is a value in the other 
##  duplicate

# order on cell_site (no subsite), date and count.  I think that NA's should be second element of row pairs.
#blank.detection<-blank.detection[order(blank.detection$cell_site,blank.detection$dtDate,blank.detection$count),]
#  when ordered as above, this should remove the duplicates with an NA value
#blank.detection<-blank.detection[!duplicated(data.frame(blank.detection$cell_site, blank.detection$dtDate)),]


###
#save blank detection history
#  View(blank.detection[blank.detection$cell_site == "22342_1",])
blank2020<-blank.detection
out.path<-"./data/output"
out.file<-'2020BlankDetection_subsites'
write.table(blank2020, file.path(out.path, out.file))
#  blank2018<-read.table(file.path(out.path,out.file))
##  blank2018<-read.table(file.path(out.path, out.file))
## View(blank2018)
## unique(blank2018$cell_site)
## blank.detection<-blank2018

## trim camera days, no camera deployed (not installed, or removed):
## use operational dates:

## total trapping days:
nrow(blank2019[which(blank2019$count == 0),])

## total deployed days:
nrow(blank2019[which(blank2019$installed == 1),])

## cells - days in passes:

# cell sites in passes (2019 03 12)
cell_site_pass<-c('20184_1',
                  '20244_2',
                  '20245_2',
                  '20366_1',
                  '20426_2',
                  '20669_1',
                  '20730_1',
                  '21031_2',
                  '21032_2',
                  '21092_1',
                  '21092_2',
                  '21750_2',
                  '22048_1',
                  '22108_1',
                  '22166_2',
                  '22225_1',
                  '22283_2',
                  '22342_1',
                  '22342_2',
                  '22400_1')


blank2018.pass<-blank2018[ blank2018$cell_site %in% cell_site_pass,]
View(blank2018.pass)

unique(blank2018.pass$cell_site)
length(unique(blank2018.pass$cell_site))
unique(blank2018.pass$cell)

## trapping days:
nrow(blank2018.pass[which(blank2018.pass$count == 0),])


## total deployed days:
nrow(blank2018.pass[which(blank2018.pass$installed == 1),])




######################### add detection histories:




##get photo data
in.path<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2018"
#out.path<-"C:/Users/bhatfield/Desktop"
in.file<-"2018_photodata_meta_30MinDetections.txt"

#write.table(count.dat,file.path(out.path,out.file))
count.dat<-read.table(file.path(in.path,in.file), stringsAsFactors = F)
# View(count.dat)


### 30 min detections:

count.dat<-count.dat[!is.na(count.dat$count30),]

#### must match on subsite level, make cell_site_subsite field
count.dat$cell_site_sub<-count.dat$cell_site


## fix subsite from cell_site_sub to cell_site.sub
# count.dat$cell_site_sub<-sub("(_.*?)_", "\\1\\.", count.dat$cell_site_sub)



#### add coyote
coyote.2018<-dailyDetectionOut(	count.dat, 
									blank.detection,
									'Coyote',
									2018 ,
									spField='species', 
									csField = 'cell_site', 
									dtField='photo_date', 
									countField='count30',
									dtFormat='%Y:%m:%d'
								)
								
# View(coyote.2018)



# checks:
sum(coyote.2018$count, na.rm = T)


# coyote.2017$month<-substring(coyote.2017$dtDate,6,7)

## save coyote
out.path<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2018/detectionHistories"
out.file<-"2018_coyote.txt"
write.table(coyote.2018, file.path(out.path, out.file))


## fox 2018

fox.2018<-dailyDetectionOut(	count.dat, 
									blank.detection,
									'RedFox',
									2018 ,
									spField='species', 
									csField = 'cell_site', 
									dtField='photo_date', 
									countField='count30',
									dtFormat='%Y:%m:%d'
							)

## by cell site:

fox.2018.cs<-ddply(	fox.2018,
						c('cell_site'), 
						function(df)c(
							days = length(which(!is.na(df$count))),
							detections=sum(df$count,na.rm=T)
							
						)
					)
							
marten.2018<-dailyDetectionOut(	count.dat, 
									blank.detection,
									'AmericanMarten',
									2018 ,
									spField='species', 
									csField = 'cell_site', 
									dtField='photo_date', 
									countField='count30',
									dtFormat='%Y:%m:%d'
								)

#### add wtj
WTJ.2018<-dailyDetectionOut(	count.dat, 
									blank.detection,
									'WhiteTailedJackrabbit',
									2018 ,
									spField='species', 
									csField = 'cell_site', 
									dtField='photo_date', 
									countField='count30',
									dtFormat='%Y:%m:%d'
								)
								
# View(coyote.2018)

# checks:
sum(WTJ.2018$count, na.rm = T)

## save
out.path<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2018/detectionHistories"
out.file<-"2018_WTJackrabbit.txt"
write.table(WTJ.2018, file.path(out.path, out.file))