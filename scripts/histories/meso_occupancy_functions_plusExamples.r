###############   Unmarked Occupancy Functions: 
###############   Data preparation functions: 




###############   Create Daily Detection History   ################ no holes in the data
###############   Long Format, with date and julian date of detection history
###############			## explain long format:
###############   aggregate on detection period (eg 30 minute detections)
######data:   raw detection data frame including: cell_site, 


############### Create empty detection history long format from station data
####  stData:  			data frame with station cell_site, Min Date, Max Date
####  minField:  		field of station start dates
####  maxField: 		field of station end dates
####  dtFormat: 		format of date in format syntax eg '%Y/%m/%d'
####  detectionYear:	data year for julian date, number, 4 digit
####  minDt: 			cutoff minimum date (string, correct format)
####  maxDt: 			cutoff maximum date  (string, correct format)
####   				if you want to use a start or end date other than min or max of all stations
####
####  output:  dataframe with cell_site, detection (count), julian date of current year (dates befor jan 1 negative), cell_site
####			detection field gets NA if it is outside station dates, 0 if in station dates

## *************************  add region to output  *******************************

dailyDetection0<-function(	stData, 
							detectionYear, 
							csField='cell_site',
							minField='stStart', 
							maxField='stStop', 
							dtFormat='%Y/%m/%d', 
							minDt = NA, 
							maxDt = NA){
		# vector of mindt to maxdt
		if(is.na(minDt)){
			minDt<-min(as.Date(stData[,minField],format=dtFormat))
		}else{
			minDt<-as.Date(minDt,format=dtFormat)
		}
		
		if(is.na(maxDt)){
			maxDt<-max(as.Date(stData[,maxField],format=dtFormat))
		}else{
			maxDt<-as.Date(maxDt,format=dtFormat)
		}

		allDts<-as.Date(minDt:maxDt,origin="1970-01-01")
		yrs<-format(allDts,'%Y')
		# last day of last year, this year
		thisYr<-as.numeric(format(as.Date(paste0(detectionYear,'/12/31')),'%j'))
		lastYr<-as.numeric(format(as.Date(paste0(detectionYear-1,'/12/31')),'%j'))
		jDts<-as.numeric(format(allDts,'%j'))
		jDts[as.numeric(yrs)== detectionYear-1]<-jDts[as.numeric(yrs)== detectionYear-1]-lastYr
		jDts[as.numeric(yrs)== detectionYear+1]<-jDts[as.numeric(yrs)== detectionYear+1]+thisYr
		#preallocate
		blankRow<-rep(NA, length(jDts)*nrow(stData))
		detectionHist<-data.frame(cell_site=blankRow, julianDt=blankRow, count=blankRow, dtDate=blankRow, cell=blankRow, installed=blankRow)
		#detectionHist<-data.frame(cell_site=blankRow, julianDt=blankRow, count=blankRow, dtDate=blankRow, cell=blankRow)
		#  fill in blank daily history
		# i = 1
		rows<-c(1:length(jDts))
		for(i in 1:nrow(stData)){
			min.st.dt<-as.Date(stData[i, minField],format=dtFormat)
			max.st.dt<-as.Date(stData[i, maxField],format=dtFormat)
			st.dts<-as.Date(min.st.dt : max.st.dt, origin = "1970-01-01")
			st.yrs<-as.numeric(format(st.dts, '%Y'))
			st.dts<-as.numeric(format(st.dts,'%j'))
			st.dts[as.numeric(st.yrs)== detectionYear-1]<-st.dts[as.numeric(st.yrs)== detectionYear-1]-lastYr
			st.dts[as.numeric(st.yrs)== detectionYear+1]<-st.dts[as.numeric(st.yrs)== detectionYear+1]+thisYr
			detectionHist$cell_site[rows]<-rep(stData[,csField][i],length(jDts))
			detectionHist$julianDt[rows]<-jDts
			detectionHist$dtDate[rows]<-as.character(allDts)
			detectionHist$count[rows][detectionHist$julianDt[rows]%in%st.dts]<-0
			detectionHist$installed[rows][detectionHist$julianDt[rows]%in%st.dts]<-1
			rows<-rows+length(jDts)
		}	
		detectionHist$cell<-substring(detectionHist$cell_site,1,5)
		detectionHist
}

# # working example:
# st.inpath<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2016/occupancy"
 # st.infile<-"Station install removal date.csv"
 # st.dat<-read.csv(file.path(st.inpath, st.infile),header=T)
 # View(st.dat)
# # dailyDetection0(stData, detectionYear, minField='stStart',  maxField='stStop', dtFormat='%Y/%m/%d', minDt = NA, maxDt = NA)

# i <- sapply(st.dat, is.factor)
# st.dat[i] <- lapply(st.dat[i], as.character)
# blank.detection <- dailyDetection0(	stData=st.dat, 
									# detectionYear=2016, 
									# minField='MinOfDate', 
									# maxField='MaxOfDate', 
									# dtFormat='%m/%d/%Y', 
									# #minDt = NA,
									# minDt = '10/01/2015',
									# maxDt = NA
								# )

# # View(blank.detection)
# # blank.detection<-blank.detection[blank.detection$cell!=21034,]


###################################################################################
###################### Function: detectionOffDts()
######################  add non operational dates to long form detection history with NAs
######################  Provide a blank detection history and a station data table with cell_site and date ranges where station was not working (eg buried)
######################  this function steps through the cell sites in the station data table, and adds NA to the detection history for the provided date ranges
##### Inputs:
	# blankDetectionHist			Detections history output from dailyDetection0 function	
	# blCountField='count'			blank detection history field with 0/na count
	# blCsField='cell_site'			blank detection history cell site field name
	# blDtField='dtDate',			blank detection history text date field name
	# blankDtFormat='%Y-%m-%d'		blank detection history date format for date field
	# stData						Station Data, non operational dates including cell_site, non op start date, non op end date
	# csField='cell_site'			stData cell site field name
	# startField='stStart'			stData non op start date field name
	# stopField='stStop'			stData non op stop date field name
	# stDtFormat='%m/%d/%Y')		stData date format






detectionOffDts<-function(	blankDetectionHist,	
							blCountField='count',
							blCsField='cell_site',
							blDtField='dtDate',
							blankDtFormat='%Y-%m-%d',
							stData, 
							csField='cell_site',
							startField='stStart', 
							stopField='stStop', 
							stDtFormat='%m/%d/%Y'){
		# convert text date fields to date class
		stData[,startField]<-as.Date(stData[,startField],stDtFormat)
		stData[,stopField]<-as.Date(stData[,stopField],stDtFormat)
		blankDetectionHist[,blDtField]<-as.Date(blankDetectionHist[,blDtField],blankDtFormat)
		
		# match cell_site, min date, max date, and add NA for non operational time spans
		# i = 1
		for(i in 1:nrow(stData)){
			blankDetectionHist[blankDetectionHist[,blCsField] == stData[i,csField] &
								  blankDetectionHist[,blDtField] >= stData[i,startField]&
								  blankDetectionHist[,blDtField] <= stData[i,stopField] ,
								 blCountField] <- NA
		}	
		
		# convert date back to text in the blank history
		blankDetectionHist[,blDtField]<-as.character(blankDetectionHist[,blDtField])
		#return detection history
		blankDetectionHist

}


# #### working example:
# ## 2017 Data
# # first: Blank Detection history using dailyDetection0
# st.inpath<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2017/stationStatusTables"
 # st.infile<-"2017_station_begin_end_dts.csv"
 # st.dat<-read.csv(file.path(st.inpath, st.infile),header=T, stringsAsFactors = F)
 # View(st.dat)
# # dailyDetection0(stData, detectionYear, minField='stStart',  maxField='stStop', dtFormat='%Y/%m/%d', minDt = NA, maxDt = NA)

# i <- sapply(st.dat, is.factor)
# st.dat[i] <- lapply(st.dat[i], as.character)
# blank.detection <- dailyDetection0(	stData=st.dat, 
									# detectionYear=2017, 
									# minField='MinOfDate', 
									# maxField='MaxOfDate', 
									# dtFormat='%m/%d/%Y', 
									# minDt = NA,
									# maxDt = NA
								# )
								
# View(blank.detection)


# ### function to add non operational Dates:


# st.inpath<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2017/stationStatusTables"
 # st.infile<-"2017_nonOperationalDts.csv"
 # st.off.dat<-read.csv(file.path(st.inpath, st.infile),header=T, stringsAsFactors = F)
 # View(st.off.dat)
 # str(st.off.dat)
 
 # i <- sapply(st.off.dat, is.factor)
# st.off.dat[i] <- lapply(st.off.dat[i], as.character)
 

# blank.detection.ofDts<-detectionOffDts( blankDetectionHist = blank.detection,	
							# blCountField='count',
							# blCsField='cell_site',
							# blDtField='dtDate',
							# blankDtFormat='%Y-%m-%d',
							# stData= st.off.dat, 
							# csField='cell_site',
							# startField='stStart', 
							# stopField='stStop', 
							# stDtFormat='%m/%d/%Y')
							
# View(blank.detection.ofDts)



######################  Fill in blank detection history with daily species detections
####					*** cell site has to match in the two data frames (if can data has subsite, det history must have subsite)
####
####	camData:		Camera Data including: cell_site, species, date, and detectionCounts (30 minute or whatever)
####	detection0:		blank detection history from dailyDetection0
####	species:		species to create detection history
####  	detectionYear:	data year for julian date, number, 4 digit
####	csField:		cell_site field
####	dtField:		date field
####	spField:		species field
####	idField:		known id field
####	countField:		species count field
####	dtFormat:		text date format
####	ID:				logical T, F, if TRUE add known id information to the output

####	requires: 		plyr

####	Output:			data.frame, daily detection history for year and species supplied

dailyDetectionOut<-function(camData, 
							detection0,
							species,
							detectionYear,
							spField='species', 
							csField = 'cell_site', 
							dtField='txtDate', 
							idField='id',
							countField='count30',
							dtFormat='%Y:%m:%d',
							ID = FALSE){
	if (!require(plyr)){
		install.packages('plyr')
		require(plyr)
	}
	# reduce to fields of interest
	if(ID==FALSE){
		camData <- camData[,c(spField, csField, dtField, countField)]
	}else{
		camData <- camData[,c(spField, csField, dtField, countField,idField)]
	}
	# reduce to species of interest
	camData <- camData[camData[,spField] ==  species,]
	
	## Julian Date, for survey year
	# last day of last year, this year
	allDts<-as.Date(camData[,dtField], format = dtFormat)
	yrs<-format(allDts,'%Y')
	thisYr <- as.numeric(format(as.Date(paste0(detectionYear,'/12/31')),'%j'))
	lastYr <- as.numeric(format(as.Date(paste0(detectionYear - 1,'/12/31')),'%j'))
	jDts <- as.numeric(format(allDts,'%j'))
	jDts[as.numeric(yrs)== detectionYear - 1] <- jDts[as.numeric(yrs)== detectionYear - 1]-lastYr
	jDts[as.numeric(yrs)== detectionYear + 1] <- jDts[as.numeric(yrs)== detectionYear + 1]+ thisYr
	camData$julianDt <- jDts
	
	# aggregate to daily 30 minute counts:
	if(ID == FALSE){
		daily.counts<-ddply(	camData, 
								c(csField,'julianDt'), 
								function(df)c(
									count1 = sum(as.numeric(df[,countField]),na.rm=T)
								)
		)
	}else{	
		## id, if there is one identifiable animal per detection
		daily.counts<-ddply(	camData, 
								c(csField,'julianDt'), 
								function(df)c(
									count1 = sum(as.numeric(df[,countField]),na.rm=T),
									id =  unique(df[,idField])[1]
								)
		)
	}	

	# View(daily.counts)
	
	## id, should add the option to add multiple ID's
	
	# merge blank detection and daily counts, keep all records from detection0 (all.x = T)
	## *** this version of merge the cell site field names have to match *** 
	## *** could update this to specify the cell site field from each table, might be fine as is ****
	detection.out<-merge(detection0, daily.counts,  by.y = c(csField,  'julianDt'), by.x=c(csField,'julianDt'), all.x=T)
	# update count with non NA values from count1
	detection.out$count[!(is.na(detection.out$count1))] <- detection.out$count1[!(is.na(detection.out$count1))] 
	# remove count1
	detection.out<-subset(detection.out, select = -c(count1))
	detection.out
}


# ## working example

# in.path <-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2016"
# in.file <-"2016_photodata_2ndSpecies_30MinDetections.txt"
# photo.dat <- read.table(file.path(in.path,in.file))
# # View(photo.dat)

# daily.det.hist<-dailyDetectionOut(	photo.dat, 
									# blank.detection,
									# 'AmericanMarten',
									# 2016,
									# spField='species', 
									# csField = 'cell_site', 
									# dtField='photo_date', 
									# countField='count30',
									# dtFormat='%Y:%m:%d'
								# )

# View(daily.det.hist)


################################Global version, for multy year history, usefull for plotting etc
################# *********** Return vector of detections, not entire df ****************
#########################  tidyverse
######################  Fill in blank detection history with daily species detections
####					*** cell site has to match in the two data frames (if can data has subsite, det history must have subsite)
####
####	camData:		Camera Data including: cell_site, species, date, and detectionCounts (30 minute or whatever)
####	detection0:		blank detection history from dailyDetection0
####	species:		species to create detection history
####  # detectionYear:	data year for julian date, number, 4 digit not needed for global option, matching on real date
####	csField:		cell_site field
####	dtField:		date field
####	spField:		species field
####	idField:		known id field
####	countField:		species count field
####  # dataYrField   field with data year infor to match on  *** not needed, match on date
####	dtFormat:		text date format
####	ID:				logical T, F, if TRUE add known id information to the output

####	requires: 		plyr

####	Output:			data.frame, daily detection history for year and species supplied

###Testing
# camData = count.dat
# detection0 = all.blank.sp
# species = 'Coyote'
# spField = 'species' 
# csField = 'year_cell_site' 
# dtField='photo_date' 
# countField='count30'
# dtFormat='%Y:%m:%d'
# ID = FALSE

#sum(camData$count30[camData$species == 'Coyote'], na.rm = T)


dailyDetectionOutGl<-function(camData, 
                            detection0,
                            species,
                            # detectionYear,
                            spField = 'species', 
                            csField = 'cell_site', 
                            dtField='txtDate', 
                            idField='id',
                            countField='count30',
                            # dataYrField='data.year',
                            dtFormat='%Y:%m:%d',
                            ID = FALSE){

  if (!require(tidyverse)){
    install.packages('tidyverse')
    require(tidyverse)
  }
  if (!require(lubridate)){
    install.packages('lubridate')
    require(lubridate)
  }
  # tibble and convert date:
  # utc should be fine, cams do not change on dls, so everything should match
  # date field and format should be static, from detction0 ouput:
  # as_tibble *** does not change names if supplied table is also a tibble, (tibble funcion produces unexpected results)
  detection0<-as_tibble(detection0) %>% 
    mutate(dtDate = ymd(dtDate), cell_site = !!sym(csField))
  
  # reduce to fields of interest
  if(ID==FALSE){
    camData <- as_tibble(camData) %>% 
      #dplyr::select(c(spField, csField, dtField, countField)) 
      dplyr::select(c(all_of(spField), all_of(csField), all_of(dtField), all_of(countField)))
    names(camData)<-c('sp', 'cell_site', 'dtDate', 'sp.count')
    camData <- camData %>%
        mutate(dtDate = ymd(dtDate))
    # View(camData)
  }else{
    #camData <- camData[,c(spField, csField, dtField, countField,idField)]
    camData <- as_tibble(camData) %>% 
      dplyr::select(c(spField, csField, dtField, countField, idField)) 
    names(camData)<-c('sp', 'cell_site', 'dtDate', 'sp.count', 'id')
    camData <- camData %>%
      mutate(dtDate = ymd(dtDate))
  }
  # sum(camData$sp.count[camData$sp == 'Coyote'], na.rm = T)
  # reduce to species of interest, with detections
  camData <- camData %>% 
    filter(sp == species) %>% 
    filter(sp.count >= 0)
  # sum(camData$sp.count, na.rm = T)
  # aggregate to daily 30 minute counts:
  if(ID == FALSE){
    daily.counts<- camData %>% 
      group_by(cell_site, dtDate) %>%
      summarize(count1 = sum(sp.count, na.rm=T))

  }else{	

    ## has not been tested
    daily.counts<- camData %>% 
      group_by(cell_site, dtdate) %>%
      summarize(count1 = sum(sp.count, na.rm=T),
                id = unique(idField)[1])
  }	
  
  # View(daily.counts)
  ## id, should add the option to add multiple ID's
  # merge blank detection and daily counts, keep all records from detection0 left joun
  detection.out<-left_join(detection0, daily.counts, by = c('cell_site', 'dtDate'))
  detection.out<- detection.out %>% mutate(count1 = ifelse(is.na(count1), count,count1))
  # view(detection.out)
  # detection.out
  # return count col for sp:
  detection.out$count1
}


################################################
#######################  Lure Date Function
####
####
####           inputs:
####  detection.history:	detection history data.frame
####  lure.data:			data frame of lure info including cell site, and date lure applied
####  lure.dt.field:		lure.data field for date lure applied
####  lure.cs.field:		lure.data field for cell site
####  dtFormat:				text format of lure.dt.field
####  addNA:				add NA, if count is NA
####  
####
####  output:				vector of days since lure was applied for each detection date
# i=1051
getLureDays<-function(detection.history, lure.data, lure.dt.field = 'lureDt', lure.cs.field = 'cell_site', dtFormat = '%Y/%m/%d', addNA = TRUE){
	# preallocate:
	lure.days<-rep(NA, nrow(detection.history))
	# iterate over detection history to get days since lure for each detection date
	for( i in 1:nrow(detection.history)){
		# assign values from detection history for row i
		cell.site <- detection.history$cell_site[i]
		detection.date <- detection.history$dtDate[i]
		detection.count <- detection.history$count[i]
		# lure.dt: vector of dates lure applied for detection cell site.
		lure.dt <- lure.data[lure.data[, lure.cs.field] == cell.site , lure.dt.field]
		# lure.dt <- lure.data[lure.data[, lure.cs.field] == cell.site , ]
		lure.dt <- as.Date(lure.dt, format = dtFormat)
		# vector of date diffs between detection date and lure dates
		lure.diff<-as.numeric(difftime(as.Date(detection.date), lure.dt, units = 'days'))
		#  lure.day:  days since lure applied, 
				# 0 if difference is 0, added this so NAs do not walk on lure application day
				# if all are less than 0 then 
		# 		NA if count is NA, added logical addNA, for case where na is not wanted in independent cov
		#		-199 if det count is not NA, but no positive values in the diff vector (indicates a problem)
		#		otherwise = minimum of the positive values in the vector
		if(addNA ==TRUE){
			if (any(lure.diff == 0)){
				lure.day <- min(lure.diff[lure.diff >= 0])
			} else if (is.na(detection.count)){
				lure.day <- NA
			} else if (length(lure.diff[lure.diff >= 0])== 0) {
				lure.day<- -199
			} else {
				lure.day <- min(lure.diff[lure.diff >= 0])
			}

		}else{
			if (any(lure.diff == 0)){
				lure.day <- min(lure.diff[lure.diff >= 0])
			} else if (all(lure.diff < 0)){
				lure.day<- 1000
			} else {
				lure.day <- min(lure.diff[lure.diff >= 0])
			}		
		}
		lure.days[i] <- lure.day
	}
	lure.days
}

# i<-654
# detection.history <- daily.det.hist
# lure.data<-lure.date
# lure.dt.field<-'ldt'
# lure.cs.field<-'cell_site'
# dtFormat <- '%Y:%m:%d'

# View(lure.days)
# ## example:
# lure.path<-"M:/AlpineMesocarnivore/Data/PhotoData/PhotoDataTables/2016/occupancy"
# lure.file<-"allStationGustoDt1.csv"
# lure.date<-read.csv(file.path(lure.path,lure.file),header=T, as.is=c(1,2,3,4))
# View(lure.date)


# #daily.det.hist$lureDays <- getLureDays(detection.history = daily.det.hist,
# blank.detection$lureDays<- getLureDays(detection.history = blank.detection,
										# lure.data = lure.date,
										# lure.dt.field = 'ldt',
										# lure.cs.field= 'cell_site',
										# dtFormat = '%Y:%m:%d' 
									# )

# # View(daily.det.hist)

# length(daily.det.hist$lureDays[daily.det.hist$lureDays==-99])
# View(daily.det.hist[daily.det.hist$lureDays==-99,])





#############################################################
####### If Subsites are included in the detection history:
#######			Not currently a function, pretty simple subsetting/ordering/removing,process
####### reduce detection history from cell site subsite, to cell site:
#######  	this works if end date of site (xxxxx_1) corresponds with the start date 
#######  	of the replacement camera (xxxxx_1.1), another words the new camera, subsite 1,
#######  	repaces the original camera, subsite 0, on the same date and the new cam takes ver
#######  	detections from that date.  This has to be set up in the cam op date input 
#######  	table

#######     duplicate values of cell site then have to be removed, if there is a value, then remove NA

#######		create and keep cell site subsite field, needed to match species detections...

## commented out bh 2020
# ## create a cell site subsite field, then remove subsite from the cell site field
# blank.detection$cell_site_sub<-blank.detection$cell_site
# blank.detection$cell_site<-substring(blank.detection$cell_site_sub,1,7)


## from web, order and remove duplicates, this should remove NA value, when there is a value in the other 
##  duplicate


## commented out bh 2020
# # order on cell_site (no subsite), date and count.  I think that NA's should be second element of row pairs.
# blank.detection<-blank.detection[order(blank.detection$cell_site,blank.detection$dtDate,blank.detection$count),]
# #  when ordered as above, this should remove the duplicates with an NA value
# blank.detection<-blank.detection[!duplicated(data.frame(blank.detection$cell_site, blank.detection$dtDate)),]



#######################################################
############# add dates funcion
######### problem, aggregated detection history includes uneven history by julina date
######## all stations need the same julian dts


# ## testing vars
# blankHist <- all.blank
# yearField = 'data.year'
# csField = 'cell_site'
# jDtField = 'julianDt'
# dtDtField = 'dtDate'
# lureField = 'lureDays'

##############################################################################
#######################  Fill dates funciont
####          Fills in j dates, text dates, and other fields to make all sites
####          equal lenght across data years
####
####           inputs:
####  blankHist:	detection history tibble
####       ###   note fields should not change, but just in case:
####  yearField:			data.year field from detection history
####  csField:		cell site field
####  jDtField:		julian date field
####  dtDtField:				text date field, **** needs to be compatable w ymd function  ****
####  lureField:			lure days field
####  
####
####  output:				vector of days since lure was applied for each detection date
# i=1051


fill.detection.dts <- function(blankHist,
                               yearField = 'data.year',
                               csField = 'cell_site',
                               jDtField = 'julianDt',
                               dtDtField = 'dtDate',
                               lureField = 'lureDays'
) {
  #### copy of blankhist to add cell site year, save blankHist for adding rows and final return
  blankHist<-blankHist %>% 
    mutate(year_cell_site = paste(data.year,cell_site,sep='_'))
  
  ## min and max julian dates
  min.dt<-min(blankHist[jDtField])
  max.dt<-max(blankHist[jDtField])
  
  ## get unique cell sites
  sites <- unique(blankHist$year_cell_site)
  # i = 1
  
  ## loop through indvidule sites
  ## add rows to blankHist by site
  for(i in 1:length(sites)){
    
    ## filter to site i
    blank.cell<-blankHist %>% 
      filter(year_cell_site == sites[i])
    
    ## subset max and min j date 
    min.dt.cell<-min(blank.cell[jDtField])
    max.dt.cell<-max(blank.cell[jDtField])
    
    ## subset max and min dt date
    min.dtDate<-min(ymd(blank.cell[[dtDtField]]))
    max.dtDate<-max(ymd(blank.cell[[dtDtField]]))
    
    ## get seqence to add befor first jdate
    befor.julian<-c(min.dt:(min.dt.cell-1))
    befor.dt<-as.character(seq.Date((min.dtDate - length(befor.julian)) , min.dtDate-1, by = 'day'))
    
    ## get sequence to add after last jdate   
    after.julian <- c((max.dt.cell + 1) : max.dt)
    after.dt <- as.character(seq.Date((max.dtDate +1),  max.dtDate + length(after.julian), by = 'day'))
    
    ## unique values
    yrcs <- sites[i]
    yr <- unique(blank.cell[[yearField]])[1]
    cs <- unique(blank.cell[[csField]])[1]
    lr <- 1000
    
    ## add rows to blankHist before subset min, if subset min is greater than all data minimum
    if(min.dt.cell > min.dt) {
      blankHist<-add_row(blankHist,
                         !!sym(jDtField) := befor.julian, 
                         !!sym(dtDtField) := befor.dt,
                         year_cell_site = yrcs,
                         !!sym(yearField) := yr,
                         !!sym(csField) := cs,
                         !!sym(lureField) := lr
      )
    } ## if
    
    ## add rows to blankhist after subset max, if subset max is less than data max
    if(max.dt.cell < max.dt) {
      blankHist<-add_row(blankHist,
                         !!sym(jDtField) := after.julian, 
                         !!sym(dtDtField) := after.dt,
                         year_cell_site = yrcs,
                         !!sym(yearField) := yr,
                         !!sym(csField) := cs,
                         !!sym(lureField) := lr 
      )
    } ## if
    ## View(blankHist)
    ## View(blank.cell)
    
  } ## for loop
  blankHist<-blankHist %>% 
    arrange(data.year, cell_site, julianDt)
  blankHist
}## function
