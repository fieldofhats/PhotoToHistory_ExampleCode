#  plan:
## list recursively, reduce paths to mammal detections
## tabulate from dir structure
## pull exif data as needed (eg photo date time)
## convert to 30 minute detections

if (!require(data.table)){
install.packages('data.table')
require(data.table)
}

# if (!require(xts)){
# install.packages('xts')
# require(xts)
# }
# endpoints

# if (!require(plyr)){
# install.packages('plyr')
# require(plyr)
# }

if (!require(ggplot2)){
install.packages('ggplot2')
require(ggplot2)
}

if (!require(tidyverse)){
  install.packages('tidyverse')
  require(tidyverse)
}

if (!require(lubridate)){
  install.packages('lubridate')
  require(lubridate)
}



##########################################################################
################################  Load file List and create raw Data Frame:

#photo direcory
##  directory where sorted photos are located
in.dir<-"E:/sort/passive/to_WI_folders"


# list all photo files
photos<-list.files(in.dir,recursive=T)

## only include jpgs
photos<-grep('.jpg',photos,ignore.case=T,value=T)



## split photo list on backslash (/)
## list of vectors from file structure:
photo.split<-strsplit(photos,'/')
# length(photo.split)
# str(photo.split)
# photo.split[[1]]

#### convert photo.split to data frame:
## even length vectors:
# from stack overflow

photo.dat.raw <-do.call(rbind.data.frame, photo.split)

# View(photo.dat.raw)
# 
# str(photo.dat.raw)


#################################################
####################### col Names:


photo.dat<-photo.dat.raw
# head(photo.dat)

## col names adjust as needed
dat.names<-c('data_year','sa','cell','cell_site','class','species','count','file', 'unkcol')
colnames(photo.dat)<-dat.names
#photo.dat<-photo.dat[,-9]
# str(photo.dat)


## convert all to char:
photo.dat[]<-lapply(photo.dat, as.character)

## check colls

############################ various checks #########################################################
#### reduce to mamal other bird insect (add catagories if needed)
## sites:
photo.dat %>% 
  as_tibble %>% 
  filter(class == 'Mammal' | class == 'Other' | class == 'Bird' | class == 'Insect') %>% 
  select(cell_site) %>%
  arrange(cell_site) %>% 
  pull() %>% 
  unique() %>% 
  view()



## count
photo.dat %>% 
  as_tibble %>% 
  filter(class == 'Mammal' | class == 'Other' | class == 'Bird' | class == 'Insect') %>% 
  select(count) %>%
  #arrange(species) %>% 
  unique() %>% 
  view()


photo.dat %>% 
  as_tibble %>% 
  filter(class == 'Mammal' | class == 'Other' | class == 'Bird' | class == 'Insect') %>% 
  mutate(check = substring(count,1,8)) %>% 
  group_by(check) %>% 
  summarize(sa[1], species[1]) %>% 
  arrange(check) %>% 
  View

photo.dat %>% 
  as_tibble %>% 
  filter(class == 'Mammal' | class == 'Other' | class == 'Bird' | class == 'Insect') %>% 
  mutate(check = substring(count,1,8)) %>% 
  group_by(check, data_year, cell_site) %>% 
  summarize(sa[1], species[1]) %>% 
  arrange(check) %>% 
  View


## file

photo.dat %>% 
  as_tibble %>% 
  filter(class == 'Mammal' | class == 'Other' | class == 'Bird' | class == 'Insect') %>% 
  mutate(check = substring(file,1,8)) %>% 
  group_by(check) %>% 
  summarize(sa[1], species[1]) %>% 
  arrange(check) %>% 
  View





photo.dat %>% 
  as_tibble %>% 
  filter(species == '1') %>% 
  group_by(data_year, cell_site) %>% 
  summarize(data_year[1],sa[1],cell_site[1])



#### clean up:


## reduce to classes of interest

photo.dat<-
  photo.dat %>% 
  as_tibble %>% 
  filter(class == 'Mammal' | class == 'Other' | class == 'Bird' | class == 'Insect') %>%
  mutate(count = as.numeric(count))


# View(photo.dat)

## more tests:
nrow(photo.dat)

unique(photo.dat$species)
unique(photo.dat$count)
unique(photo.dat$sa)
unique(photo.dat$cell)
unique(photo.dat$cell_site)
unique(photo.dat$class)

sum(duplicated(photo.dat$file))
photo.dat$cell_site[which(duplicated(photo.dat$file))]
photo.dat$file[which(duplicated(photo.dat$file))]

photo.dat$cell_site[which(is.na(photo.dat$count))]
############# add file path:

photo.full.path<-paste0(in.dir,'/',photo.dat$file)
head(photo.full.path)
photo.dat$path<-photo.full.path



## save raw photo data table
out.path<-"./data/output"
out.file<-"2016_2020_passive_raw_photoData_tab.txt"
write.table(photo.dat,file.path(out.path,out.file))
photo.dat<-read.table(file.path(out.path,out.file), stringsAsFactors = F)

## View(photo.dat)
## View(photo.dat[which(photo.dat$cell_site == '21750_1'),])

########### format for WI


### check file name

## look at nchar
unique(nchar(photo.dat$file))


## look at each nchar above and check for errors problems
tibble(photo.dat) %>% 
  filter(nchar(file) == 37
         ) %>% 
  View()

photo.dat %>% 
  filter(nchar(file) == 40) %>% 
  group_by(cell_site, data_year, species ) %>% 
  summarize(file[1], sum(count)) %>% 
 
  View()



######  get data year and sa for below
dat.yr<-unique(photo.dat$data_year)
sa<-unique(photo.dat$sa)

#### clean up sa:

## look at SA and fix as needed:
photo.dat<-
  photo.dat %>% 
  mutate(sa = ifelse(sa == "SanJoaquinRidge", "San Joaquin Ridge", sa))


### add short name that matches sa above
sa.short<-c('NL', 'RC', 'SB', 'TL', 'MC', 'SC', 'ML', 'RR', 'SJ')



#########  get wi style datetime from filenames:
## stringsplit file name into tibble:
file.split<-strsplit(photo.dat$file,'_')
file.split <-do.call(rbind.data.frame, file.split)
file.split[]<-lapply(file.split, as.character)
file.split[,1]<-1:nrow(file.split)
nrow(file.split)
# file.split<-tibble(file.split)
file.split.1<-file.split[which(nchar(file.split[,3]) == 8 ),]
file.split.1<-file.split.1[,c(1,3,4)]
names(file.split.1)<-c('index','dt','tm')
file.split.2<-file.split[which(nchar(file.split[,3]) == 0 ),]
file.split.2<-file.split.2[,c(1,4,5)]
names(file.split.2)<-c('index','dt','tm')

file.split<-rbind(file.split.1,file.split.2)
dt.tm<-file.split[order(file.split$index),]
nrow(file.split)
## View(dt.tm)


dt.tm$dt<-paste(substring(dt.tm$dt,1,4), substring(dt.tm$dt,5,6), substring(dt.tm$dt,7,8), sep = '-')


dt.tm$tm <-paste(substring(dt.tm$tm,1,2), substring(dt.tm$tm,3,4), substring(dt.tm$tm,5,6), sep = ':')

dt.tm<-paste(dt.tm$dt,dt.tm$tm,sep = " ")


j.tab<-tibble(sa = sa, sa.short = sa.short)


## get correct deployment id (matching database key:) from database/excell:


in.path<-'./data/in'
in.file<-'yrlocationcellsite_key_20220407.csv'
key.field<-read.csv(file.path(in.path,in.file), stringsAsFactors = F)

key.field<-tibble(key.field) %>% 
  mutate(data_year = dmy(DateSetup)) %>% 
  mutate(data_year = ifelse(month(data_year)>=9,year(data_year)+1, year(data_year))) %>% 
  mutate(year_cell_site = paste(data_year, Cell, Site, sep = '_'))

# key.yr_cell_site<-key.field %>% 
#   select(year_cell_site) %>% 
#   unique() %>% 
#   pull()


## View(deployment.id.dat)
# unique(substring (dt, 1,4))

###############################
## wi working table:


#photo.wi<-tibble(data_year = photo.dat$data_year, sa = photo.dat$sa, cell_site = photo.dat$cell_site)

photo.wi<-photo.dat %>% 
  mutate(photo_file = photo.dat$file) %>% 
  select(data_year, sa, cell_site, photo_file, class, species, count) %>% 
  mutate(year_cell_site = paste(data_year, cell_site, sep='_'))

#wi.yr_cell_site<-photo.wi %>% 


photo.wi<-photo.wi %>% 
  left_join(j.tab) %>%
  left_join(key.field, 'year_cell_site') %>% 
  mutate(project_id = NA, deployment_id = YearLocationCellSite)

 


################################################################################################
## problems between photo wi and the keyfield -- mismatches , na's , etc...


## need to add p to  non matching year_cell_sites, and update in the database:


convert.to.p <- photo.wi %>%
  filter(is.na(deployment_id)) %>%
  group_by(year_cell_site) %>%
  summarize(sa[1]) %>%
  mutate(year_cell_site_nop = sub("p", "", year_cell_site, ignore.case = T)) %>%
  left_join(key.field, by = c('year_cell_site_nop' = 'year_cell_site'))

out.path<-'./data/output'
out.file<-'problem_year_cell_site.csv'
write.csv(convert.to.p, file.path(out.path, out.file))

  #########################################################################

## cell site summary

# photo.wi %>%
#   group_by(deployment_id) %>% 
#   summarize( yr = yr[1],
#              sa = sa.short[1]) %>% 
#   View()



#### create the wi table
####  check_id is an extra for troubleshooting:
photo.wi<-photo.wi %>% 
  select(project_id, deployment_id) %>% 
  mutate(image_id = photo.wi$photo_file,
         check_id = photo.wi$year_cell_site,
         location = NA,
         identified_by = 'Liz Siemion',
         wi_taxon_id = NA,
         species = NA,
         class = NA,
         order = NA,
         family = NA,
         genus = NA,
         #alpine_class = photo.wi$class,
         #alpine_name = photo.wi$species,
         common_name = photo.wi$species,
         timestamp = dt.tm,
         number_of_objects = photo.wi$count,
         highlighted = 0,
         animal_recognizable = NA,
         individual_id = NA,
         individual_animal_notes = NA,
         markings = NA
         ) 

photo.wi %>% 
  select(common_name) %>% 
  arrange(common_name) %>% 
  unique() %>% 
  pull()


#### more cleanup
## cleanup..

## unknown

## unify unknown label
photo.wi <- 
  photo.wi %>% 
  mutate(common_name = ifelse(common_name == 'Unknown', 'Unknown Species',common_name),
         common_name = ifelse(common_name == 'UnknownSpecies', 'Unknown Species',common_name)
         )



## look at other catagories
photo.wi %>% 
  select(common_name) %>%
  filter(grepl('other', common_name)) %>% 
  arrange(common_name) %>% 
  unique() %>% 
  pull()

## other:
### add Measurement Scale
photo.wi <-
  photo.wi %>%
  mutate(common_name = ifelse(common_name == 'otherScale', 'Measurement Scale',common_name)
  )


## unify otherMissedTriggers
photo.wi <-
  photo.wi %>%
  mutate(common_name = ifelse(common_name == 'otherFalseTrigger', 'otherMissedTrigger',common_name)
  )

## move other classes to notes
photo.wi <-
  photo.wi %>%
  mutate(individual_animal_notes = ifelse(grepl('other', common_name), common_name, individual_animal_notes)
  )


### check other classes in notes
photo.wi %>% 
  select(individual_animal_notes) %>%
  filter(grepl('other', individual_animal_notes)) %>% 
  arrange(individual_animal_notes) %>% 
  unique() %>% 
  pull()


## leftover other class to Blank
photo.wi <-
  photo.wi %>%
  mutate(common_name = ifelse(grepl('other', common_name), 'Blank', common_name)
  )



photo.wi %>% 
  select(deployment_id) %>% 
  unique() %>% 
  pull()

photo.wi %>% 
  select(check_id) %>% 
  unique() %>% 
  pull()

photo.wi %>% 
  filter(is.na(deployment_id)) %>% 
  group_by()

 
  
# View(unique(photo.wi$alpine_name))

## load name conversion csv

# in.path<-'./data/in'
# in.file<-'alpineToWI_names.csv'
# 
# wi.names<-tibble(read.csv(file.path(in.path, in.file), stringsAsFactors =F)[,-1])
# View(wi.names)

## trailing space problems??
# trim.trailing <- function (x) sub("\\s+$", "", x) 
# 
# photo.wi$alpine_name <- trim.trailing(photo.wi$alpine_name)
# wi.names$alpine_name <- trim.trailing(wi.names$alpine_name)

# photo.wi <- photo.wi %>% 
#   left_join(wi.names)

# View(photo.wi)

# clean up :

# photo.wi<-
# photo.wi %>% 
#   mutate(common_name = WI.Name,
#          hightlighted = ifelse(t_highlighted == 1, 1, hightlighted),
#          animal_recognizable = ifelse(t_animal_recognizable == 'Yes', 'Yes', 'No'),
#          individual_id = t_individual_id,
#          individual_animal_notes = t_individual_animal_notes,
#          markings = t_markings,
#          individual_animal_notes = ifelse(alpine_class == 'Tracks', 'Tracks', individual_animal_notes)
#                                
#         ) %>% 
#   select( project_id,
#           deployment_id,
#           image_id,
#           location,
#           identified_by,
#           wi_taxon_id,
#           class,
#           order,
#           family,
#           genus,
#           # alpine_class,
#           # alpine_name,
#           common_name,
#           timestamp,
#           number_of_objects,
#           highlighted,
#           animal_recognizable,
#           individual_id,
#           individual_animal_notes,
#           markings
#         )


out.path<-'./data/output'
out.file<-'photoDataToWI_passive_20220411.csv'
write.csv(photo.wi, file.path(out.path, out.file))
#  photo.wi<-read.csv(file.path(out.path,out.file), stringsAsFactors =F)


#############################################################################################
## add correct scale name ('Measurement Scale' )

## View(photo.wi)

photo.wi %>% 
  filter(common_name == 'Scale') %>% 
  nrow()

photo.wi %>% 
  filter(common_name == 'Measurement Scale') %>% 
  nrow()

photo.wi <-photo.wi %>% 
  mutate(common_name = ifelse(common_name == 'Scale', 'Measurement Scale', common_name))
  
out.path<-'./data/output'
out.file<-'photoDataToWI_mscale_20210208.csv'
write.csv(photo.wi, file.path(out.path, out.file))

###############################################################################################

