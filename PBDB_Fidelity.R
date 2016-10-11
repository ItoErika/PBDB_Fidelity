# Load Libraries 
library("RCurl")
library("pbapply")
library("doParallel")
library("data.table")
# library("RPostgreSQL")

# ONLY RUN THE FOLLOWING COMMANDS ONCE
# Connet to PostgreSQL
# Driver <- dbDriver("PostgreSQL") # Establish database driver
# Connection <- dbConnect(Driver, dbname = "labuser", host = "localhost", port = 5432, user = "labuser")

# DeepDiveData<-dbGetQuery(Connection,"SELECT * FROM pbdb_fidelity.pbdb_fidelity_data")
# Save DeepDiveData as a csv to desired folder
# write.csv(DeepDiveData,file="~/Documents/DeepDive/PBDB_Fidelity/R/DeepDiveData.csv",row.names=FALSE)

# Load DeepDiveData 
DeepDiveData<- fread("~/Documents/DeepDive/PBDB_Fidelity/R/DeepDiveData.csv")
DeepDiveData<-as.data.frame(DeepDiveData)

# Load previously run command outputs
UnitHits<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/R/UnitHits.rds")

# Extract columns of interest from DeepDiveData
DeepDiveData<-DeepDiveData[,c("docid","sentid","wordidx","words","poses","dep_parents")]

# Remove symbols 
DeepDiveData[,"words"]<-gsub("\\{|\\}","",DeepDiveData[,"words"])
DeepDiveData[,"poses"]<-gsub("\\{|\\}","",DeepDiveData[,"poses"])
# Make a substitute for commas so they are counted correctly as elements for future functions
DeepDiveData[,"words"]<-gsub("\",\"","COMMASUB",DeepDiveData[,"words"])
DeepDiveData[,"poses"]<-gsub("\",\"","COMMASUB",DeepDiveData[,"poses"])

# Download dictionary of unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

# Subset UnitsFrame to extract only units that are identified as unfossiliferous in PBDB
NoPBDB<-subset(UnitsFrame, UnitsFrame[,"pbdb_collections"]==0)

DocUnitTuples<-fread("~/Documents/DeepDive/PBDB_Fidelity/strat_pbdb_overlap 2/strat_overlap_doc_terms",header=FALSE)
DocUnitTuples<-as.matrix(DocUnitTuples)
# Assign column names to DocUnitTuples matrix
colnames(DocUnitTuples)[1]<-"docid"
colnames(DocUnitTuples)[2]<-"unit"

# Make a list of units that are unfossiliferous according to PBDB
CandidateUnits<-unique(NoPBDB[,"strat_name_long"])

# Subset those units to the ones we have matches for in the DeepDiveData documents
CandidateUnits<-subset(DocUnitTuples,DocUnitTuples[,"unit"]%in%as.character(CandidateUnits)==TRUE)
DeepDiveData<-as.data.frame(DeepDiveData,stringsAsFactors=FALSE)

# Subset DeepDiveData to only documents which contain candidate units
DeepDiveData<-subset(DeepDiveData,DeepDiveData[,"docid"]%in%CandidateUnits[,"docid"]==TRUE)

# Make a dictionary of candidate unit names
UnitDictionary<-CandidateUnits[,"unit"]

# Remove commas from DeepDiveData to prepare to run grep function
CleanedWords<-gsub(","," ",DeepDiveData[,"words"])

# Cluster<-makeCluster(4)

# Start<-print(Sys.time())
# UnitHits<-parSapply(Cluster,UnitDictionary,function(x,y) grep(x,y,ignore.case=FALSE, perl = TRUE),CleanedWords)
# End<-print(Sys.time())
  
# stopCluster(Cluster)

##################### Eliminate sentences in which more than one unit names appears ###########################

# Create a vector of the number of unit hits for each respective unit name in DeepDiveData
UnitHitsLength<-pbsapply(UnitHits,length)
# Create a vector of unit names, such that each name is repeated by its number of hits in DeepDiveData
UnitHitNames<-rep(names(UnitHits),times=UnitHitsLength)
# Bind the unit name column to the corresponding row location for the match
UnitHitData<-cbind(UnitHitNames,unlist(UnitHits))
# convert matrix to data frame
UnitHitData<-as.data.frame(UnitHitData)
# Name column denoting row locations within Cleaned Words
colnames(UnitHitData)[2]<-"MatchLocation"
# Make sure the column data is numerical
UnitHitData[,"MatchLocation"]<-as.numeric(as.character(UnitHitData[,"MatchLocation"]))

# Make a table showing the number of unit names which occur in each DeeoDiveData row that we know has at least one unit match
RowHitsTable<-table(UnitHitData[,"MatchLocation"])
# Locate and extract rows which contain only one long unit
# Remember that the names of RowHitsTable correspond to rows within CleanedWords
SingleHits<-as.numeric(names(RowHitsTable)[which((RowHitsTable)==1)])    

# Subset UnitHitData to get dataframe of Cleaned Words rows and associated single hit long unit names
SingleHitData<-subset(UnitHitData,UnitHitData[,"MatchLocation"]%in%SingleHits==TRUE)    

################################ Eliminate sentences that are more than 350 characters long ###############################





