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

# Load strat-name dictionary and docid tuples from GeoDeepDive
DocUnitTuples<-fread("~/Documents/DeepDive/PBDB_Fidelity/strat_pbdb_overlap 2/strat_overlap_doc_terms",header=FALSE)
DocUnitTuples<-as.matrix(DocUnitTuples)
# Assign column names to DocUnitTuples matrix
colnames(DocUnitTuples)[1]<-"docid"
colnames(DocUnitTuples)[2]<-"unit"

# Download dictionary of unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)
# Subset UnitsFrame to extract only units that are identified as unfossiliferous in PBDB
NoPBDB<-subset(UnitsFrame, UnitsFrame[,"pbdb_collections"]==0)

# Make a list of units that are unfossiliferous according to PBDB
CandidateUnits<-as.character(unique(NoPBDB[,"strat_name_long"]))
CandidateUnits<-CandidateUnits[which(sapply(CandidateUnits,nchar)>0)]

# Subset the DocUnitTuples to only CandidateUnits
SubsetTuples<-subset(DocUnitTuples,DocUnitTuples[,"unit"]%in%CandidateUnits==TRUE) # Goes from 351024 to 128426

# Subset DeepDiveData 
SubsetDeepDive<-subset(DeepDiveData,DeepDiveData[,"docid"]%in%unique(SubsetTuples[,"docid"])==TRUE) # Goes from 5.9 to 3.2 million sentences

# Clean up syntaxical, grammatical, and typographical issues in the words column of DeepDiveData
SubsetDeepDive[,"words"]<-gsub("\\{|\\}","",SubsetDeepDive[,"words"])
SubsetDeepDive[,"poses"]<-gsub("\\{|\\}","",SubsetDeepDive[,"poses"])
# Make a substitute for commas so they are counted correctly as elements for future functions
SubsetDeepDive[,"words"]<-gsub("\",\"","COMMASUB",SubsetDeepDive[,"words"])
SubsetDeepDive[,"poses"]<-gsub("\",\"","COMMASUB",SubsetDeepDive[,"poses"])
# Remove commas from DeepDiveData to prepare to run grep function
CleanedWords<-gsub(","," ",SubsetDeepDive[,"words"])

# Start a cluster for multicore
Cluster<-makeCluster(3)

# Record start time
Start<-print(Sys.time())
# Apply grep to cleaned words
UnitHits<-parSapply(Cluster,CandidateUnits,function(x,y) grep(x,y,ignore.case=FALSE, perl = TRUE),CleanedWords)
# Record end time
End<-print(Sys.time())
# Find total runtime
End-Start

# Stop the cluster so the computer does not kill itself in anger and self loathing.  
stopCluster(Cluster)

# Debugging pause
  
  
# Extract columns of interest from DeepDiveData
DeepDiveData<-DeepDiveData[,c("docid","sentid","wordidx","words","poses","dep_parents")]

# Remove symbols 
DeepDiveData[,"words"]<-gsub("\\{|\\}","",DeepDiveData[,"words"])
DeepDiveData[,"poses"]<-gsub("\\{|\\}","",DeepDiveData[,"poses"])
# Make a substitute for commas so they are counted correctly as elements for future functions
DeepDiveData[,"words"]<-gsub("\",\"","COMMASUB",DeepDiveData[,"words"])
DeepDiveData[,"poses"]<-gsub("\",\"","COMMASUB",DeepDiveData[,"poses"])


# Make a dictionary of candidate unit names
UnitDictionary<-CandidateUnits[,"unit"]

# Cluster<-makeCluster(3)

# Start<-print(Sys.time())
# UnitHits<-parSapply(Cluster,UnitDictionary,function(x,y) grep(x,y,ignore.case=FALSE, perl = TRUE),CleanedWords)
# End<-print(Sys.time())
  
# stopCluster(Cluster)

# Save UnitHits to a folder
# saveRDS(UnitHits,file="~/Documents/DeepDive/PBDB_Fidelity/R/UnitHits.rds")

# Load UnitHits
UnitHits<-readRDS("~/Documents/DeepDive/PBDB_Fidelity/R/UnitHits.rds")

##################### Eliminate sentences in which more than one unit names appears ###########################

# Eliminate elements/unit names in UnitHits with no matches

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

# Create a column of sentences from CleanedWords and bind it to SingleHitData
UnitSentences<-CleanedWords[SingleHitData[,"MatchLocation"]]
SingleHitData<-cbind(SingleHitData,UnitSentences)
# Find the character length for each character string in UnitSentences
Chars<-sapply(SingleHitData[,"UnitSentences"], function (x) nchar(as.character(x)))
# bind the number of characters for each sentence to SingleHitData
SingleHitData<-cbind(SingleHitData,Chars)
# Locate the rows which have CleanedWords sentences with less than or equal to 350 characters
ShortSents<-which(SingleHitData[,"Chars"]<=350)
SingleHitsCut<-SingleHitData[ShortSents,]


