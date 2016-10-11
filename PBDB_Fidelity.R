# Load Libraries 
library("RCurl")
library("doParallel")
library("data.table")

# Download dictionary of unit names from Macrostrat Database
UnitsURL<-paste("https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv")
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)

# Subset UnitsFrame to extract only units that are identified as unfossiliferous in PBDB
NoPBDB<-subset(UnitsFrame, UnitsFrame[,"pbdb_collections"]==0)

Overlap<-fread("~/Documents/DeepDive/PBDB_Fidelity/strat_pbdb_overlap 2/strat_overlap_doc_terms",header=FALSE)
Overlap<-as.matrix(Overlap)
CandidateUnits<-unique(NoPBDB[,"strat_name_long"])
CandidateUnits<-subset(Overlap,Overlap[,"V2"]%in%as.character(CandidateUnits)==TRUE)
DeepDiveData<-as.data.frame(DeepDiveData,stringsAsFactors=FALSE)
DeepDiveData<-subset(DeepDiveData,DeepDiveData[,"V1"]%in%CandidateUnits[,"V1"]==TRUE)

UnitDictionary<-CandidateUnits[,"V2"]

CleanedWords<-gsub(","," ",DeepDiveData[,"V4"])

Cluster<-makeCluster(4)

Start<-print(Sys.time())
UnitHits<-parSapply(Cluster,UnitDictionary,function(x,y) grep(x,y,ignore.case=FALSE, perl = TRUE),CleanedWords)
End<-print(Sys.time())
  
stopCluster(Cluster)


