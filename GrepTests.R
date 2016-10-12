library("rbenchmark")
library("RCurl")
library("pbapply")
library("doParallel")
library("data.table")

Cluster<-makeCluster(3)

# Record start time
Start<-print(Sys.time())
# Apply grep to cleaned words
benchmark(
parSapply(CandidateUnits[1:50], grepFunction, CleanedWords[1:10000], Word1="fossiliferous"),
parSapply(CandidateUnits[1:50], greplFunction, CleanedWords[1:10000], Word1="fossiliferous"),
parSapply(CandidateUnits[1:50], greplFixed, CleanedWords[1:10000],Word1="fossiliferous"),
parSapply(CandidateUnits[1:50], greplPerl,CleanedWords[1:10000], Word1="fossiliferous"),
parSapply(CandidateUnits[1:50], greplPatternsPerl, CleanedWords[1:10000],Word1="fossiliferous"),
parSapply(CandidateUnits[1:50], greplPatternsFixed, CleanedWords[1:10000], Word1="fossiliferous"),
replications=100)
# Record end time
End<-print(Sys.time())
# Find total runtime
End-Start
  
stopCluster(Cluster)




  
grepFunction<-function(x,y,Word1="fossiliferous"){
  Hits<-grep(x,y, ignore.case=FALSE, perl=TRUE)
  MatchUnits<-names(Hits[which(sapply(Hits,length)>0)])
  SubsetHits<-Hits[MatchUnits]
  SubsetWords<-CleanedWords[unlist(SubsetHits)]                  
  Hits2<-grep(Word1,SubsetWords, ignore.case=FALSE, perl=TRUE)
  return(Hits2) 
  }

grepFunctionHits<-parSapply(CandidateUnits[1:50], grepFunction, CleanedWords[1:10000], Word1="fossiliferous")
                                                      
greplFunction<-function(x,y,Word1="fossiliferous"){
    Hits<-grepl(x,y, ignore.case=FALSE, perl=TRUE)
    HitsVector<-as.vector(Hits)
    RepVector<-apply(Hits,2,length)
    RepRowsVector<-rep(1:10000,times=50)
    UnitNames<-rep(colnames(Hits),times=RepVector)
    HitMatrix<-cbind(UnitNames,HitsVector,RepRowsVector)
    TrueLocation<-which(Hits==TRUE)
    MatchMatrix<-HitMatrix[TrueLocation,]
    MatchFrame<-as.data.frame(MatchMatrix)
    MatchFrame[,"RepRowsVector"]<-as.numeric(as.character(MatchFrame[,"RepRowsVector"]))
    SubsetWords<-CleanedWords[MatchFrame[,"RepRowsVector"]]
    Hits2<-grepl(Word1,SubsetWords, ignore.case=FALSE, perl=TRUE)
    return(Hits2)
    }
                     
greplFunctionHits<-parSapply(CandidateUnits[1:50], greplFunction, CleanedWords[1:10000], Word1="fossiliferous")                    
                   
greplFixed<-function(x,y,Word1="fossiliferous"){
    Hits<-grepl(x,y, ignore.case=FALSE,fixed=TRUE)
    HitsVector<-as.vector(Hits)
    RepVector<-apply(Hits,2,length)
    RepRowsVector<-rep(1:10000,times=50)
    UnitNames<-rep(colnames(Hits),times=RepVector)
    HitMatrix<-cbind(UnitNames,HitsVector,RepRowsVector)
    TrueLocation<-which(Hits==TRUE)
    MatchMatrix<-HitMatrix[TrueLocation,]
    MatchFrame<-as.data.frame(MatchMatrix)
    MatchFrame[,"RepRowsVector"]<-as.numeric(as.character(MatchFrame[,"RepRowsVector"]))
    SubsetWords<-CleanedWords[MatchFrame[,"RepRowsVector"]]
    Hits2<-grepl(Word1,SubsetWords, ignore.case=FALSE, fixed=TRUE)
    return(Hits2)
    }
                     
greplFixedHits<-parSapply(CandidateUnits[1:50], greplFixed, CleanedWords[1:10000],Word1="fossiliferous")
                     
greplPerl<-function(x,y,Word1="fossiliferous"){
    Hits<-grepl(x,y, ignore.case=FALSE,perl=TRUE)
    HitsVector<-as.vector(Hits)
    RepVector<-apply(Hits,2,length)
    RepRowsVector<-rep(1:10000,times=50)
    UnitNames<-rep(colnames(Hits),times=RepVector)
    HitMatrix<-cbind(UnitNames,HitsVector,RepRowsVector)
    TrueLocation<-which(Hits==TRUE)
    MatchMatrix<-HitMatrix[TrueLocation,]
    MatchFrame<-as.data.frame(MatchMatrix)
    MatchFrame[,"RepRowsVector"]<-as.numeric(as.character(MatchFrame[,"RepRowsVector"]))
    SubsetWords<-CleanedWords[MatchFrame[,"RepRowsVector"]]
    Hits2<-grepl(Word1,SubsetWords, ignore.case=FALSE, perl=TRUE)
    return(Hits2)
    }
                     
greplPerlHits<-parSapply(CandidateUnits[1:50], greplPerl,CleanedWords[1:10000], Word1="fossiliferous")
    
greplPatternsPerl<-function(x,y,Word1="fossiliferous"){ 
    Hits<-grepl(x,y,perl=TRUE)&grepl(Word1,y,perl=TRUE)
    return(Hits)
    }

PatternsPerlHits<-parSapply(CandidateUnits[1:50], greplPatternsPerl, CleanedWords[1:10000],Word1="fossiliferous")

                 
greplPatternsFixed<-function(x,y,Word1="fossiliferous"){ 
    Hits<-grepl(x,y,fixed=TRUE)&grepl(Word1,y,fixed=TRUE)
    return(Hits)
    }

PatternsFixedHits<-parSapply(CandidateUnits[1:50], greplPatternsFixed, CleanedWords[1:10000], Word1="fossiliferous")
 
                     
                     
                     
                     
                     
                     
Test<-grepl("Lexington Limestone",Sentences,fixed=TRUE) & grepl("fossil",Sentences,fixed=TRUE)                   
                              
function2<-function(CandidateUnits,CleanedWords){
  Hits<-sapply(CandidateUnits[1:15, function(x,y) grep(x,y, ignore.case=FALSE, perl=TRUE, CleanedWords[1:5000])
  return(Hits)
  }    
    
    
function2<-sapply(CandidateUnits[1:15, function(x,y) grepl(x,y, ignore.case=FALSE, fixed=TRUE, perl=TRUE),CleanedWords[1:5000]) 
sapply(CandidateUnits[1:15],function(x,y) grepl(x,y,ignore.case=FALSE,fixed=TRUE),
sapply(CandidateUnits[1:15],function(x,y) grepl(x,y,ignore.case=FALSE,fixed=TRUE),CleanedWords[1:5000]) & 
grepl("fossil",CleanedWords[1:5000],fixed=TRUE) & grepl("fossiliferous",CleanedWords[1:5000]),
sapply(CandidateUnits[1:15],function(x,y) grepl(x,y,ignore.case=FALSE,fixed=TRUE),CleanedWords[1:5000]) & 
grepl("fossil",CleanedWords[1:5000],fixed=TRUE)

  
  
sapply(LongUnitDictionary[1:15],function(x,y) grepl(x,y,ignore.case=FALSE,perl=TRUE),CleanedWords[1:5000]),
sapply(LongUnitDictionary[1:15],function(x,y) grep(x,y,ignore.case=FALSE,fixed=TRUE,pearl=TRUE),CleanedWords[1:5000]), 
sapply(LongUnitDictionary[1:15],function(x,y) grep(x,y,ignore.case=FALSE),CleanedWords[1:5000]),replications=20)

Test<-grepl("Lexington Limestone",Sentences,fixed=TRUE) & grepl("fossil",Sentences,fixed=TRUE)
