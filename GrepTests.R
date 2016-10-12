library("rbenchmark")
library("RCurl")
library("pbapply")
library("doParallel")
library("data.table")

Cluster<-makeCluster(3)
clusterExport(cl=Cluster,varlist=c("grepFunction","greplFunction","greplFixed","greplPerl","greplPatternsPerl","greplPatternsFixed"))

# Record start time
Start<-print(Sys.time())
# Apply grep to cleaned words
benchmark(
sapply(CandidateUnits[1:50], grepFunction, CleanedWords[1:10000], Word1="the"),
sapply(CandidateUnits[1:50], greplPerl, CleanedWords=CleanedWords[1:10000], Word1="the"),
sapply(CandidateUnits[1:50], greplFixed, CleanedWords=CleanedWords[1:10000], Word1="the"),
sapply(CandidateUnits[1:50], greplPatternsPerl, CleanedWords[1:10000],Word1="the"),
sapply(CandidateUnits[1:50], greplPatternsFixed, CleanedWords[1:10000], Word1="the"),  
replications=100)
# Record end time
End<-print(Sys.time())
# Find total runtime
End-Start
  
stopCluster(Cluster)




  
grepFunction<-function(CandidateUnits=CandidateUnits[1:50],CleanedWords=CleanedWords[1:10000],Word1="the"){
  Hits<-grep(CandidateUnits,CleanedWords, ignore.case=FALSE, perl=TRUE)
  SubsetWords<-CleanedWords[Hits]
  Location<-which(Hits==TRUE)
  SubsetMatrix<-cbind(Location,SubsetWords)
  Hits2<-grep(Word1,SubsetWords, ignore.case=FALSE, perl=TRUE)
  return(SubsetMatrix[which(Hits2==TRUE)]) 
  }

grepFunctionHits<-sapply(CandidateUnits[1:50], grepFunction, CleanedWords[1:10000], Word1="the")
                                                      
greplPerl<-function(CandidateUnits=CandidateUnits[1:50], CleanedWords=CleanedWords[1:10000],Word1="the"){
     Hits<-grepl(CandidateUnits,CleanedWords, ignore.case=FALSE, perl=TRUE)
     Location<-which(Hits==TRUE)
     SubsetWords<-CleanedWords[Location]
     SubsetMatrix<-cbind(Location,SubsetWords)
     Hits2<-grepl(Word1,SubsetWords, ignore.case=FALSE, perl=TRUE)
     return(SubsetMatrix[which(Hits2==TRUE)])
     }

greplPerlHits<-sapply(CandidateUnits[1:50], greplPerl, CleanedWords=CleanedWords[1:10000], Word1="the")                               
                   
greplFixed<-function(CandidateUnits=CandidateUnits[1:50], CleanedWords=CleanedWords[1:10000],Word1="the"){
     Hits<-grepl(CandidateUnits,CleanedWords, ignore.case=FALSE, fixed=TRUE)
     Location<-which(Hits==TRUE)
     SubsetWords<-CleanedWords[Location]
     SubsetMatrix<-cbind(Location,SubsetWords)
     Hits2<-grepl(Word1,SubsetWords, ignore.case=FALSE, fixed=TRUE)
     return(SubsetMatrix[which(Hits2==TRUE)])
     }

greplFixedHits<-sapply(CandidateUnits[1:50], greplFixed, CleanedWords=CleanedWords[1:10000], Word1="the")                     
    
greplPatternsPerl<-function(x,y,Word1="the"){ 
    Hits<-grepl(x,y,perl=TRUE)&grepl(Word1,y,perl=TRUE)
    return(Hits)
    }

PatternsPerlHits<-sapply(CandidateUnits[1:50], greplPatternsPerl, CleanedWords[1:10000],Word1="the")
                 
greplPatternsFixed<-function(x,y,Word1="the"){ 
    Hits<-grepl(x,y,fixed=TRUE)&grepl(Word1,y,fixed=TRUE)
    return(Hits)
    }

PatternsFixedHits<-sapply(CandidateUnits[1:50], greplPatternsFixed, CleanedWords[1:10000], Word1="the")
 
                     
                     
                     
                     
                     
                     
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
