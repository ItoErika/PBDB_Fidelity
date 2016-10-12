library("rbenchmark")


benchmark(
  
function1<-function(CandidateUnits,CleanedWords){
  
  
  sapply(CandidateUnits[1:15, function(x,y) grep(x,y, ignore.case=FALSE, perl=TRUE, CleanedWords[1:5000])
  
    
    
    
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
