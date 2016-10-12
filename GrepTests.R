library("rbenchmark")


benchmark(
sapply(CandidateUnits[1:15],function(x,y) grep(x,y,ignore.case=FALSE,fixed=TRUE),CleanedWords[1:5000] & 
),

sapply(LongUnitDictionary[1:15],function(x,y) grepl(x,y,ignore.case=FALSE,perl=TRUE),CleanedWords[1:5000]),
sapply(LongUnitDictionary[1:15],function(x,y) grep(x,y,ignore.case=FALSE,fixed=TRUE,pearl=TRUE),CleanedWords[1:5000]), 
sapply(LongUnitDictionary[1:15],function(x,y) grep(x,y,ignore.case=FALSE),CleanedWords[1:5000]),replications=20)

Test<-grepl("Lexington Limestone",Sentences,fixed=TRUE) & grepl("fossil",Sentences,fixed=TRUE)
