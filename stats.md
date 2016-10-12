#### Initial number of documents: 
`length((unique(DeepDiveData[,"docid"])))`
````R
9,508
````

#### Number of documents after subsetting DeepDiveData to only documents that contain sedimentary, marine units with no fossils according to PBDB:
`length((unique(SubsetDeepDive[,"docid"])))`
````R
4,753
````

#### Initial number of document unit name hits in DeepDiveData that are marine and sedimentary according to macrostrat, and unfossiliferous according to PBDB:

`length(unlist(UnitHits))`
````R
31,175
````
#### Number of unit name hits in DeepDiveData after removing sentences which contain more than one unit name:

`length(SingleHitData[,"MatchLocation"])`
````R
25,648
````
