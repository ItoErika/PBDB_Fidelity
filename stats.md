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

#### Number of rows (sentences) in SubsetDeepDive:
`nrow(SubsetDeepDive)`
````R
3,250,465
````

#### Initial number of unit name hits in SubsetDeepDive that are marine and sedimentary according to macrostrat, and unfossiliferous according to PBDB:

`length(unlist(UnitHits))`
````R
31,175
````
#### Number of unit name hits in SubsetDeepDive after removing sentences which contain more than one unit name:

`length(SingleHitData[,"MatchLocation"])`
````R
25,648
````

#### Number of unit name hits in SubsetDeepDive after removing sentences comprised of more than 350 characters:

`length(SingleHitsCut[,"MatchLocation"])`
````R
22,400
````
