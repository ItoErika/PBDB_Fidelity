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

#### Initial number rows (sentences) with of unit name hits in SubsetDeepDive that are marine and sedimentary according to macrostrat, and unfossiliferous according to PBDB:

`length(unlist(UnitHits))`
````R
31,175
````
#### Number of rows (sentences) with unit name hits in SubsetDeepDive after removing sentences which contain more than one unit name:

`nrow(SingleHitData)`
````R
25,648
````

#### Number of rows (sentences) with unit name hits in SubsetDeepDive after removing sentences comprised of more than 350 characters:

`nrow(SingleHitsCut)
`
````R
22,400
````

#### Number of rows (sentences) in SingleHitsCut with the word "fossils" or "fossiliferous":

`nrow(FossilData)`
````R
759
````

#### Number of unique units which appear in rows (sentences) of SingleHitscut with the word "fossil" or "fossiliferous":
`length(unique(FossilData[,"UnitNames"]))`
````R
242
````
