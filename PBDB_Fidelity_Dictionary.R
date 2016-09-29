# Load Libraries 
library(RCurl)

# Download dictionary of unit names from Macrostrat Database
UnitsURL<-paste(https://macrostrat.org/api/units?lith_class=sedimentary&environ_class=marine&project_id=1&response=long&format=csv)
GotURL<-getURL(UnitsURL)
UnitsFrame<-read.csv(text=GotURL,header=TRUE)
# Extract actual unit names
UnitsDictionary<-unique(UnitsFrame[,"unit_name"])
