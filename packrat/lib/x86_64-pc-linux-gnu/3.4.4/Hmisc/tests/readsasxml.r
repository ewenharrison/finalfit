if(require(XML)) {
  w <- xmlTreeParse('demo.xml')

  z <- w$doc$children$Transfer$children$Catalog$children$CatalogSchemas$children$Schema$children$SchemaTables$children

  u <- z[[length(z)]]$children

  v <- matrix(unlist(u), nrow=length(u), byrow=T)
  
  v[,seq(3,ncol(v),by=4)][1,]

##  [1] "#_4" "#_5" "#_6" "#_7" "#_8" "#_9"

  v[,seq(5,ncol(v),by=4)]

##      [,1]  [,2]  [,3]   [,4] [,5] [,6]        
## [1,] "111" "ABC" "27"   "1"  "2"  "1976-04-22"
## [2,] "222" "XYX" "35.2" "2"  "1"  "1968-02-10"
## [3,] "333" "WHO" "19"   "1"  "1"  "1984-04-20"
## [4,] "444" "WHY" "45.7" "1"  "3"  "1957-08-14"
## [5,] "555" "HUH" "82"   "2"  "3"  "1921-05-06"

## Process variables one column of v at a time, converting appropriate
## ones to numeric.
}
