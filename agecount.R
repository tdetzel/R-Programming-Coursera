agecount <- function(age = NULL) {
## Check that "age" is non-NULL; else throw error

  if(is.null(age)) stop("Null value")

## Read "homicides.txt" data file

  homicides <- readLines(paste(getwd(),"homicides.txt", sep = "/"))

## Extract ages of victims; ignore records where no age is given
  
  findage<- regexpr("<dd>.* ([0-9]+) years old</dd>", homicides)
  allages <- regmatches(homicides, findage)
  stripages <- as.numeric(gsub("[^0-9]*","", allages))
  
## Return integer containing count of homicides for that age
  cage <- as.character(age)
##  ctages <- table(stripages)
  ctages <- as.data.frame(table(stripages))
  ifelse(!cage%in%ctages$stripages, 0, ctages[ctages$stripages == cage, "Freq"]) 
}