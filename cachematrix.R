## Put comments here that give an overall description of what your
## functions do

## The steps for this function are in the comments
# We use the first function"makecachematrix" to simply find the inverse of matrix
#"library(MASS)" can be used to solve any kind of matrix\, square or non square matrix
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL   #starting inverse as a null
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x      #get() gives us a function to get the matrix (x)
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%%x.    # the " %%" is used to inverse the matrix
  }
list(set=set,get=get,setinv=setinv,getinv=getinv)

}



## Write a short comment describing this function
# we use this cache our data
cacheSolve <- function(x, ...)# 
  { 
  inv<-x$getinv()
  if(!is.null(inv)){
    message("retrieving cached data")
    return(inv)#checking if inverse is null and it returns the inverse value as well
    
  }
  data<-x$get()
  inv<-solve(data,...)#calulates the inverse of the cached matrix
  x$setinv(inv)
  inv  #printing it
  
}
## Return a matrix that is the inverse of 'x'

