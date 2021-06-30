## Put comments here that give an overall description of what your
## functions do
##Grecia Pacheco

## Write a short comment describing this function
## makeCacheMatrix is a function that take as a parameter a matrix and canculates its inverse, considering that it´s possible
## then in caches the result, in order to have the possibility of being used for the next function its important the creation of getters and setters
#install.packages("matlib")
library(matlib)
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setInverse<-function(inv)m<<-inv
  getInverse<-function()m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
##This funcction check if the inverse had been already calculated; if so returns the value from cache, else calculates it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
      print("Getting result from cache")
      return(m)
  }
  
  toChange<-x$get()
  n<-inv(toChange)
  x$setInverse(n)
  n
  
}

#TESTING
A <- matrix( c(5, 1, 0, 3, -1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
result<-makeCacheMatrix(A)
x<-cacheSolve(result)
x
x<-cacheSolve(result)
x
