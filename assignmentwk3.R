##This is a pair of functions tat cache the inverse of a matrix.
##It creates a special 'matrix' object to cache its inverse.
makecachematrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  get <-function() {x}
  setInverse<-function(inverse) {inv <<- inverse}
  getInverse<-function() {inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
##The function below computes the inverse of the above 'special'
##matrix created by the makecachematrix.
cachesolve<-function(x, ...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("obtaining cached data")
    return(inv)
  }
  mat<- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}