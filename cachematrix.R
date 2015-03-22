## Put comments here that give an overall description of what your
## functions do

##!Written by Mitchell Masuda
##!Functions are meant to use the <<- operator to store/cache a matrix and its inverse.
##!1st function creates the matrix, 2nd calc's inverse, but checks first to see if cached.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
##!set the value of the matrix  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
##!get the value of the matrix
  get<-function() x

##!set value of matrix inverse
  setsolve<-function(solve) m <<-solve
##!get the value of the inverse  
  getsolve<-function() m
  list(set=set, get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}


## Write a short comment describing this function
##!Function first checks if inverse of matrix is cached
##!If cached, messages the user it's accessing cached data
##!If not cached, calculates inverse of matrix and returns it.
cacheSolve <- function(x, ...) {

##!Double negative!
##! If 'm' is NOT NULL, then inverse is cached
##! returns a message and cached data
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }

##! If solution not cached, passes x$get to a matrix and solves
  data<-x$get()
  m<-solve(data, ...)
  x$setsolve(m)
  m
}
