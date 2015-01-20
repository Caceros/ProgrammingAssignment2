## makeCacheMatrix creats a special matrix
## cacheSolve calcutates the inverse of the special matrix created above, 
## it can skip the calcutation and return the inverse directly if 
## it has been calculated before.
##test
## get() returns value of the matrix
## setmatrix can store the inverse in m
## getmatrix can get the value of m 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## If the inverse has been calculated before, then return the inverse directly
## Else it will calculate the inverse

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
