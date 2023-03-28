## Create a pair of functions that cache the inverse of a matrix.
## Create a special "matrix" object that can cache its inverse which will be used later

makeCacheMatrix<-function(x=matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setInverse<-function(invermatrix) inverse<<-invermatrix
  getInverse<-function() inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)


}


## Compute the inverse of the outcome created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve<-function(x, ...) {
  inverse<-x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setInverse(inverse)
  inverse
}
