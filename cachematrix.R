#create a matrix and define mutator and accessor functions for that
#matrix. If a previous
#object of type makeCacheMatrix has had its inverse calculated and
#stored in the cache, make
#CacheMatrix clears the cache.

makeCacheMatrix <-makeMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInv<- function(inverse) inv<<-inverse
  getInv<-function () inv
  list (set=set, get=get,
        setInv=setInv,
        getInv=getInv)
}

#If there is no inverse value in cache, this function is supposed to
#calculate the
#inverse of thde inputed object of type makeCacheMatrix and store it
#in the cache
#, else it retrevies the previously computed inverse from the cache.
#However, this function
#fails to act as intented due to an " Error in as.vector(x, mode) :
#cannot coerce type 'closure' to vector of type 'any'" this error
#occurs when the function
#attempts to calculate and inverse.

cacheSolve<-function(x, ...){
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get
  inv<-solve(data, ...)
  x$setInv(inv)
  inv
}