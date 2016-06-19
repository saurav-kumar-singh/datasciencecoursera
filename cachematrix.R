## Caching the Inverse of a Matrix

## create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set=function(y){
    x<<-y
    inv<<-NULL
  }
  get=function()x
  setinv=function(inverse) inv<<- inverse
  getinv=function()inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## compute the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        inv=x$getinv()
        if(!is.null(inv)){
          #get it from  cache and skip the computation
          message("getting cached data")
          return(inv)
        }
        #otherwise calculate the inverse
        mat.data=x$get()
        inv=solve(mat.data,...)
        #set the value of the  inverse in the cache via the setinv function.
        x$setinv(inv)
        return(inv)
        }
