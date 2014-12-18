## Create a matrix and take the inverse of that matrix through caching and scoped functions

## Create a matrix with the functions get,set,getinverse and set inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function(){x}
    setinverse<-function(inverse){i<<-inverse}
    getinverse<-function(){i}
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}




## Fetch the inverse of the matrix and store the data for future calls

cacheSolve <- function(x, ...) {
    i<-x$getinverse()
    if (!is.null(i)){
        message('getting cached data')
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setinverse(i)
    i
}
