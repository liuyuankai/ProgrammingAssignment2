## The below two functions are used to create a 
## special object that stores a numeric matrix and
## caches its inverse

## makeCacheMatrix function creates a list ,which 
## contains the below four functions:
## 1. set: set the value of the matrix
## 2. get: get the value of the matrix
## 3. setinverse: set the value of the matrix inverse
## 4. getinverse: get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(inverse){
                i <<- inverse
        }
        getinverse <- function(){
                i
        }
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the 
## "list" object created by makeCacheMatrix function.
## if inverse has already been cached,it return the cached
## value.Otherwise, it calculates the inverse that will be 
## cached and returned;

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

