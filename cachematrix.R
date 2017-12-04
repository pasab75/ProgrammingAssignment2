## Put comments here that give an overall description of what your
## functions do

## makes a special instance of a matrix that has the ability to cache its solve
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL;
    set <- function(y = matrix()){ #this allows for the over writing of the underlying matrix object, clears the caches of the inverse
        x <<- y;
        m <<- NULL;
    }
    
    get <- function() x;
    setsolve <- function(solve) m <<- solve; # sets the cached inverse 
    getsolve <- function(solve) m; # returns the cached inverse
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve);
}




## This function takes a makeCacheMatrix constructed instance,
## and either returns its cached inverse or solves for it if needed
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve();
    if(!is.null(m)){
        message("getting cached data");
        return(m)
    }
    data <- x$get();
    print(data)
    m <- solve(data, ...);
    x$setsolve(m);
    m;
}
