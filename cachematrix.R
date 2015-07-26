## Matrix solution caching
##
## `makeCacheMatrix` and `cacheSolve` are used together to solve and cache an
## inverted matrix.
##
## Example usage:
##
## ```
## m <- matrix(c(1, 2, 3, 4), nrow=2)
## x <- makeCacheMatrix(m)
## solution <- cacheSolve(x)
## all.equal(solution, solve(m))    # TRUE
## ```

#' Creates a "cache matrix" object, optionally accepting an initial matrix.
#' The returned object is meant to be used with `cacheSolve()`.
#' 
#' `$get()`: Gets the matrix to be solved.
#' `$set()`: Sets the matrix to be solved. This invalidates the cache.
#'
#' Notice: Users should not call the internal functions
#' `$getSolution()` and `$setSolution()`, as doing so could lead to unexpected
#' results in `cacheSolve()`.

makeCacheMatrix <- function(x = matrix()) {
    solution <- NULL
    
    get <- function() x
    
    set <- function(y) {
        x <<- y
        solution <<- NULL
    }
    
    getSolution <- function() solution
    
    setSolution <- function(s) {
        solution <<- s
    }
    
    list(
        get = get,
        set = set,
        getSolution = getSolution,
        setSolution = setSolution
    )
}


#' Returns the solved matrix of `x`. `x` must be a "cache matrix" object.
#'
#' This is functionally equivalent to `solve(x$get())`, but relies on a cached
#' solution when possible.
#' 
#' Notice: I removed the ellipsis `...` that gets passed to solve
#' because I believe this behavior is incorrect.
#' (see https://class.coursera.org/rprog-030/forum/thread?thread_id=424)

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getSolution()
    if (is.null(m)) {
        # The result isn't cached - solve it!
        
        message("Solving...")
        solution <- solve(x$get())
        x$setSolution(solution)
        
        solution
    } else {
        # The result is cached.
        m
    }
}