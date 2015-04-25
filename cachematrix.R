## Put comments here that give an overall description of what your
## functions do

##creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        # The matrix has to be a square matrix to be able to calculate its inverse using the solve function
        if (nrow(x) != ncol(x))
                stop("The matrix should have the same number of columns and rows")
        
        if (det(x) == 0)
                stop("The matrix is not invertible. Please pass an invertible matrix (its determinant should be <> 0)")
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##  Computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse))
        {
                message("getting cached data")
                return (inverse)
        }
        data <- x$get()
        inverse <- solve(data, diag(nrow(data)))
        x$setInverse(inverse)
        inverse
}

## function to test my function (doesn't test the caching part though, only the correctness of the operation)
test <- function (x = matrix()) {
        cm <- makeCacheMatrix(x)
        
        # The identity matrix should be the same as the product between the original matrix and the result of the cacheSolve function
        diff <- (cacheSolve(cm) %*% x) - diag(nrow(x))
        #Because of rounding errors I used this method of comparing the identity matrix with the product
        all(diff < 0.00000000001)
}

#Examples to try:

# u <- makeCacheMatrix(matrix(rnorm(25), ncol=5))
# u$get()
# test(u$get())

#n<-1000
#u <- makeCacheMatrix(matrix(rnorm(n*n), ncol=n))
#v <- cacheSolve(u)
#test(u$get())

