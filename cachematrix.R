## Put comments here that give an overall description of what your
## I have mostly adapted the given code examples to perform the task as required. Below the two function definitions, I have added code to test the behavior and output of my two functions, which should be self-explanatory.

## Write a short comment describing this function
## This function takes a matrix as input value (which is assumed to be invertible). It defines setter and getter functions for the input-matrix ('set' and 'get') and its inverse ('setinverse' and 'getinverse'), which are added to a list object that is returned by the function. By taking advantage of the lexical scoping rules of R, we can now set and get the values of the matrix using the functions returned in the list by using the '<<-' operator to assign values within a different scope.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # set initial value of inverted matrix to NULL
        set <- function(y) { # define setter function for matrix
            x <<- y
            inv <<- NULL # this is important, since if we change the value of the matrix, we want the code to recalculate the value of the inverted matrix
        }
        get <- function() x # inline definition for getter of matrix value
        setinverse <- function(invInput) inv <<- invInput # inline definition for setter of inverted matrix
        getinverse <- function() inv # inline definition for getter of inverted matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # return list of setter and getter functions
}


## Write a short comment describing this function
## This function uses the setter and getter functions provided by the special matrix object created with 'makeCacheMatrix'. It checks if the inverse for the matrix has already been calculated in if(!is.null(inv)) { ... }. If the the inverse exists it is returned. If not, it is calculated and the function uses the setter 'setinverse' to set the resulting value as the cached value for the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() # get value of cached inverted matrix
        if(!is.null(inv)) { # check if inverted matrix has already been calculated
            message("CACHED: Getting cached inverse matrix.")
            return(inv)
        }
        data <- x$get() # get matrix value
        inv <- solve(data, ...) # use 'solve' function to invert matrix
        x$setinverse(inv) # set matrix inverse
        inv # return inverse value
}

#### test matrix inversion code #### 

# crate matrix and calculate its inverse
mat <- matrix(c(5,2,4,1,5,6,9,2,1.7),nrow=3,ncol=3)
matCached <- makeCacheMatrix(mat)

message("Original matrix:")
print(matCached$get())

message("Get inverse matrix. This outputs NULL, since it is not calculated yet:")
print(matCached$getinverse())

message("Invert matrix and reprint. This now gives the inverse matrix:")
cacheSolve(matCached)
print(matCached$getinverse())

message("Recalculated inverse matrix and reprint. Now shows cached version:")
cacheSolve(matCached)
print(matCached$getinverse())

#### test inversion result #### 

message("Original vector:")
vecOrig <- as.matrix(c(1,5,7.4)) # We need to define the vector as a matrix, so that the all.equal(...) test does not fail below.
print(vecOrig)

vecTrafo <- matCached$get() %*% vecOrig
message("Transformed vector:")
print(vecTrafo)

vecBackTrafo <- matCached$getinverse() %*% vecTrafo
message("Backwards transformed vector:")
print(vecBackTrafo)

message("All equal?:")
print(all.equal(vecBackTrafo,vecOrig))