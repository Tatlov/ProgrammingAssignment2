# tests for cachematrix.R
source( "cachematrix.R" )

# to run the tests, use RUnit
# library( RUnit )
# printTextProtocol( runTestFile("cachematrix_test.R") )

test_CacheMatrix_set_and_set_inverse <- function(){
    m0 <- matrix(2:5,2,2)
    inverse_m0 <- solve(m0)
    m <- matrix(1:4,2,2)
    # test set_inverse matrix
    cache_matrix <- makeCacheMatrix(m0)
    cache_matrix$set_inverse(inverse_m0)
    checkEquals( inverse_m0, cache_matrix$get_inverse() )
    # test set matrix
    cache_matrix$set( m )
    checkEquals( m, cache_matrix$get() )
    # test reset of inverse
    checkTrue( is.null( cache_matrix$get_inverse() ))
    # test that set_inverse can only set a true inverse
    checkException( cache_matrix$set_inverse(inverse_m0) )
}

test_CacheMatrix_get_inverse <- function(){
    # test get inverse if inverse has not been set
    cache_matrix <- makeCacheMatrix()
    checkTrue( is.null( cache_matrix$get_inverse() ))
}

test_CacheMatrix_get <- function(){
    m <- matrix(1:4,2,2)
    # test get matrix
    cache_matrix <- makeCacheMatrix( m )
    checkEquals( m, cache_matrix$get() )
    
    # test get for an empty matrix
    cache_matrix <- makeCacheMatrix()
    checkEquals( matrix(), cache_matrix$get() )
    
}


#test the inverse of an invertible matrix
test_matrix_inverse <- function(){
    m <- matrix(1:4, 2, 2)
    cache_matrix <- makeCacheMatrix( m )
    # test computation of the inverse
    inverse_m <- cacheSolve(cache_matrix)
    checkEqualsNumeric(diag(2), m %*% inverse_m, tolerance = 1.0e-4)
    # test if it stores the inverse
    checkEqualsNumeric(diag(2), m %*% cache_matrix$get_inverse(), 
                       tolerance = 1.0e-4)
    # test the cached result
    inverse_m <- cacheSolve(cache_matrix)
    checkEqualsNumeric(diag(2), m %*% inverse_m, tolerance = 1.0e-4)
}

