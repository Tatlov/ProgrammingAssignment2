# Caching the inverse of a matrix
# ===============================
# 
# Creates an object CacheMatrix via function makeCacheMatrix
# that consists of a matrix and its inverse if it has been calculated 
# using cacheSolve.
#
# It is assumed that the matrices are invertible, i.e. that the inverse can be 
# computed using solve.
# 
# Programming Assignment for Peer Assessment
# in the Coursera Course: R Programming.
#
# Template file from
# https://github.com/rdpeng/ProgrammingAssignment2
#
# Tests for the functions in this script can be found in cachematrix_test.R


# makeCacheMatrix creates a CacheMatrix object.
# The CacheMatrix object is a list of functions:
#   - get(): returns the matrix
#   - set( m ): stores a matrix m and resets 
#               the inverse to NULL.
#   - get_inverse(): returns the inverse of the matrix.
#                    If the inverse has not been calculated yet,
#                    it returns NULL.
#   - set_inverse( inv ): stores inv, the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # Args:
    #   x is assumed to be an invertible matrix
    #
    # Returns:
    #   CacheMatrix
    
    x_inverse <- NULL
    
    # the matrix itself
    set <- function( m ){
        x <<- m
        x_inverse <<- NULL
    }
    get <- function() x
    
    # the inverse of the matrix
    get_inverse <- function() x_inverse
    # set the inverse if m_inverse is the inverse
    set_inverse <- function( m_inverse ){
        # check if m_inverse is the inverse of x
        # with a tolerance
        tol <- 1.0e-4
        if ( all( abs( x %*% m_inverse - diag(nrow(m_inverse)) ) < tol ) ) {
            x_inverse <<- m_inverse
        } else {
            # m_inverse is not the inverse of x
            stop( 'This is not the correct inverse.')
        }
        
    }
    # export the CacheMatrix (list of functions)
    list( get = get, set = set, 
          get_inverse = get_inverse, set_inverse = set_inverse )
}


# cacheSolve returns the inverse of a CacheMatrix as a matrix.
#
# If the inverse has been computed before,
# it uses the cached result instead of recomputing it.
# Otherwise it returns the inverse computed with solve.
cacheSolve <- function(x, ...) {
    # Args:
    #   x a CacheMatrix
    #   ... arguments are passed to solve
    #
    # Returns:
    #   inverse_x, a matrix
    
    # Check if the inverse has been computed already.
    # If it has been computed before, return the cached result.
    inverse_x <- x$get_inverse()
    if (!is.null(inverse_x)){
        message("using cached data")
        return( inverse_x )
    }
    # Compute the inverse of x using solve.
    inverse_x <- solve(x$get(), ...)
    # Store the result for future use.
    x$set_inverse(inverse_x)
    # Return the computed inverse
    inverse_x
}
