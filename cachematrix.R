
# makeCacheMatrix  creates  list containing follwowing functions and caches its inverse.
# setmatrix     :to store the matrix
# getmatrix     :to read the matrix
# getinvmatrix  :to read the inverse matrix from cache
# setinvmatrix :to set the matrix in the cache.
# Usage : 
#  x <- makeCacheMatrix( m1) where m1 is a square matrix which can be inverted
# reference setmatrix, getmatrix, setinvmatrix, getinvmatrix by respectively using
# x$getmatrix(), x$setinvmatrix(m1), x$getinvmatrix(), x$setinvmatrix(m1)

makeCacheMatrix  <-  function( mtx = matrix()) {
        
        inv.mtx<-NULL
        
        
        # set the matrix in cache and reset the inverse matrix cache
        setmatrix<-function(y){
                mtx<<-y
                inv.mtx<<-NULL
        }
        # read the matrix
        getmatrix<-function() mtx
        # set the inverse marix  in cache
        setinvmatrix<-function(z) inv.mtx<<- solve(z)
        #read the inverse matrix from cache
        getinvmatrix<-function() inv.mtx
        # create a special vector containint list of functions to do above.
        # This allows reference later with <variable$ge tmatrix() etc
        list(setmatrix=setmatrix, getmatrix=getmatrix,
             setinvmatrix=setinvmatrix,
             getinvmatrix=getinvmatrix)
}

cacheSolve <- function(x,y, ...) {
        # x is the object assigned with makeCacheMatrix()
        # y is the matrix to solve
        # read the matrix and if same, return the inverse matrix from cache
        mtx<-x$getmatrix()
        
        if(identical(mtx,y)){
                
                m <- x$getinvmatrix()
                
                if (! is.null(m)){
                        
                        message("getting cached data")
                        return(m)
                } 
                
        }
        
        # if continue from above,  solve and recache the inverse matrix
        x$setmatrix(y)
        # update inverse matrix
        x$setinvmatrix(y)
        
        m <- x$getinvmatrix()
        
        #return results
        return(m)
}