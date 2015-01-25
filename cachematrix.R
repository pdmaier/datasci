## Hi! My name is Paul Maier. I hope my comments aren't 
## too annoying for you: just wanted to make sure I got
## full credit in that regard at the very least!

## Many thanks to Roger Peng for providing the example 
## which I essentially retool for our purposes. Also a big
## shout-out to the super-helpful folks in the forums, like
## Hussain Boxwala, who write out really high-quality reviews
## because they're nice and they already did the work anyway.

## These two functions work together to first 
## create an inverted matrix, and then cache that matrix
## so we don#'t have to figure it out again when we try
## to do the same thing twice.

## makeCacheMatrix doesn't really do anything the first
## time around. Really, it just gives us a place to store
## the result of cacheSolve once we do that. However, 
## after we run cacheSolve, its result is passed through
## to makeCacheMatrix through the variable i (for inverted).


## Here, for makeCacheMatrix, I'm going to borrow
## heavily from our example, but don't worry!
## I'll also comment it up so I can show I 
## understand what's going on.

makeCacheMatrix <- function(x = matrix()) 
{
	# First, let's make i NULL so when we run things again,
	# values don't begin to crash into each other.
	i <- NULL

	# Now we're going to create a function that 
	# essentially stores i in the global environment,
	# rather than just within this function. The whole 
	# setting y to x thing is admittedly confusing, but
	# I'm going to take it at face value.
	set <- function(y) 
    {
        x <<- y
        i <<- NULL
    }

    # This function literally just gives us x.
    # We make it a function so that the list at the
    # end will be all functions. 
    # Essentially, get <- x.
    get <- function() x

    # This is key: in cacheSolve, we're going to call 
    # this function and put the computed inverted matrix
    # in as the argument (arbitrarily called inversion).
    # Then this function will just save that value as
    # m, thus changing the value of m from NULL (as 
    # specified above) to the actual inverted matrix.
    setinv <- function(inversion) i <<- inversion

    # Just like get, this just returns i, but is set
    # as a function to make things nice and neat in
    # the upcoming list. getinversion <- i.
    getinv <- function() i

    # Here's that list I was talking about! The output,
    # which will be saved to whatever variable this big
    # function is going in, will be a list of these four
    # functions: set, get, setinv, and getinv.
    list(
        set = set, 
        get = get, 
        setinv = setinv, 
        getinv = getinv
        )
}


## Ok, so this is where we do the actual inverting if we#'ve
## never done it before, and we pull the values from the above
## functions if we have.

## Quick note that confused me massively here: you should insert
## the result of makeCacheMatrix as the argument in cacheSolve. 
## So if you say result <- makeCacheMatrix(data), you should 
## then put cacheSolve(result) to get your final answer!

cacheSolve <- function(x, ...) 
{
	# This just pulls the value of i from our previous
	# function and gets it here so we can work with it.
	# It'll either be NULL if we have never done this
	# before, or it'll be the inverted matrix we want.
   	i <- x$getinv()

   	# if i is not null, we've done this before! Pull the
   	# value we've saved and return so this function stops
   	# and gives us what we want.
    if(!is.null(i)) 
    {
        message("getting cached data")
        return(i)
    }

    # Now the reason we do this is because cacheSolve does
    # not get data as an argument. Instead, it gets the result
    # of the prior function. So this just pulls the data from the
    # result of the prior function.
	data <- x$get()

	# This is actually where we do the computation of the 
	# inversion! Woo! The rest is just the paperwork, this
	# is the computation. However, this is hard work: that's
	# why we've buried it as a task only if we haven't done it
	# before.
	i <- solve(data, ...)

	# This ships over our newfound inverted matrix to our storage
	# in the other function so we can refer to it when we need it 
	# again.
	x$setinv(i)

	# Return the inverted matrix! We're done!
	i

}
