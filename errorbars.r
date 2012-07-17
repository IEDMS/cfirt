##### based on http://monkeysuncle.stanford.edu/?p=485

error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
       if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
       stop("vectors must be same length")
       arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
      }
      
yerror.bar <- function(x, y, upper, lower=upper, length=0.05,...){
       if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
       stop("vectors must be same length")
       arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
      }

xerror.bar <- function(x, y, upper, lower=upper, length=0.05,...){
       if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
       stop("vectors must be same length")
       arrows(x+upper,y, x-lower, y, angle=90, code=3, length=length, ...)
      }



##########################
##### END FUNCTIONS ######
##########################
