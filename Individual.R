Individual <- function(dimensions)
{
  
  me <- list(
     data = replicate(dimensions,sample(-100:100,1,rep=TRUE))
  )
  
  ## Set the name for the class
  class(me) <- append(class(me),"Individual")
  return(me)
}