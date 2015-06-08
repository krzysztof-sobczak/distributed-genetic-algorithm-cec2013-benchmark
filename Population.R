source('Individual.R')

Population <- function(size, dimensions, fitnessFunction)
{
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  # matrix with individuals as rows
  Individuals = as.matrix(replicate(dimensions,sample(-100:100,size,rep=TRUE)))
  FitnessFunction = fitnessFunction
  
  me <- list(
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,
    
    ## Define the accessors for the data fields.
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    },
    
    getIndividuals = function()
    {
      return(get("Individuals",thisEnv))
    },
    setIndividuals = function(individuals)
    {
      assign("Individuals",individuals,thisEnv)
      setFitnessFunctionResults()
    },
    getFitnessFunctionResults = function()
    {
      return(get("FitnessFunctionResults",thisEnv))
    },
    ## the best individual as matrix row
    getChampion = function()
    {
      championError = min(FitnessFunctionResults)
      championIndex = which.min(FitnessFunctionResults)
      championIndividual = Individuals[championIndex,]
      return(c(championError, championIndividual))
    },
    getBestIndividuals = function(fraction)
    {
      ## get population size
      populationSize <- length(Individuals[,1])
      ## get chunk size (equal to the number of returned individuals)
      chunkSize <- fraction * populationSize
      ## sort individuals by fitness funtion results
      sortedIndividuals <- Individuals[order(FitnessFunctionResults),]
      ## get best individuals
      return(sortedIndividuals[1:chunkSize,])
    }
  )
  
  setFitnessFunctionResults = function()
  {
    return(assign("FitnessFunctionResults",apply(Individuals,1,FitnessFunction),thisEnv))
  }
  
  setFitnessFunctionResults()
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"Population")
  return(me)
}