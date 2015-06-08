source('Population.R')
GeneticAlgorithm <- function(populationSize, generations, fitnessFunction, dimensions)
{
  CHAMPIONS_FRACTION = 0.1;
  TOURNAMENT_SIZE = 2;
  CROSSOVER_COLUMNS_EXCHANGE_RATE = 0.5
  MUTATION_RATE = 0.1
  MUTATION_COLUMNS_RATE = 0.5
  MINIMUM_ERROR = 0.00000001
  
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  ErrorList <- c()
  
  Population = Population(populationSize, dimensions, fitnessFunction)
  PopulationSize = populationSize
  Generations = generations
  Dimensions = dimensions
  FitnessFunction = fitnessFunction
  CurrentGeneration = 1
  
  me <- list(
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,
    
    ## Define the accessors for the data fields.
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    },
    
    getPopulation = function()
    {
      return(get("Population",thisEnv))
    },
    getGenerations = function()
    {
      return(get("Generations",thisEnv))
    },
    getDimensions = function()
    {
      return(get("Dimensions",thisEnv))
    },
    getFitnessFunction = function()
    {
      return(get("FitnessFunction",thisEnv))
    },
    getErrorList = function()
    {
      return(get("ErrorList",thisEnv))
    },
    run = function() {
      
#       message(paste("Generation 1 started. Current champion:"))
      champion <- Population$getChampion()
#       print(champion)
      currentError <- champion[1]
      ErrorList <- append(ErrorList, currentError)
      
      for(generation in 1:Generations)
      {
        #message(paste("Generation ",generation," evolving")
        evolve()
        #message(paste("Generation ",generation," evolved")
#         message(paste("Generation ",generation," evolved. New champion:"))
        champion <- Population$getChampion()
#         print(champion)
        currentError <- champion[1]
        ErrorList <- append(ErrorList, currentError)
        if(currentError <= MINIMUM_ERROR)
        {
#           message("Champion reached minimum error. Stopping further evolution.")
          break;
        }
      }
      
      return(ErrorList)
    }
  )
  ## performs one evolutional step
  evolve = function()
  {
    newIndividuals <- matrix(, nrow = PopulationSize, ncol = Dimensions)
    ## get the best 10% of current individuals
    bestIndividuals <- Population$getBestIndividuals(CHAMPIONS_FRACTION)
    ## Note: there should at least 2 best individuals for correction dimensions handling
    bestIndividualsCount <- length(bestIndividuals[,1])
    ## first part (CHAMPIONS_FRACTION) of newIndividuals with bestIndividuals
    newIndividuals[1:bestIndividualsCount,] <- bestIndividuals
    
    newIndividualChildsStart <- bestIndividualsCount + 1
    ## selection and crossover
    for(i in newIndividualChildsStart:PopulationSize)
    {
      ## get first parent via selection
      parent1 <- selection()
      ## get second parent via selection
      parent2 <- selection()
      ## perform crossover of parents
      child <- crossover(parent1, parent2)
      newIndividuals[i,] <- child
    }
    
    ## mutation
    ## skip champions mutation (start from where childs have started)
    for(i in newIndividualChildsStart:PopulationSize)
    {
      if(sample(1:100,1) < MUTATION_RATE * 100)
      {
        newIndividuals[i,] <- mutation(newIndividuals[i,])
      }
    }
    
    Population$setIndividuals(newIndividuals)
  }
  
  ## Selection
  ## Method: tournament
  selection = function()
  {
    tournamentMembers <- Population$getIndividuals()[sample(1:PopulationSize,TOURNAMENT_SIZE),]
    ## get the best tournament member (evaluation of fitness function) - call him a champion
    tournamentChampion <- tournamentMembers[order(apply(tournamentMembers,1,FitnessFunction))[1],]
    return(tournamentChampion)
  }
  
  ## Crossover
  ## parents must have equal dimensions
  crossover = function(parent1, parent2)
  {
    #     message("Crossover of parents")
    #     print(parent1)
    #     print(parent2)
    child <- parent1
    ## get number of columns to exchange between parents
    exchangeColumnsCount <- floor(Dimensions * CROSSOVER_COLUMNS_EXCHANGE_RATE)
    ## get random exchange columns
    exchangeColumns <- sample(1:Dimensions, exchangeColumnsCount)
    ## exchange columns
    child[exchangeColumns] <- parent2[exchangeColumns]
    #     message("Result child")
    #     print(child)
    return(child)
  }
  
  ## Mutation
  mutation = function(individual)
  {
    #     message("Mutation of individual")
    #     print(individual)
    ## get number of mutation columns
    mutationColumnsCount <- floor(Dimensions * MUTATION_COLUMNS_RATE)
    ## get random mutation columns
    mutationColumns <- sample(1:Dimensions,mutationColumnsCount)
    ## get fully mutated individual
    fullyMutatedIndividual <- individual * (sample(-200:200,1) / 100)
    #     message("IndividualFullyMutated")
    #     print(fullyMutatedIndividual)
    ## mutate individual
    individual[mutationColumns] <- fullyMutatedIndividual[mutationColumns]
    #     message("NewIndividual")
    #     print(individual)
    return(individual)
  }
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"GeneticAlgorithm")
  return(me)
}