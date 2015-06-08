source('GeneticAlgorithm.R')
DistributedGeneticAlgorithm <- function(populationSize, generations, fitnessFunction, dimensions)
{
  ISLANDS_COUNT = 5
  MIGRATIONS_COUNT = 5
  MIGRATION_FRACTION = 0.2
  
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
  
  Islands <- list()
  IslandPopulationSize <- floor(PopulationSize / ISLANDS_COUNT)
  IslandGenerations <- floor(Generations / ISLANDS_COUNT)
  
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
    run = function() {
      ## init islands
#       message("Creating islands")
      initIslands()
      
      ## islands evolution
#       message("Islands evolution")
      bestIslandErrorList <- islandsEvolution()
      
      islandNumbers <- rep(1:ISLANDS_COUNT)
      migratingIndividualsCount <- floor(IslandPopulationSize * MIGRATION_FRACTION)
      ## migration
      for(migration in 1:MIGRATIONS_COUNT)
      {
        message(paste("Starting islands migration - ", migration))
        ## get migration targets for each island
        ## ensure that every island has assigned different island
        migrationTargets <- sample(islandNumbers)
        while(sum(migrationTargets==islandNumbers) > 0) 
        { 
          migrationTargets <- sample(islandNumbers) 
        }
        
        ## shuffle all islands before migration
        ## we will be taking the first chunk of each island
        for(island in 1:ISLANDS_COUNT)
        {
          Islands[[island]]$getPopulation()$getIndividuals()[sample(nrow(Islands[[island]]$getPopulation()$getIndividuals())),]
        }
        
        ## perform migrations
        for(island in 1:ISLANDS_COUNT)
        {
#           message(paste("Migrating ",migratingIndividualsCount," individuals from island ", island, " to island ", migrationTargets[island]))
          ## get migrating individuals
          migratingIndividuals <- Islands[[island]]$getPopulation()$getIndividuals()[1:migratingIndividualsCount,]
#           print(migratingIndividuals)
          ## remove migrating individuals from source island
          Islands[[migrationTargets[island]]]$getPopulation()$setIndividuals(Islands[[island]]$getPopulation()$getIndividuals()[-1:-migratingIndividualsCount,])
          ## add migrating individuals to target island
          Islands[[migrationTargets[island]]]$getPopulation()$setIndividuals(rbind(Islands[[migrationTargets[island]]]$getPopulation()$getIndividuals(),migratingIndividuals))
        }
        
        ## islands evolution
#         message(paste("Islands evolution after migration - ",migration))
        bestIslandErrorList <- append(bestIslandErrorList, islandsEvolution())
      }
      
      return(bestIslandErrorList)
    }
  )
  
  ## initialize islands from population
  initIslands = function()
  {
    for(island in 1:ISLANDS_COUNT)
    {
      Islands[[length(Islands)+1]] <- GeneticAlgorithm(IslandPopulationSize,IslandGenerations,FitnessFunction, Dimensions)
    }
    
    assign("Islands",Islands,thisEnv)
  }
  
  ## islands independent evolution
  islandsEvolution = function()
  {
    bestIslandError <- 1000000000000000
    for(island in 1:ISLANDS_COUNT)
    {
      errorList <- Islands[[island]]$run()
      islandError <- min(errorList)
      if(islandError < bestIslandError)
      {
        bestIsland <- Islands[[island]]
        bestIslandError <- islandError
        bestIslandErrorList <- errorList
      }
    }
    return(bestIslandErrorList)
  }
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"GeneticAlgorithm")
  return(me)
}