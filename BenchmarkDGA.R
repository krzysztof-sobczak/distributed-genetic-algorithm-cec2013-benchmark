library("cec2013", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
source('DistributedGeneticAlgorithm.R')

RUN_REPEAT = 10

for(i in 1:3) {
  cec2013Unshifted = function(x) { 
    shifted_result <- cec2013(i, x)
    return(shifted_result + 1400 - 100*(i-1) - 100*((i-1)>13))
  }
  for(dimensions in c(2,5,10,30)) {
    ga <- DistributedGeneticAlgorithm(100,200,cec2013Unshifted, dimensions)
    message(paste("Distributed GA - CEC2013 Benchmark function ", i, dimensions,"D - run: ",1))
    results = ga$run()
    resultsError = min(results)
    bestResults = results
    bestError = resultsError
    for (r in 2:RUN_REPEAT)
    {
      ga <- DistributedGeneticAlgorithm(100,200,cec2013Unshifted, dimensions)
      message(paste("Distributed GA - CEC2013 Benchmark function ", i, dimensions,"D - run: ",r))
      results = ga$run()
      resultsError = min(results)
      if(resultsError < bestError)
      {
        bestResults = results
        bestError = resultsError
      }
    }
    plot(bestResults,
         type="b",
         ylab="Error",
         xlab="Generation (number)",
         main=paste("Distributed GA - CEC2013 Benchmark function ", i, dimensions,"D"))
  }
}