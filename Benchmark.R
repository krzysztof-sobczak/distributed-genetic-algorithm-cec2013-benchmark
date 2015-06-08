library("cec2013", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
source('GeneticAlgorithm.R')

RUN_REPEAT = 10

for(i in 1:28) {
  cec2013Unshifted = function(x) { 
    shifted_result <- cec2013(i, x)
    return(shifted_result + 1400 - 100*(i-1) - 100*((i-1)>13))
  }
  for(dimensions in c(2, 5, 10, 30)) {
    ga <- GeneticAlgorithm(65,300,cec2013Unshifted, dimensions)
    message(paste("Standard GA - CEC2013 Benchmark function ", i, dimensions,"D - run: ",1))
    results = ga$run()
    resultsError = min(results)
    bestResults = results
    bestError = resultsError
    for (r in 2:RUN_REPEAT)
    {
      ga <- GeneticAlgorithm(65,300,cec2013Unshifted, dimensions)
      message(paste("Standard GA - CEC2013 Benchmark function ", i, dimensions,"D - run: ",r))
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
         main=paste("Standard GA - CEC2013 Benchmark function ", i, dimensions,"D"))
  }
}