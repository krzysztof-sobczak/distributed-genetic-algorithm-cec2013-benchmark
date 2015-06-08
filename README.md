# Standard Genetic Algorithm vs Distributed Genetic Algorithm (CEC2013 Benchmark)
Compare of standard genetic algorithm and distributed genetic algorithm.

Based on CEC2013 benchmark:
http://www.ntu.edu.sg/home/EPNSugan/index_files/CEC2013/Definitions%20of%20%20CEC%2013%20benchmark%20suite%200117.pdf

## Running benchmark 

Standard Genetic Algorithm:
R> source('Benchmark.R')

Distributed Genetic Algorithm:
R> source('BenchmarkDGA.R')

## Standard Genetic Algorithm structure

Individuals are represented as rows in a matrix, where number of columns matches the number of CEC2013 dimensions

1. Initialization - random population within range specified in CEC2013 [-100,100]
2. Selection
  a. Some population champions (default 10%)
  b. Tournament selection
    Tournament involves some random individuals (default 5). The best one is chosen.
    We take 2 parents via 2 tournament and pass them to crossover.
3. Crossover
  Child is a first parent vector with some random columns exchanged with the second parent columns (default 50%)
4. Mutation
  Mutation result is a base individual with some random columns (default 50%) multiplied by a random number from range <-2,2>
5. Termination
  We finish after specified number of generations or when the error is below 10^-8

## Distributed Genetic Algorithm structure

Based on islands model.

1. Create n islands (default 5)
2. Divide population into those islands
3. Perform evolution on each island separetely
4. Migration
  a. Take some random individuals from each island (default 10%)
  b. Move those individuals to another random island
  c. Find best island

Final error list is based on partial error lists of current best islands.

## Implemention notes:
Object-oriented R implementation of genetic algorithm.
