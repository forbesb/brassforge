// Author: Brian Forbes
package brassforge

import scala.util.Random

class GeneticAlgorithm[Solution](initialize: Int => Set[Solution],
                                 fitness: Solution => Double,
                                 crossover: (Solution, Solution) => Solution,
                                 mutate: (Solution, Double) => Solution,
                                 tournamentSize: Int,
                                 populationSize: Int = Defaults.defaultPopulationSize,
                                 maxGenerations: Int = Defaults.defaultGenerations,
                                 mutationRate: Double = Defaults.defaultMutationRate) {

  def solve(solutions: Set[Solution] = initialize(populationSize), generation: Int = 0): Solution =
    if (generation == maxGenerations) {
      fittest(solutions)
    } else {
      solve(evolve(solutions), generation + 1)
    }

  private def evolve(population: Set[Solution]): Set[Solution] =
    buildSolutionSet(population, populationSize)

  private def buildSolutionSet(population: Set[Solution], size: Int): Set[Solution] =
    if (size > 0) {
     buildSolutionSet(population, size - 1) + mutate(crossover(tournamentSelect(population), tournamentSelect(population)), mutationRate)
    } else {
      Set.empty
    }

  private def tournamentSelect(population: Set[Solution]): Solution =
    tournamentSelectRecursive(population, population.toVector(Random.nextInt(populationSize)), 1)

  private def tournamentSelectRecursive(population: Set[Solution], best: Solution, depth: Int): Solution =
    if (depth == tournamentSize) {
      best
    } else {
      val challenger = population.toVector(Random.nextInt(populationSize))
      val update = fitness(challenger) > fitness(best)

      tournamentSelectRecursive(
        if (update) population - challenger else population,
        if (update) challenger else best,
        depth + 1)
    }

  private def fittest(solutions: Set[Solution]): Solution =
    solutions.maxBy(fitness(_))

}
