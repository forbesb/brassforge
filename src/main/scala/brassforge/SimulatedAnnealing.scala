// Author: Brian Forbes

package brassforge

import scala.util.Random

class SimulatedAnnealing[Solution](neighbor: Solution=>Solution,
                                   cost: Solution => Double,
                                   coolingRate: Double = Defaults.defaultCoolingRate,
                                   initialTemperature: Double = Defaults.defaultInitialTemperature,
                                   minimumTemperature: Double = Defaults.defaultMinimumTemperature,
                                   acceptance: (Double, Double, Double) => Double = Defaults.defaultAcceptance) {

  def anneal(solution: Solution): (Solution, Double) = {
    val initialCost = cost(solution)
    annealInternal(solution, initialCost, initialTemperature, (solution, initialCost))
  }

  private def annealInternal(solution:Solution, currentCost: Double, temperature: Double, best: (Solution, Double)): (Solution, Double) = {
    if (temperature > minimumTemperature) {
      val newSolution = neighbor(solution)
      val newCost = cost(newSolution)

      if (acceptance(newCost, currentCost, temperature) > Random.nextDouble()) {
        annealInternal(newSolution, newCost, temperature * (1-coolingRate), if (newCost < best._2 ) (newSolution, newCost) else best )
      } else {
        annealInternal(solution, currentCost, temperature * (1-coolingRate), best)
      }
    } else {
      if (currentCost < best._2) (solution, currentCost) else best
    }
  }
}
