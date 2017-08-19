// Author: Brian Forbes

package brassforge

import scala.util.Random

object Defaults {
  val defaultCoolingRate = 0.03
  val defaultInitialTemperature = 10000.0
  val defaultMinimumTemperature = 1.0
  def defaultAcceptance(newCost: Double, oldCost: Double, temperature: Double): Double = {
    if (newCost > oldCost) {
      1.0
    } else {
      math.exp((oldCost - newCost) / temperature)
    }
  }
}

class SimulatedAnnealing[Solution](neighbor: Solution=>Solution,
                                   cost: Solution => Double,
                                   coolingRate: Double = Defaults.defaultCoolingRate,
                                   initialTemperature: Double = Defaults.defaultInitialTemperature,
                                   minimumTemperature: Double = Defaults.defaultMinimumTemperature,
                                   acceptance: (Double, Double, Double) => Double = Defaults.defaultAcceptance) {

  def anneal(solution: Solution): (Solution, Double) = {
    annealInternal(solution, cost(solution), initialTemperature)
  }

  private def annealInternal(solution:Solution, currentCost: Double, temperature: Double): (Solution, Double) = {
    if (temperature > minimumTemperature) {
      val newSolution = neighbor(solution)
      val newCost = cost(newSolution)

      if (acceptance(newCost, currentCost, temperature) > Random.nextDouble()) {
        annealInternal(newSolution, newCost, temperature * (1-coolingRate))
      } else {
        annealInternal(solution, currentCost, temperature * (1-coolingRate))
      }

    } else {
      (solution, currentCost)
    }
  }
}
