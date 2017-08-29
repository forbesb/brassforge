// Author: Brian Forbes

package brassforge

object Defaults {
  val defaultCoolingRate = 0.003
  val defaultInitialTemperature = 10000.0
  val defaultMinimumTemperature = 1.0

  val defaultPopulationSize = 50;
  val defaultTournamentSize = 5;
  val defaultMutationRate = 0.015
  val defaultGenerations = 100;

  def defaultAcceptance(newCost: Double, oldCost: Double, temperature: Double): Double = {
    if (newCost < oldCost) {
      1.0
    } else {
      math.exp((oldCost - newCost) / temperature)
    }
  }
}