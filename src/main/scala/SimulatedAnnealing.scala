/**
  * Created by brian on 19/08/17.
  */


class SimulatedAnnealing[Solution](cooling_rate: Double,
                                   initialTemperature: Double,
                                   minimumTemperature: Double,
                                   neighbor: Solution=>Solution,
                                   cost: Solution => Integer) {

  def anneal(solution: Solution): (Solution, Integer) = {
    annealInternal(solution, cost(solution), initialTemperature)
  }

  private def annealInternal(solution:Solution, current_cost: Integer, temperature: Double): (Solution, Integer) = {
    

    (solution, current_cost)
  }
}
