// Author: Brian Forbes

package examples

import main.SimulatedAnnealing

case class City(x: Int, y: Int){
  def distanceTo(city: City): Double = math.sqrt( (x - city.x) * (x-city.x) + (y - city.y) * (y - city.y))
}

class Tour(cities: Seq[City]) {
  def distance(): Double = {
    cities.foldLeft((0.0, cities(0)))(combine)._1
  }

  

  private def combine(v: (Double, City), c: City) = (v._1 + c.distanceTo(v._2), c)
}


object Cities {
  def citiesExample(): Unit = {
    val simulatedAnnealing = new SimulatedAnnealing[Tour](x => x,  _.distance())


  }


}
