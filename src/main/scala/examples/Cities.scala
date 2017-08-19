// Author: Brian Forbes

package examples

import brassforge.SimulatedAnnealing

import scala.util.Random

case class City(x: Int, y: Int){
  def distanceTo(city: City): Double = math.sqrt( (x - city.x) * (x-city.x) + (y - city.y) * (y - city.y))
}

class Tour(cities: Seq[City]) {
  def distance(): Double =
    cities.foldLeft((0.0, cities(0)))(combine)._1

  def permute(): Tour =
    new Tour(swap(cities,
      cities(Random.nextInt(cities.length)),
      cities(Random.nextInt(cities.length))))

  override def toString: String = cities.foldRight("")((city, string) => "|%d, %d|%s".format(city.x, city.y, string))

  private def combine(v: (Double, City), c: City) = (v._1 + c.distanceTo(v._2), c)
  private def swap[T](seq: Seq[T], first: T, second: T): Seq[T] = seq match {
    case Nil => Nil
    case `first`::rest => second::rest
    case `second`::rest => first::rest
    case x::rest => x::rest
  }
}


object Cities {
  val mapLength = 200
  val mapWidth = 200

  val numCities = 20

  def citiesExample(): Unit = {
    val simulatedAnnealing = new SimulatedAnnealing[Tour](_.permute(),  _.distance())

    println(simulatedAnnealing.anneal(new Tour(generateCities(numCities))))
  }

  def generateCities(n: Int): List[City] = n match {
    case 0 => Nil
    case n => City(Random.nextInt(mapLength), Random.nextInt(mapWidth))::generateCities(n-1)
  }
}
