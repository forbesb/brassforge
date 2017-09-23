// Author: Brian Forbes

package examples

import brassforge.{GeneticAlgorithm, SimulatedAnnealing}

import scala.util.Random

case class City(x: Int, y: Int){
  def distanceTo(city: City): Double = math.sqrt( (x - city.x) * (x-city.x) + (y - city.y) * (y - city.y))
}

class Tour(cities: List[City]) {
  def distance(): Double =
    cities.foldLeft((0.0, cities(0)))(combine)._1

  // Simulated Annealing
  def permute(): Tour =
    new Tour(swap(cities,
      cities(Random.nextInt(cities.length)),
      cities(Random.nextInt(cities.length))))

  // Genetic Algorithm

  def generateSolutionPool(size: Int): Set[Tour] =
    if (size == 0) {
      Set.empty
    } else {
      generateSolutionPool(size - 1) + new Tour(Random.shuffle(cities))
    }

  def crossover(partner: Tour): Tour = {
    var lst = Nil
    val crossoverPoint = Random.nextInt(cities.length);
    for (i <- 0 to cities.length - 1) {
    }

    partner
  }

  def mutate(mutationRate: Double, startPosition: Int = 0): Tour = {
    if (startPosition == cities.size) {
      this
    } else {
      if (Random.nextDouble() < mutationRate) {
        new Tour(swap(cities, cities(startPosition), cities(Random.nextInt(cities.size)))).mutate(mutationRate, startPosition + 1)
      } else {
        mutate(mutationRate, startPosition + 1)
      }
    }
  }


  override def toString: String = cities.foldRight("")((city, string) => "|%d, %d|%s".format(city.x, city.y, string))

  private def combine(v: (Double, City), c: City) = (v._1 + c.distanceTo(v._2), c)

  private def swap[T](seq: List[T], first: T, second: T): List[T] = seq match {
    case Nil => Nil
    case `first`::rest => second::swap(rest, first, second)
    case `second`::rest => first::swap(rest, first, second)
    case x::rest => x::swap(rest, first, second)
  }
}


object Cities {
  val mapLength = 200
  val mapWidth = 200

  val numCities = 20

  def citiesExample(): Unit = {
    val exampleCities =
      City(60,200)::City(180,200)::City(80,180)::City(140,180)::City(20,160)::City(100,160)::City(200,160)::
        City(140,140)::City(40,120)::City(100,120)::City(180,100)::City(60,80)::City(120,80)::City(180,60)::
        City(20,40)::City(100,40)::City(200,40)::City(20,20)::City(60,20)::City(160,20)::Nil

    val simulatedAnnealing = new SimulatedAnnealing[Tour](_.permute(),  _.distance())
    val geneticAlgorithm1 = new GeneticAlgorithm[Tour](new Tour(generateCities(numCities)).generateSolutionPool(_),
    _.distance(),
    _.crossover(_),
    _.mutate(_))
    val geneticAlgorithm2 = new GeneticAlgorithm[Tour](new Tour(exampleCities).generateSolutionPool(_), _.distance(), _.crossover(_), _.mutate(_))

    println("Simulated Annealing:")
    println(simulatedAnnealing.anneal(new Tour(generateCities(numCities))))
    println(simulatedAnnealing.anneal(new Tour(exampleCities)))

    println()

    println("Genetic Algorithm")
    print(geneticAlgorithm1.solve())
    println(geneticAlgorithm2.solve())
  }

  def generateCities(n: Int): List[City] = n match {
    case 0 => Nil
    case n => City(Random.nextInt(mapLength), Random.nextInt(mapWidth))::generateCities(n-1)
  }
}
