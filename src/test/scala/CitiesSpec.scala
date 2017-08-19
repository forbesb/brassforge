import examples.{City, Tour}
import org.scalatest.FlatSpec
// Author: Brian Forbes

class CitiesSpec extends FlatSpec{
  "A City" should "properly calculate distance to another city" in {
    val toronto = City(0,0)
    val ottowa = City(3, 4)

    assert(toronto.distanceTo(ottowa) == 5)
  }

  "A Tour" should "properly calculate total distance" in {
    val tour = new Tour(City(0,0)::City(0,1)::City(0,2)::City(0,3)::Nil)

    assert(tour.distance() == 3.0)
  }
}
