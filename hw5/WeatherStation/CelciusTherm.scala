package WeatherStation

class CelsiusTherm {
   // = degrees Celsius
   def computeTemp(city: String) = 50 * math.random // fake temperature for now
}

trait IThermometer {
   // = avg degrees Farenheit
   def getMeanTemperature(cities: List[String]): Double
}
/*
object WeatherStation extends App {
  val thermometer: IThermometer = new ThermAdapter
  val avgTemp =
      thermometer.getMeanTemperature(List("LA", "SF", "SLC", "Rio"))
  println("avg temp = " + avgTemp)
}
*/