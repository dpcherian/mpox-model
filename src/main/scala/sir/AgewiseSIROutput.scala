package sir

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import sir.InfectionStatus._

import scala.collection.mutable

class AgewiseSIROutput(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] =
    List(
      "Day",
      "Susceptible (0-18)",
      "Exposed (0-18)",
      "Infected (0-18)",
      "Recovered (0-18)",

      "Susceptible (18-45)",
      "Exposed (18-45)",
      "Infected (18-45)",
      "Recovered (18-45)",

      "Susceptible (45-60)",
      "Exposed (45-60)",
      "Infected (45-60)",
      "Recovered (45-60)",

      "Susceptible (60+)",
      "Exposed (60+)",
      "Infected (60+)",
      "Recovered (60+)"
    )

  override def getRows(): List[List[Any]] = {
    if (context.getCurrentStep % Parameters.numberOfTicksInADay == 0) {
      val graphProvider = context.graphProvider
      val label = "Person"
      val countMap = mutable.HashMap.empty[String, Int]
      val nodes = graphProvider.fetchNodes(label)
      nodes.foreach(node => {
        val infectedState = node.getParams.apply("infectionState").toString
        val ageCategory = mapAgeToCat(node.getParams.apply("age").toString.toInt)
        val existingCount = countMap.getOrElse(infectedState + ageCategory, 0)
        countMap.put(infectedState + ageCategory, existingCount + 1)

      })

      val row = List(
        context.getCurrentStep * Parameters.dt,
        countMap.getOrElse(Susceptible.toString + "0-18", 0),
        countMap.getOrElse(Exposed.toString + "0-18", 0),
        countMap.getOrElse(Infected.toString + "0-18", 0),
        countMap.getOrElse(Recovered.toString + "0-18", 0),

        countMap.getOrElse(Susceptible.toString + "18-45", 0),
        countMap.getOrElse(Exposed.toString + "18-45", 0),
        countMap.getOrElse(Infected.toString + "18-45", 0),
        countMap.getOrElse(Recovered.toString + "18-45", 0),

        countMap.getOrElse(Susceptible.toString + "45-60", 0),
        countMap.getOrElse(Exposed.toString + "45-60", 0),
        countMap.getOrElse(Infected.toString + "45-60", 0),
        countMap.getOrElse(Recovered.toString + "45-60", 0),

        countMap.getOrElse(Susceptible.toString + "60+", 0),
        countMap.getOrElse(Exposed.toString + "60+", 0),
        countMap.getOrElse(Infected.toString + "60+", 0),
        countMap.getOrElse(Recovered.toString + "60+", 0),
      )
      List(row)
    }
    else {
      List.empty
    }

  }

  def mapAgeToCat(age: Int): String = {
    if (0 <= age && age < 18) {"0-18"}
    else if (18 <= age && age < 45) {"18-45"}
    else if (45 <= age && age < 60) {"45-60"}
    else {"60+"}
  }

}
