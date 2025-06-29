package sir

import com.bharatsim.engine.Context
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import sir.InfectionStatus._

class SIROutputSpec(context: Context) extends CSVSpecs {
  override def getHeaders: List[String] =
    List(
      "Day",
      "Susceptible",
      "Exposed",
      "Infected",
      "Recovered"
    )

  override def getRows(): List[List[Any]] = {
    if(context.getCurrentStep % Parameters.numberOfTicksInADay == 0) {
      val graphProvider = context.graphProvider
      val label = "Person"
      val row = List(
        (context.getCurrentStep*Parameters.dt).toInt,
        graphProvider.fetchCount(label, "infectionState" equ Susceptible),
        graphProvider.fetchCount(label, "infectionState" equ Exposed),
        graphProvider.fetchCount(label, "infectionState" equ Infected),
        graphProvider.fetchCount(label, "infectionState" equ Recovered)
      )
      List(row)
    }
    else{
      List.empty
    }
  }
}
