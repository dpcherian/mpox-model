package sirFSM

import com.bharatsim.engine.Context
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import sirFSM.InfectionStatus.{Infected, Removed, Susceptible}

class SEIROutputSpec(context: Context) extends CSVSpecs {
  override def getHeaders: List[String] =
    List(
      "Day",
      "Susceptible",
      "Infected",
      "Removed"
    )

  override def getRows(): List[List[Any]] = {
    if(context.getCurrentStep%Disease.numberOfTicksInADay==0) {
      val graphProvider = context.graphProvider
      val label = "Person"
      val row = List(
        (context.getCurrentStep*Disease.dt).asInstanceOf[Int],
        graphProvider.fetchCount(label, "infectionState" equ Susceptible),
        graphProvider.fetchCount(label, "infectionState" equ Infected),
        graphProvider.fetchCount(label, "infectionState" equ Removed)
      )
      List(row)
    }
    else{
      List.empty
    }
  }
}
