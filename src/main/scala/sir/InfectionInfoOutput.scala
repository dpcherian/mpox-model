package sir

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import sir.InfectionStatus.Infected

import scala.collection.mutable

class InfectionInfoOutput(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] =
    List(
      "Day",
      "InfectedByMSM",
      "InfectedByOther",
      "InfectedAtHome",
      "InfectedAtWork",
      "InfectedAtSchool",
      "InfectedAtEncounter"
    )

  override def getRows(): List[List[Any]] = {
    if (context.getCurrentStep % Parameters.numberOfTicksInADay == 0) {
      val graphProvider = context.graphProvider
      val label = "Person"
      val infectedByMap = mutable.HashMap.empty[String, Int]
      val infectedAtMap = mutable.HashMap.empty[String, Int]
      val nodes = graphProvider.fetchNodes(label)
      nodes.foreach(node => {
        if(node.getParams.apply("infectionState")==Infected.toString){
          val infectedBy = if (node.getParams.apply("infectedByMSM").toString.toBoolean) "MSM" else "Other"
          val existingInfectedByCount = infectedByMap.getOrElse(infectedBy, 0)
          infectedByMap.put(infectedBy, existingInfectedByCount + 1)
          val infectedAt = node.getParams.apply("infectedAt").toString
          val existingInfectedAtCount = infectedAtMap.getOrElse(infectedAt, 0)
          infectedAtMap.put(infectedAt, existingInfectedAtCount + 1)
        }
      })

      val row = List(
        context.getCurrentStep * Parameters.dt,
        infectedByMap.getOrElse("MSM", 0),
        infectedByMap.getOrElse("Other", 0),
        infectedAtMap.getOrElse("House", 0),
        infectedAtMap.getOrElse("Office", 0),
        infectedAtMap.getOrElse("School", 0),
        infectedAtMap.getOrElse("Encounter", 0)
      )

      return List(row)

    }
    List.empty
  }

}