package sir

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.CSVSpecs
import com.bharatsim.engine.models.Node
import sir.Person

import scala.collection.mutable.ListBuffer

class AgentInfoOutput(context: Context) extends CSVSpecs {

  override def getHeaders: List[String] = List("AgentID", "MSM", "NumberOfContacts", "ExposedOn", "InfectedOn", "RecoveredOn", "DaysInfected", "InfectingAgent", "InfectedByMSM", "InfectedAt", "NumberOfInfections", "InfectionState", "VaccinatedOn")

  override def getRows(): List[List[Any]] = {
    val rows = ListBuffer.empty[List[String]]

    val graphProvider = context.graphProvider
    val label = "Person"
    val nodes = graphProvider.fetchNodes(label)

    nodes.foreach(node => {
      val person = node.as[Person]
      // TODO: Susceptible people don't show up, so this file cannot be used to compute aggregate numbers of vaccinated, for example
      if(!person.isSusceptible) {
        rows.addOne(List(
          person.id.toString,
          person.msm.toString,
          person.n_contacts.toString,
          person.exposedOnDay.toString,
          person.infectedOnDay.toString,
          person.recoveredOnDay.toString,
          person.daysInfected.toString,
          person.infectingAgent.toString,
          person.infectedByMSM.toString,
          person.infectedAt.toString,
          person.agentsInfected.toString,
          person.infectionState.toString,
          person.vaccinatedOnDay.toString))
      }
    })
    rows.toList

  }

}

