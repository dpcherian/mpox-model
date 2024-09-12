package sirFSM

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.models.{Agent, Network, Node, StatefulAgent}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import sirFSM.InfectionStatus._

case class Person(id: Long,
                  age: Int,
                  gender: String,
                  msm: Boolean,
                  infectionState: InfectionStatus,
                  daysInfected: Int,
                  n_contacts: Int = -1,
                  contacts: List[Int] = List.empty,
                  currentLocation: String = "Home",
                  infectedOnDay: Double = 10000.0,
                  infectedByMSM: Boolean = false) extends StatefulAgent {

  private val incrementInfectionDuration: Context => Unit = (context: Context) => {
    if (isInfected && context.getCurrentStep % Disease.numberOfTicksInADay == 0) {
      updateParam("daysInfected", daysInfected + 1)
    }
  }

  def isSusceptible: Boolean = infectionState == Susceptible

  def isInfected: Boolean = infectionState == Infected

  def isRecovered: Boolean = infectionState == Removed


  def decodeNode(classType: String, node: GraphNode): Network = {
    classType match {
      case "House" => node.as[House]
      case "Office" => node.as[Office]
      case "School" => node.as[School]
    }
  }

  private def updateCurrentLocation(context: Context): Unit = {
    val currentPlaceOption = getCurrentPlace(context)
    currentPlaceOption match {
      case Some(x) => {
        if (this.currentLocation != x) {
          updateParam("currentLocation", x)
        }
      }
      case _ =>
    }
  }

  private def getCurrentPlace(context: Context): Option[String] = {
    val schedule = context.fetchScheduleFor(this).get
    val currentStep = context.getCurrentStep + 1
    val placeType: String = schedule.getForStep(currentStep)

    Some(placeType)
  }

  if(this.msm){
    val meetMSM: Context => Unit = (context: Context) => {
      if(biasedCoinToss(Disease.meeting_rate*Disease.dt)){
        val this_guy = this

        if(contacts.size > 0) {
          val that_guy_id = contacts(Main.splittableRandom.nextInt(contacts.size))
          val that_guy = context.graphProvider.fetchNodes("Person", "id" equ that_guy_id).toList.head.as[Person]

          if(this_guy.isInfected && that_guy.isSusceptible){
            // If this guy is infected and their contact is _not_, infect the contact
            if(biasedCoinToss(Disease.msmInfectionProb)){
//              that_guy.updateParam("infectionState", Infected)
              that_guy.forceUpdateActiveState()
              that_guy.updateParam("infectedOnDay", context.getCurrentStep * Disease.dt)
              that_guy.updateParam("infectedByMSM", true)

              that_guy
            }
          }
          else if(that_guy.isInfected && this_guy.isSusceptible){
            // If this guy is _not_ infected and their contact is infected, infect this person
            if(biasedCoinToss(Disease.msmInfectionProb)){
              this_guy.updateParam("infectionState", Infected)
              this_guy.updateParam("infectedOnDay", context.getCurrentStep * Disease.dt)
              this_guy.updateParam("infectedByMSM", true)
            }
          }

        }
      }
    }

    addBehaviour(meetMSM)
  }

  addBehaviour(incrementInfectionDuration)
  addBehaviour(updateCurrentLocation)

  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  addRelation[School]("STUDIES_AT")
}
