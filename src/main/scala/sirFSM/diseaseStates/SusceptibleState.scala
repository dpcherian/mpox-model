package sirFSM.diseaseStates

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.fsm.State
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.models.{Network, StatefulAgent}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import sirFSM.InfectionStatus._
import sirFSM.{Disease, Person}

case class SusceptibleState() extends State {

  override def enterAction(context: Context, agent: StatefulAgent): Unit = {
    agent.updateParam("infectionState", Susceptible)
  }

  override def perTickAction(context: Context, agent: StatefulAgent): Unit = {

  }


  def exitSusceptible(context: Context, agent: StatefulAgent): Boolean = {
    val infectionProbability = Disease.beta * Disease.dt

    val schedule = context.fetchScheduleFor(agent).get

    val currentStep = context.getCurrentStep
    val placeType: String = schedule.getForStep(currentStep)

    val places = agent.getConnections(agent.getRelation(placeType).get).toList

    if (places.nonEmpty) {
      val place = places.head
      val decodedPlace = agent.asInstanceOf[Person].decodeNode(placeType, place)
      val all = decodedPlace.getConnections(decodedPlace.getRelation[Person]().get)
      if(placeType=="School"){
        println(placeType+ " "+ decodedPlace +" "+all.size)
      }

      val infectedNumbers = fetchInfectedCount(decodedPlace, placeType, context)

      val allInfected = infectedNumbers(0)
      val total = infectedNumbers(1)
      val infectedMSM = infectedNumbers(2)
      val nMSM = infectedNumbers(3)

      val infectedFraction = allInfected.toFloat/total.toFloat

      val shouldBeInfected = biasedCoinToss(infectionProbability * infectedFraction)

      return shouldBeInfected
    }

    false
  }


  private def fetchInfectedCount(decodedPlace: Network, placeType: String, context: Context): List[Int] = {
    val cache = context.perTickCache

    val tuple = (placeType, decodedPlace.internalId)
    cache.getOrUpdate(tuple, () => fetchFromStore(decodedPlace, placeType)).asInstanceOf[List[Int]]
  }

  private def fetchFromStore(decodedPlace: Network, placeType: String): List[Int] = {
    val infectedPattern = ("infectionState" equ Infected)

    val total = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, "currentLocation" equ placeType)
    val nMSM = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("currentLocation" equ placeType) and ("MSM" equ true))
    val infectedMSM = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("currentLocation" equ placeType) and ("MSM" equ true) and infectedPattern)
    val allInfected = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("currentLocation" equ placeType) and infectedPattern)
//    println("Fetched for "+ placeType + " location " + decodedPlace+ " with "+total + " people")
    if (total == 0.0) {
      return List(0,0,0,0)
    }

    List(allInfected, total, infectedMSM, nMSM)
  }


  addTransition(
    when = exitSusceptible,
      to = context => InfectedState()
  )


}
