package sir

import com.bharatsim.engine.Context
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.models.{Agent, Network}
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import sir.InfectionStatus._

import scala.collection.mutable.ListBuffer

case class Person(id: Long,
                  age: Int,
                  gender: String,
                  msm: Boolean,
                  infectionState: InfectionStatus,
                  daysInfected: Int,
                  n_contacts: Int = -1,
                  contacts: List[Int] = List.empty,
                  currentLocation: String = "Home",
                  exposedOnDay: Double = -10000.0,
                  infectedOnDay: Double = -10000.0,
                  recoveredOnDay: Double = -10000.0,
                  infectingAgent: Long = -1,
                  agentsInfected: Int = 0,
                  infectedByMSM: Boolean = false,
                  infectedAt: String = "",
                  isVaccinated: Boolean = false,
                  vaccinatedOnDay: Double = -10000.0) extends Agent {

  private val incrementInfectionDuration: Context => Unit = (context: Context) => {
    if (isInfectious && context.getCurrentStep % Parameters.numberOfTicksInADay == 0) {
      updateParam("daysInfected", daysInfected + 1)
    }
  }

  def isSusceptible: Boolean = infectionState == Susceptible

  def isExposed: Boolean = infectionState == Exposed

  def isInfectious: Boolean = infectionState == Infected

  def isInfected: Boolean = (infectionState == Exposed) || (infectionState == Infected)

  def isRecovered: Boolean = infectionState == Recovered


  def decodeNode(classType: String, node: GraphNode): Network = {
    classType match {
      case "House" => node.as[House]
      case "Office" => node.as[Office]
      case "School" => node.as[School]
    }
  }

  def exitSusceptible(context: Context): Unit = {
    if(this.isSusceptible) {
      val agent = this

      val schedule = context.fetchScheduleFor(agent).get

      val currentStep = context.getCurrentStep
      val placeType: String = schedule.getForStep(currentStep)

      val places = agent.getConnections(agent.getRelation(placeType).get).toList

      if (places.nonEmpty) {
        val place = places.head
        val decodedPlace = agent.decodeNode(placeType, place)

        val infectedNumbers = fetchInfectedCount(decodedPlace, placeType, context)

        val allInfected = infectedNumbers(0)
        val vaccinatedInfected = infectedNumbers(1)
        val total = infectedNumbers(2)
        val infectedMSM = infectedNumbers(3)
        val nMSM = infectedNumbers(4)

        val unvaccinatedInfected = allInfected - vaccinatedInfected

        val infectedFraction = (unvaccinatedInfected.toFloat + (1-Parameters.vaccinatedTransmissionReduction)*vaccinatedInfected) / total.toFloat

        var relativeRisk : Double = 1.0

        if (agent.isVaccinated) { // If this (susceptible) person is vaccinated, reduce their probability of getting infected
          relativeRisk = 1.0 - Parameters.vaccinatedBetaReduction
        }

        val infectionProbability = relativeRisk * decodedPlace.getContactProbability() * Parameters.beta * Parameters.dt

        val shouldBeInfected = biasedCoinToss(infectionProbability * infectedFraction)

        if (shouldBeInfected) {
          updateParam("infectionState", Exposed)
          updateParam("exposedOnDay", context.getCurrentStep * Parameters.dt)
          updateParam("infectedByMSM", false)
          updateParam("infectedAt", placeType)

          val infecting_agent = fetchInfectingAgent(decodedPlace, placeType, context)

//          val infecting_agent_agents_infected = context.perTickCache.getOrUpdate((infecting_agent.id, "agents infected this tick"), () => 0 ).asInstanceOf[Int]

          infecting_agent.updateParam("agentsInfected", infecting_agent.agentsInfected+1)

//          val new_infecting_agent_agents_infected = context.perTickCache.getOrUpdate((infecting_agent.id, "agents infected this tick"), () => infecting_agent_agents_infected + 1 ).asInstanceOf[Int]

          updateParam("infectingAgent", infecting_agent.id)
          // TODO: Implement keeping track of infecting agents even if person _isn't_ infected in MSM encounter.
          // Right now, an agent can infect someone multiple times in a tick but only have it recorded as "once". Something needs to be fixed.
        }

      }
    }
  }

  private def fetchInfectingAgent(decodedPlace: Network, placeType: String, context: Context): Person = {
    val cache = context.perTickCache

    val tuple = ("InfectingAgentsAt"+placeType, decodedPlace.internalId)
    val infectedHereList = cache.getOrUpdate(tuple, () => fetchInfectingAgentsFromStore(decodedPlace, placeType)).asInstanceOf[List[Person]]

    val infectingAgent = infectedHereList(Main.splittableRandom.nextInt(infectedHereList.size))

    infectingAgent
  }

  private def fetchInfectingAgentsFromStore(decodedPlace: Network, placeType: String): List[Person] = {
    val peopleHere = decodedPlace.getConnections(decodedPlace.getRelation[Person]().get)
    val infectedHere = new ListBuffer[Person]()

    peopleHere.foreach(node => {
      val person = node.as[Person]
      if (person.isInfectious && person.currentLocation==placeType) {
        infectedHere += person
      }
    })
    infectedHere.toList
  }

  private def fetchInfectedCount(decodedPlace: Network, placeType: String, context: Context): List[Int] = {
    val cache = context.perTickCache

    val tuple = (placeType, decodedPlace.internalId)
    cache.getOrUpdate(tuple, () => fetchFromStore(decodedPlace, placeType)).asInstanceOf[List[Int]]
  }

  private def fetchFromStore(decodedPlace: Network, placeType: String): List[Int] = {
    val infectedPattern = ("infectionState" equ Infected)

    val total = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, "currentLocation" equ placeType)
    if (total == 0.0) {
      return List(0, 0, 0, 0, 0)
    }

    val nMSM = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("currentLocation" equ placeType) and ("MSM" equ true))
    val infectedMSM = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("currentLocation" equ placeType) and ("MSM" equ true) and infectedPattern)
    val allInfected = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("currentLocation" equ placeType) and infectedPattern)
    val allVaccinatedInfected = decodedPlace.getConnectionCount(decodedPlace.getRelation[Person]().get, ("currentLocation" equ placeType) and infectedPattern and ("isVaccinated" equ true))
    //    println("Fetched for "+ placeType + " location " + decodedPlace+ " with "+total + " people")

    List(allInfected, allVaccinatedInfected, total, infectedMSM, nMSM)
  }

  def exitExposed(context: Context): Unit = {
    if (this.isExposed) {
      if (biasedCoinToss(Parameters.lambda_E * Parameters.dt)) {
        updateParam("infectionState", Infected)
        updateParam("infectedOnDay", context.getCurrentStep*Parameters.dt)
      }
    }
  }
  def exitInfected(context: Context): Unit = {
    if(this.isInfectious){
      if(biasedCoinToss(Parameters.lambda_I * Parameters.dt)){
        updateParam("infectionState", Recovered)
        updateParam("recoveredOnDay", context.getCurrentStep*Parameters.dt)
      }
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
      if(biasedCoinToss(Parameters.meeting_rate*Parameters.dt)){
        val this_guy = this

        if(contacts.size > 0) {
          val that_guy_id = contacts(Main.splittableRandom.nextInt(contacts.size))
          val that_guy = context.graphProvider.fetchNodes("Person", "id" equ that_guy_id).toList.head.as[Person]

          var relativeRiskInfection : Double = 1.0
          var relativeRiskTransmission : Double = 1.0

          if(this_guy.isInfectious && that_guy.isSusceptible){
            // If this guy is infected and their contact is _not_, infect the contact

            if (that_guy.isVaccinated) { // If the susceptible person is vaccinated, reduce their probability of getting infected
              relativeRiskInfection = 1.0 - Parameters.vaccinatedMuReduction
            }

            if (this_guy.isVaccinated) { // If the infected person is vaccinated, reduce their probability of transmitting the infection
              relativeRiskTransmission = 1.0 - Parameters.vaccinatedTransmissionReduction
            }

            if(biasedCoinToss(relativeRiskInfection * relativeRiskTransmission * Parameters.msmInfectionProb)){
              that_guy.updateParam("infectionState", Exposed)
              that_guy.updateParam("exposedOnDay", context.getCurrentStep * Parameters.dt)
              that_guy.updateParam("infectedByMSM", true)
              that_guy.updateParam("infectedAt", "Encounter")
              that_guy.updateParam("infectingAgent", this_guy.id)
              this_guy.updateParam("agentsInfected", this_guy.agentsInfected + 1)
            }
          }
          else if(that_guy.isInfectious && this_guy.isSusceptible){
            // If this guy is _not_ infected and their contact is infected, infect this person

            if (this_guy.isVaccinated) { // Again, if the susceptible person is vaccinated, reduce their probability of getting infected
              relativeRiskInfection = 1.0 - Parameters.vaccinatedMuReduction
            }

            if(that_guy.isVaccinated) { // If, on the other hand, the infected person is vaccinated, reduce their probability of transmitting the disease
              relativeRiskTransmission = 1.0 - Parameters.vaccinatedTransmissionReduction
            }

            if(biasedCoinToss(relativeRiskInfection * relativeRiskTransmission * Parameters.msmInfectionProb)){
              this_guy.updateParam("infectionState", Exposed)
              this_guy.updateParam("exposedOnDay", context.getCurrentStep * Parameters.dt)
              this_guy.updateParam("infectedByMSM", true)
              this_guy.updateParam("infectedAt", "Encounter")
              this_guy.updateParam("infectingAgent", that_guy.id)
              that_guy.updateParam("agentsInfected", that_guy.agentsInfected + 1)
            }
          }

        }
      }
    }

    addBehaviour(meetMSM)
  }

  addBehaviour(incrementInfectionDuration)
  addBehaviour(updateCurrentLocation)

  addBehaviour(exitSusceptible)
  addBehaviour(exitExposed)
  addBehaviour(exitInfected)

  addRelation[House]("STAYS_AT")
  addRelation[Office]("WORKS_AT")
  addRelation[School]("STUDIES_AT")
}
