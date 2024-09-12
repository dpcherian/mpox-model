package sirFSM

import com.bharatsim.engine.ContextBuilder._
import com.bharatsim.engine._
import com.bharatsim.engine.actions.StopSimulation
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.dsl.SyntaxHelpers._
import com.bharatsim.engine.execution.Simulation
import com.bharatsim.engine.graph.ingestion.{GraphData, Relation}
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.listeners.{CsvOutputGenerator, SimulationListenerRegistry}
import com.bharatsim.engine.models.Agent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import sirFSM.InfectionStatus._
import sirFSM.diseaseStates.{InfectedState, RecoveredState, SusceptibleState}
import com.typesafe.scalalogging.LazyLogging

import java.util.Date


object Main extends LazyLogging {
  private val initialInfectedFraction = 0.0f/100

  private val myTick: ScheduleUnit = new ScheduleUnit(1)
  private val myDay: ScheduleUnit = new ScheduleUnit(myTick * 2)

  final val splittableRandom: RandomNumberGenerator = RandomNumberGenerator()

  def main(args: Array[String]): Unit = {
    val input = args(0)
    var beforeCount = 0
    val simulation = Simulation()

    simulation.ingestData(implicit context => {
      ingestCSVData(input+".csv", csvDataExtractor)
      logger.debug("Ingestion done")
    })

    simulation.defineSimulation(implicit context => {
      create12HourSchedules()

      registerAction(
        StopSimulation,
        (context: Context) => {
          context.getCurrentStep == Disease.SIMTICKS
        }
      )

      beforeCount = getInfectedCount(context)

      registerAgent[Person]

      registerState[SusceptibleState]
      registerState[InfectedState]
      registerState[RecoveredState]

      val currentTime = new Date().getTime

      SimulationListenerRegistry.register(
        new CsvOutputGenerator( "output_"+ currentTime + ".csv", new SEIROutputSpec(context))
      )
    })

    simulation.onCompleteSimulation { implicit context =>
      printStats(beforeCount)
      teardown()
    }

    val startTime = System.currentTimeMillis()
    simulation.run()
    val endTime = System.currentTimeMillis()
    logger.info("Total time: {} s", (endTime - startTime) / 1000)
  }

  private def create12HourSchedules()(implicit context: Context): Unit = {
    val employeeSchedule = (myDay, myTick)
      .add[House](0, 0)
      .add[Office](1, 1)

    val studentSchedule = (myDay, myTick)
      .add[House](0, 0)
      .add[School](1, 1)

    val quarantinedSchedule = (myDay, myTick)
      .add[House](0, 1)

    registerSchedules(
//      (quarantinedSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].isInfected, 1),
      (employeeSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age > Disease.studentAge, 2),
      (studentSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age <= Disease.studentAge, 3)
    )
  }

  private def csvDataExtractor(map: Map[String, String])(implicit context: Context): GraphData = {

    val citizenId = map("AgentID").toLong
    val age : Int = map("Age").toInt
    val gender : String = map("Gender")
    val msm : Boolean = map("MSM").toBoolean
    var initialInfectionState = if (biasedCoinToss(initialInfectedFraction)) "Infected" else "Susceptible"
    initialInfectionState = if(citizenId==2185) "Infected" else "Susceptible"

    val homeId = map("HHID").toLong
    val schoolId = map("SchoolID").toLong
    val officeId = map("WorkplaceID").toLong

    val n_contacts = map("NumberOfContacts").toInt

    var contacts = List.empty[Int]

    if(msm){
      val contactList = map("Contacts") // Remove brackets around list

      if(contactList.size > 0) {
        contacts = contactList.split(',')
          .toList
          .flatMap { case "" => 0 :: Nil
          case a => a.toInt :: Nil
          }
      }
    }

    val citizen: Person = Person(id=citizenId, age=age, gender=gender, msm=msm,
                                infectionState=InfectionStatus.withName(initialInfectionState),
                                daysInfected = 0, n_contacts = n_contacts, contacts = contacts
                              )

    if(initialInfectionState == "Susceptible"){
      citizen.setInitialState(SusceptibleState())
    }
    else if (initialInfectionState == "Infected"){
      citizen.setInitialState(InfectedState())
    }
    else{
      citizen.setInitialState(RecoveredState())
    }

    val home = House(homeId)
    val staysAt = Relation[Person, House](citizenId, "STAYS_AT", homeId)
    val memberOf = Relation[House, Person](homeId, "HOUSES", citizenId)

    val graphData = GraphData()
    graphData.addNode(citizenId, citizen)
    graphData.addNode(homeId, home)
    graphData.addRelations(staysAt, memberOf)

    if (age > Disease.studentAge) {
      val office = Office(officeId)
      val worksAt = Relation[Person, Office](citizenId, "WORKS_AT", officeId)
      val employerOf = Relation[Office, Person](officeId, "EMPLOYER_OF", citizenId)

      graphData.addNode(officeId, office)
      graphData.addRelations(worksAt, employerOf)
    } else {
      val school = School(schoolId)
      val studiesAt = Relation[Person, School](citizenId, "STUDIES_AT", schoolId)
      val studentOf = Relation[School, Person](schoolId, "STUDENT_OF", citizenId)

      graphData.addNode(schoolId, school)
      graphData.addRelations(studiesAt, studentOf)
    }

    graphData
  }

  private def printStats(beforeCount: Int)(implicit context: Context): Unit = {
    val afterCountSusceptible = getSusceptibleCount(context)
    val afterCountInfected = getInfectedCount(context)
    val afterCountRecovered = getRemovedCount(context)

    logger.info("Infected before: {}", beforeCount)
    logger.info("Infected after: {}", afterCountInfected)
    logger.info("Recovered: {}", afterCountRecovered)
    logger.info("Susceptible: {}", afterCountSusceptible)
  }

  private def getSusceptibleCount(context: Context) = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Susceptible)
  }

  private def getInfectedCount(context: Context): Int = {
    context.graphProvider.fetchCount("Person", ("infectionState" equ Infected))
  }

  private def getRemovedCount(context: Context) = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Removed)
  }
}
