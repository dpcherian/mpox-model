package sir

import com.bharatsim.engine.ContextBuilder._
import com.bharatsim.engine._
import com.bharatsim.engine.actions.StopSimulation
import com.bharatsim.engine.basicConversions.decoders.DefaultDecoders._
import com.bharatsim.engine.basicConversions.encoders.DefaultEncoders._
import com.bharatsim.engine.dsl.SyntaxHelpers._
import com.bharatsim.engine.execution.Simulation
import com.bharatsim.engine.graph.GraphNode
import com.bharatsim.engine.graph.ingestion.{GraphData, Relation}
import com.bharatsim.engine.graph.patternMatcher.MatchCondition._
import com.bharatsim.engine.intervention.{Intervention, SingleInvocationIntervention}
import com.bharatsim.engine.listeners.{CsvOutputGenerator, SimulationListenerRegistry}
import com.bharatsim.engine.models.Agent
import com.bharatsim.engine.utils.Probability.biasedCoinToss
import sir.InfectionStatus._
import com.typesafe.scalalogging.LazyLogging

import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.util.Random


object Main extends LazyLogging {
  var currentTime: Long = 0L
  private val myTick: ScheduleUnit = new ScheduleUnit(1)
  private val myDay: ScheduleUnit = new ScheduleUnit(myTick * 2)

  final val splittableRandom: RandomNumberGenerator = RandomNumberGenerator()

  var vaccinationStartedOn: Double = 10_000

  var exposedMSMs: List[Int] = List();
  var exposedNonMSMs: List[Int] = List();
  val ncontacts_listbuffer = ListBuffer[Int]()

  def parseArgs(args: Array[String]): Unit = {

    for (arg <- args) {
      val a = arg.split("=")

      if (a.length != 2) {
        throw new Exception(s"Unsupported syntax for argument: \"" + arg + "\". Flag syntax is `name=value`, without spaces.")
      }
      else {
        val key = a(0).toUpperCase
        val value = a(1)
        ""
        key match {
          case "INPUT" => {
            Parameters.inputPath = value;
            logger.info("Set input path to " + Parameters.inputPath)
          }
          case "OUTPUT" => {
            Parameters.outputPath = value;
            logger.info("Set output path to " + Parameters.outputPath)
          }
          case "IR" => {
            Parameters.initialRecoveredFraction = value.toFloat / 100f;
            logger.info("Set initial recovered fraction to " + Parameters.initialRecoveredFraction)
          }
          case "BETA" => {
            Parameters.beta = value.toDouble;
            logger.info("Set beta (lambda_S) to " + Parameters.beta)
          }
          case "HCM" => {
            Parameters.householdContactMultiplier = value.toDouble;
            logger.info("Set household contact multiplier to " + Parameters.householdContactMultiplier)
          }
          case "WCM" => {
            Parameters.workplaceContactMultiplier = value.toDouble;
            logger.info("Set workplace contact multiplier to " + Parameters.workplaceContactMultiplier)
          }
          case "SCM" => {
            Parameters.schoolContactMultiplier = value.toDouble;
            logger.info("Set school contact multiplier to " + Parameters.schoolContactMultiplier)
          }
          case "FILES" => {
            val op_types = value.toCharArray
            for (op <- op_types) {
              op match {
                case 'T' => {
                  Parameters.saveTotalOutput = true
                }
                case 'A' => {
                  Parameters.saveAgewiseOutput = true
                }
                case 'I' => {
                  Parameters.saveInfectionInfoOutput = true
                }
                case '+' => {
                  Parameters.saveAgentOutput = true
                }
                case _ => {
                  throw new Exception(s"Unsupported output file: \"" + op + "\". Available outputs are \"T\" (Total number), \"A\" (Age-stratified numbers), \"I\" (Infection-info output), \"H\" (GIS Heatmap output), \"W\" (Ward-wise output), \"G\" (Gridded output).")
                }
              }
            }
          }
          case "SIMDAYS" => {
            Parameters.SIMDAYS = value.toInt;
            logger.info("Set total simulation duration to " + Parameters.SIMDAYS + " days")
          }
          case "IEMSM" => {
            Parameters.initialMSMExposedNumber = value.toInt;
            logger.info("Set initial exposed number of MSM to " + Parameters.initialMSMExposedNumber)
          }
          case "IE" => {
            Parameters.initialNonMSMExposedNumber = value.toInt;
            logger.info("Set initial exposed number of non MSM to " + Parameters.initialNonMSMExposedNumber)
          }
          case "MSMIPROB" => {
            Parameters.msmInfectionProb = value.toDouble;
            logger.info("Set infection probability per encounter to " + Parameters.msmInfectionProb)
          }
          case "MEETDAYS" => {
            Parameters.meeting_rate = 1d / value.toDouble;
            logger.info("MSMs meet on average every " + value.toDouble + " days, which is a rate of " + Parameters.meeting_rate)
          }
          case "VPD" => {
            Parameters.vaccinesPerDay = value.toInt;
            logger.info("Set number of vaccines available per day to " + Parameters.vaccinesPerDay)
          }
          case "RBV" => {
            Parameters.riskBasedVaccination = value.toBoolean;
            if (Parameters.riskBasedVaccination) logger.info("Set vaccination strategy to risk-based (MSM) vaccination") else logger.info("Set vaccination strategy to random (MSM) vaccination")
          }
          case "MSMIDS" => {
            if (value.size > 0) {
              Parameters.msm_ids = value.split(',')
                .toList
                .flatMap { case "" => 0 :: Nil
                case a => a.toInt :: Nil
                }
            }
            logger.info("Set ids of MSMs to " + Parameters.msm_ids.toString())
          }
          case "NPOP" => {
            Parameters.npop = value.toInt
            Parameters.all_ids = (0 until Parameters.npop).toList
            logger.info("Set total population size to " + Parameters.npop + " and created associated list of non-MSM agents")
          }
          case _ => {
            throw new Exception(s"Unsupported flag: \"" + key + "\". Available flags are \"INPUT\", \"OUTPUT\", \"IR\", \"BETA\", \"HCM\", \"WCM\", \"SCM\", \"FILES\", \"SIMDAYS\", \"IEMSM\", \"MSMIPROB\", \"MEETDAYS\", \"VPD\", \"RBV\", \"MSMIDS\", \"NPOP\".")
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    parseArgs(args)
    var beforeCount = 0
    val simulation = Simulation()

    Parameters.non_msm_ids = Parameters.all_ids diff Parameters.msm_ids

    Parameters.msm_ids = scala.util.Random.shuffle(Parameters.msm_ids)
    Parameters.non_msm_ids = scala.util.Random.shuffle(Parameters.non_msm_ids)

    exposedMSMs = Parameters.msm_ids.take(Parameters.initialMSMExposedNumber) // Take exposed number of MSMs from list
    exposedNonMSMs = Parameters.non_msm_ids.take(Parameters.initialNonMSMExposedNumber) // Take non-MSM exposed number from list

    //    println(Parameters.msm_ids)
    //    println(exposedMSMs)
    //    println(exposedNonMSMs)

    simulation.ingestData(implicit context => {
      ingestCSVData(Parameters.inputPath + ".csv", csvDataExtractor)
      logger.debug("Ingestion done")

      val n_contacts_list = ncontacts_listbuffer.toList
      Parameters.lr_ncons_threshold = quantile(n_contacts_list, Parameters.lr_percentile)
      Parameters.hr_ncons_threshold = quantile(n_contacts_list, Parameters.hr_percentile)

      logger.info("Low and high risk msm n_contacts at "+Parameters.lr_ncons_threshold + " (LR) and " + Parameters.hr_ncons_threshold + " (HR)")

    })

    simulation.defineSimulation(implicit context => {
      create12HourSchedules()

      registerAction(
        StopSimulation,
        (context: Context) => {
          (getInfectiousCount(context) == 0) ||
            (context.getCurrentStep == Parameters.SIMDAYS * Parameters.numberOfTicksInADay)
        }
      )

      beforeCount = getInfectiousCount(context)

      registerAgent[Person]
      if (Parameters.vaccinesPerDay > 0) {
        vaccination
      }

      currentTime = new Date().getTime

      if (Parameters.saveTotalOutput) {
        SimulationListenerRegistry.register(
          new CsvOutputGenerator(Parameters.outputPath + "total_" + currentTime + ".csv", new SIROutputSpec(context))
        )
      }

      if (Parameters.saveAgewiseOutput) {
        SimulationListenerRegistry.register(
          new CsvOutputGenerator(Parameters.outputPath + "/agewise_" + currentTime + ".csv", new AgewiseSIROutput(context))
        )
      }

      if (Parameters.saveInfectionInfoOutput) {
        SimulationListenerRegistry.register(
          new CsvOutputGenerator(Parameters.outputPath + "/infectioninfo_" + currentTime + ".csv", new InfectionInfoOutput(context))
        )
      }

    })

    simulation.onCompleteSimulation { implicit context =>
      if (Parameters.saveAgentOutput) {
        val agentOutputGenerator = new CsvOutputGenerator(Parameters.outputPath + "/agentinfo_" + currentTime + ".csv", new AgentInfoOutput(context))
        agentOutputGenerator.onSimulationStart(context)
        agentOutputGenerator.onStepStart(context)
        agentOutputGenerator.onSimulationEnd(context)
      }
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
      (employeeSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age > Parameters.maxStudentAge, 2),
      (studentSchedule, (agent: Agent, _: Context) => agent.asInstanceOf[Person].age <= Parameters.maxStudentAge, 3)
    )
  }

  private def csvDataExtractor(map: Map[String, String])(implicit context: Context): GraphData = {

    val citizenId = map("AgentID").toLong
    val age: Int = map("Age").toInt
    val gender: String = map("Gender")
    val msm: Boolean = map("MSM").toBoolean
    var initialInfectionState = "Susceptible"

    if (msm && exposedMSMs.contains(citizenId.toInt)) {
      initialInfectionState = "Exposed"
      logger.info("Infected MSM agent: {}", citizenId)
    }
    else if (!msm && exposedNonMSMs.contains(citizenId.toInt)) {
      initialInfectionState = "Exposed"
      logger.info("Infected non-MSM agent: {}", citizenId)
    }


    val homeId = map("HHID").toLong
    val schoolId = map("SchoolID").toLong
    val officeId = map("WorkplaceID").toLong

    val n_contacts = map("NumberOfContacts").toInt

    var contacts = List.empty[Int]

    if (msm) {
      val contactList = map("Contacts") // Remove brackets around list

      if (contactList.size > 0) {
        contacts = contactList.split(',')
          .toList
          .flatMap { case "" => 0 :: Nil
          case a => a.toInt :: Nil
          }
      }
      ncontacts_listbuffer += n_contacts
    }

    val citizen: Person = Person(id = citizenId, age = age, gender = gender, msm = msm,
      infectionState = InfectionStatus.withName(initialInfectionState),
      daysInfected = 0, n_contacts = n_contacts, contacts = contacts
    )

    if (initialInfectionState == "Susceptible") {
      citizen.updateParam("infectionState", Susceptible)
    }
    else if (initialInfectionState == "Exposed") {
      citizen.updateParam("infectionState", Exposed)
    }
    else if (initialInfectionState == "Infected") {
      citizen.updateParam("infectionState", Infected)
    }
    else {
      citizen.updateParam("infectionState", Recovered)
    }

    val home = House(homeId)
    val staysAt = Relation[Person, House](citizenId, "STAYS_AT", homeId)
    val memberOf = Relation[House, Person](homeId, "HOUSES", citizenId)

    val graphData = GraphData()
    graphData.addNode(citizenId, citizen)
    graphData.addNode(homeId, home)
    graphData.addRelations(staysAt, memberOf)

    if (age >= Parameters.maxStudentAge) {
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


  private def vaccination(implicit context: Context): Unit = {

    var ActivatedAt = 0
    val interventionName = "vaccination"
    val activationCondition = (context: Context) => {
      val conditionMet = context.getCurrentStep * Parameters.dt >= Parameters.vaccinationStartDate
      if (conditionMet) {
        vaccinationStartedOn = context.getCurrentStep * Parameters.dt
        logger.info("Vaccination started on day " + vaccinationStartedOn)
      }
      conditionMet
    }

    val firstTimeExecution = (context: Context) => {
      ActivatedAt = context.getCurrentStep
    }
    val deActivationCondition = (context: Context) => {
      false
    }

    val perTickAction = (context: Context) => {

      if (context.getCurrentStep % Parameters.numberOfTicksInADay == 0) {

        val allMSMIterator: Iterator[GraphNode] = context.graphProvider.fetchNodes("Person", (("msm" equ true) and ("isVaccinated" equ false)))

        var vaccinesAdministeredToday = 0
        val nshots = Parameters.vaccinesPerDay

        val agentsToVaccinate = ListBuffer[Person]()

        if (Parameters.riskBasedVaccination) {
          var hrList = ListBuffer[Person]()
          var mrList = ListBuffer[Person]()
          var lrList = ListBuffer[Person]()

          allMSMIterator.foreach(node => {
            val person = node.as[Person]

            val n_contacts = person.n_contacts
            var risk = 0

            if ((n_contacts >= Parameters.lr_ncons_threshold) && (n_contacts < Parameters.hr_ncons_threshold)) {
              risk = 1
              mrList += person
            }
            else if (n_contacts >= Parameters.hr_ncons_threshold) {
              risk = 2
              hrList += person
            }
            else {
              lrList += person
            }
          })

          if (agentsToVaccinate.length <= nshots) {
            hrList = Random.shuffle(hrList)
            hrList.foreach(person => {
              if (!person.isVaccinated && vaccinesAdministeredToday < nshots) {
                agentsToVaccinate += person
                vaccinesAdministeredToday += 1
              }
            })
          }

          if (agentsToVaccinate.length <= nshots) {
            mrList = Random.shuffle(mrList)
            mrList.foreach(person => {
              if (!person.isVaccinated && vaccinesAdministeredToday < nshots) {
                agentsToVaccinate += person
                vaccinesAdministeredToday += 1
              }
            })
          }

          if (agentsToVaccinate.length <= nshots) {
            lrList = Random.shuffle(lrList)
            lrList.foreach(person => {
              if (!person.isVaccinated && vaccinesAdministeredToday < nshots) {
                agentsToVaccinate += person
                vaccinesAdministeredToday += 1
              }
            })
          }
          logger.info("Unvaccinated MSM at start: (HR={}, MR={}, LR={}). Vaccines administered today: {}", hrList.length, mrList.length, lrList.length, vaccinesAdministeredToday)
        }
        else {
          var allMSMList = allMSMIterator.toList
          allMSMList = Random.shuffle(allMSMList)
          val potentialAgentsToVaccinate = allMSMList.take(nshots)

          potentialAgentsToVaccinate.foreach(node => {
            val person = node.as[Person]
            agentsToVaccinate += person
            vaccinesAdministeredToday += 1
          })

          logger.info("Unvaccinated MSM at start: {}. Vaccines administered today: {}", allMSMList.length, vaccinesAdministeredToday)
        }

        agentsToVaccinate.foreach(person => {
          logger.info("Agent " + person.id + " was vaccinated on day " + context.getCurrentStep * Parameters.dt)
          person.updateParam("isVaccinated", true)
          person.updateParam("vaccinatedOnDay", context.getCurrentStep * Parameters.dt)
        })

        //        if(vaccinesAdministeredToday < nshots){
        //          logger.info("Vaccines wasted = {}", nshots-vaccinesAdministeredToday)
        //        }
        //        else if(vaccinesAdministeredToday>nshots){
        //          logger.info("ERROR: Vaccines administered should not be greater than available shots!")
        //        }

      }
    }

    val intervention: Intervention = SingleInvocationIntervention(interventionName, activationCondition, deActivationCondition, firstTimeExecution, perTickAction)

    registerIntervention(intervention)
  }

  private def printStats(beforeCount: Int)(implicit context: Context): Unit = {
    val afterCountSusceptible = getSusceptibleCount(context)
    val afterCountInfected = getInfectiousCount(context)
    val afterCountRecovered = getRecoveredCount(context)

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

  private def getInfectiousCount(context: Context): Int = {
    context.graphProvider.fetchCount("Person", ("infectionState" equ Infected) or ("infectionState" equ Exposed))
  }

  private def getRecoveredCount(context: Context) = {
    context.graphProvider.fetchCount("Person", "infectionState" equ Recovered)
  }


  def quantile(arr: List[Int], q: Double): Double = {
    if (arr.isEmpty) throw new IllegalArgumentException("Array must not be empty")

    val sorted = arr.sorted
    val index = (q * (sorted.length - 1)).toInt

    // Linear interpolation between elements if needed
    if (index == sorted.length - 1) sorted(index)
    else {
      val lower = sorted(index)
      val upper = sorted(index + 1)
      lower + (upper - lower) * (q * (sorted.length - 1) - index)
    }
  }
}