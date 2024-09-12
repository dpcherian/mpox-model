package sirFSM

object Disease {

  // Simulation parameters

  var SIMTICKS: Int = 200
  val studentAge : Int = 0

  final val beta: Double = 0.2
  final val lambda_I : Double = 1f/14

  var msmInfectionProb: Double = 0.1

  final val meeting_rate : Double = 1f/7

  final val numberOfTicksInADay: Int = 2
  final val dt : Double = 1f/numberOfTicksInADay
1

}
