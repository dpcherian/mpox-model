package sir

object Parameters {

  // Simulation parameters

  var SIMDAYS: Int = 300
  var inputPath = "./hpc/mpox_population"
  var outputPath= "./outputs/"

  var msm_ids : List[Int] = List()
//    List(16,   84,   87,  265,  335,  404,  437,  441,  447,  456,  747,
//        840,  888,  891,  971,  983, 1274, 1370, 1421, 1592, 1638, 1727,
//       1830, 1959, 2123, 2139, 2252, 2259, 2270, 2312, 2347, 2373, 2395,
//       2776, 2787, 2873, 3173, 3369, 3454, 3493, 3704, 3953, 3978, 3979,
//       4237, 4347, 4639, 4727, 4732, 4796, 5016, 5108, 5132, 5298, 5343,
//       5365, 5494, 5712, 5835, 6133, 6148, 6282, 6404, 6420, 6527, 6554,
//       6787, 6795, 6871, 7114, 7343, 7424, 7610, 7883, 7908, 8183, 8235,
//       8251, 8390, 8449, 8482, 8485, 8513, 8638, 8649, 8723, 8838, 8861,
//       8925, 9006, 9057, 9221, 9409, 9552, 9582, 9694, 9764, 9832, 9896,
//       9931)
  var all_ids : List[Int] = List()
  var non_msm_ids : List[Int] = List()

  var npop : Int = 10_000

  // Output parameters

  var saveTotalOutput : Boolean = false
  var saveAgentOutput : Boolean = true
  var saveAgewiseOutput : Boolean = false
  var saveInfectionInfoOutput : Boolean = false

  var initialRecoveredFraction: Double = 0.0
  var initialNonMSMExposedNumber: Int = 0
  var initialMSMExposedNumber   : Int = 0

  val maxStudentAge : Int = 18

  var beta: Double = 0.3
  final val lambda_E : Double = 1d/7
  final val lambda_I : Double = 1d/21

  var msmInfectionProb: Double = 1.0

  var householdContactMultiplier: Double = 1.0
  var workplaceContactMultiplier: Double = 0.0
  var schoolContactMultiplier: Double = 0.0

  var meeting_rate : Double = 1f/7

  final val numberOfTicksInADay: Int = 2
  final val dt : Double = 1f/numberOfTicksInADay

  // Vaccine parameters

  var riskBasedVaccination : Boolean = false

  var vaccinesPerDay : Int = 0
  var vaccinationStartDate : Double = 0.0

  var lr_percentile = 0.25
  var hr_percentile = 0.75

  var lr_ncons_threshold = 1.0
  var hr_ncons_threshold = 3.0

  var vaccinatedBetaReduction : Double = 0.75
  var vaccinatedMuReduction   : Double = 0.75
  var vaccinatedTransmissionReduction: Double = 0.0

}
