package sirFSM

import com.bharatsim.engine.models.Network

case class School(id: Long) extends Network {
  addRelation[Person]("TEACHES")

  override def getContactProbability(): Double = 1
}
