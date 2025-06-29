package sir

import com.bharatsim.engine.models.Network

case class Office(id: Long) extends Network {
  addRelation[Person]("EMPLOYER_OF")

  override def getContactProbability(): Double = Parameters.workplaceContactMultiplier
}
