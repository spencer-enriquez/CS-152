package Duel

class CrushMasher(override val name: String = "CrushMasher") extends Gladiator with Crusher with Masher{
  override def attack(opp: Gladiator) {
     super.attack(opp) 
     crush(opp) 
     mash(opp)
   }
}

object CrushMasher {
   def apply(name: String) = new CrushMasher(name)
}