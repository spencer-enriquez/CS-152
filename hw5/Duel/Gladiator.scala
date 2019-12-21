package Duel

/*
 * Problem 2: Gladiator Duel
 */
class Gladiator(val name: String = "Gladiator") {
  private var hp = 100
  def health = hp
  
  def damage(damage: Int) {
      if (hp != 0) {
        if (damage > hp && hp != 0) {hp = 0; println(name + " has been defeated.")} 
        else hp = hp - damage
      }
  }
  def attack(opp: Gladiator) {
    if (hp != 0) {
      println(name + " is attacking " + opp.name) 
      opp.damage((math.random * hp).asInstanceOf[Int])
    }
  }
}

trait Slasher {
  def slash(opp: Gladiator) {
    if (opp.health > 0) {
      println("Slash, slash, slashing " + opp.name)
      opp.damage(10)
      println(opp.name + "'s health = " + opp.health)
    }
  }
}

trait Crusher {
  def crush(opp: Gladiator) {
    if (opp.health > 0) {
      println("Crush, crush, crushing " + opp.name)
      opp.damage(10)
      println(opp.name + "'s health = " + opp.health)
    }
  }
}

trait Masher {
  def mash(opp: Gladiator) {
    if (opp.health > 0) {
      println("Mash, mash, mashing " + opp.name) 
      opp.damage(10) 
      println(opp.name + "'s health = " + opp.health)
    }
  }
}

object testBattle extends App{
    val bee = new Gladiator("Bumblebee") with Slasher with Masher {
      override def attack(opp: Gladiator) {super.attack(opp) 
        if (health > 0) {slash(opp); mash(opp)}
      }
    }
    val maximus = CrushMasher("Optimus Prime")
    for(i <- 0 to 5 if (maximus.health > 0 && bee.health > 0)) {
     maximus.attack(bee)
     bee.attack(maximus)
    }
}