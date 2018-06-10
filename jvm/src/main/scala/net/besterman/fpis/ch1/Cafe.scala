package net.besterman.fpis.ch1

object Cafe {

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee()
    val charge = Charge(cc, cup.price)
    (cup, charge)
  }

  def buyCoffees(num: Int, cc:CreditCard): (List[Coffee], Charge) = {
    val purchases = List.fill(num)(buyCoffee(cc)) // List of (Coffee, Charge) tuples
    val (coffees, charges) = purchases.unzip // "coffees" is now a List[Coffee] and charges is List[Charge]
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }

  val NUM_COFFEES = 10

  def main(args: Array[String]): Unit = {
    val purchase = buyCoffees(NUM_COFFEES, CreditCard())
    val coffees = purchase._1
    val charge = purchase._2
    printf("Bought %d coffees for $%2.2f", coffees.length, charge.amt)
  }
}
