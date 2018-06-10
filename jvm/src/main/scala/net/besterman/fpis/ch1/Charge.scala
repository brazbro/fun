package net.besterman.fpis.ch1

case class Charge(cc:CreditCard, amt: Double) {

  def combine(other: Charge): Charge = {
    if (other.cc == cc)
      Charge(cc, amt + other.amt)
    else
      throw new Exception("Cannot combine charges on different credit cards")
  }
}
