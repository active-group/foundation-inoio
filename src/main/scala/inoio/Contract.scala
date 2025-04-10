package inoio

/*
 * Komplexe Datenmodellierung
 * 
 * 1. einfaches Beispiel
 * Zero-Bond / zero-coupon bond
 * "Ich bekomme am 24.12.2025 100€."
 * 2. in "atomare" Bestandteile zerlegen
 *    - Währung: "Ich bekomme 1€ jetzt."
 *    - "Many": "Ich bekomme 100€ jetzt."
 *    - "Then"
 *    ... und Kombinatoren finden
 * ... und von vorn   
 *
 * Currency swap:
 * "Am 24.12.2025 bekomme ich 100€ und ich zahle 100GBP."
 */

case class Date(iso: String) {
  def isBeforeOrEqual(other: Date): Boolean =
    this.iso <= other.iso
}

type Amount = Double

enum Currency {
  case EUR
  case GBP
  case USD
  case YEN
}

enum Direction {
  case Long
  case Short
  def invert: Direction =
    this match {
      case Long => Short
      case Short => Long
    }
}
import Direction._

enum Contract {
  // case ZeroCouponBond(date: Date, amount: Amount, currency: Currency)
  case One(currency: Currency)
  case Many(amount: Amount, contract: Contract)
  case Then(date: Date, contract: Contract)
  // And(c1, And(c2, c3)) =...= And(And(c1, c2), c3)
  case And(contract1: Contract, contract2: Contract) // Halbgruppe
  case Zero // neutrale Element: Monoid
  case Neg(contract: Contract)

  // smart constructor
  def and(contract1: Contract, contract2: Contract): Contract =
    (contract1, contract2) match {
      case (Zero, _) => contract2
      case (_, Zero) => contract1
      case _ => And(contract1, contract2)
    }
}


import Contract._
import Currency._

// "Ich bekomme 1€ jetzt."
val c1 = One(EUR)

// "Ich zahle 1€ jetzt."
val c2 = Neg(c1)

// "Ich bekomme 1€ jetzt."
// val c3 = Dir(Long, c1)

// "Ich bekomme 1€."
// val c4 = Dir(Short, c2)

// val zcb1 = Then(Date("2025-12-24"), Many(100, One(EUR)))

// x + y = y + x

def zeroCouponBond(date: Date, amount: Amount, currency: Currency): Contract =
  Then(date, Many(amount, One(currency)))

val zcb1 = zeroCouponBond(Date("2025-12-24"), 100, EUR)

case class Payment(direction: Direction, date: Date,
                   amount: Amount, currency: Currency) {
  def scale(factor: Amount): Payment =
    this.copy(amount = this.amount * factor)

  def invert: Payment = this.copy(direction = direction.invert)
}

// Semantik
// Syntax |-> Bedeutung
// Zahlungen bis heute, raus: Residualvertrag
def meaning(contract: Contract, today: Date): (List[Payment], Contract) =
  contract match {
    case Zero => (List.empty, Zero)
    case And(contract1, contract2) => 
      val (payments1, residual1) = meaning(contract1, today)
      val (payments2, residual2) = meaning(contract2, today)
      (payments1 ++ payments2, And(residual1, residual2))
    case Many(amount, contract) =>
      val (payments, residual) = meaning(contract, today)
      (payments.map(_.scale(amount)), Many(amount, residual))
    case Neg(contract) =>
      val (payments, residual) = meaning(contract, today)
      (payments.map(_.invert), Neg(residual))
    case One(currency) =>
      (List(Payment(Long, today, 1, currency)), Zero)
    case Then(date, contract1) =>
      if date.isBeforeOrEqual(today)
      then meaning(contract1, today)
      else 
        (List.empty, contract)
  }

val c3 = Many(50, And(One(EUR), Then(Date("2025-12-24"), One(EUR))))