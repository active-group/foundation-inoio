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

case class Date(iso: String)

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
                   amount: Amount, currency: Currency)

// Semantik
// Syntax |-> Bedeutung
def meaning(contract: Contract): List[Payment] = ???
