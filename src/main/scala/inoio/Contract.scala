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
 */

case class Date(iso: String)

type Amount = Double

enum Currency {
  case EUR
  case GBP
  case USD
  case YEN
}

enum Contract {
  // case ZeroCouponBond(date: Date, amount: Amount, currency: Currency)
  case One(currency: Currency)
  case Many(amount: Amount, contract: Contract)
  case Then(date: Date, contract: Contract)
}

import Contract._
import Currency._

// "Ich bekomme 1€ jetzt."
val c1 = One(EUR)

// val zcb1 = Then(Date("2025-12-24"), Many(100, One(EUR)))

// x + y = y + x

def zeroCouponBond(date: Date, amount: Amount, currency: Currency): Contract =
  Then(date, Many(amount, One(currency)))

val zcb1 = zeroCouponBond(Date("2025-12-24"), 100, EUR)