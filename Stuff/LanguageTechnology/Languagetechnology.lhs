

> module Stuff.Languagetechnology.LanguageTechnology where

Satz von Bayes:

P(X|Y) = P (Y|X) * P(X) / P(Y)

Wahrscheinlichkeit (P), dass X eintritt gegeben (|), 
dass die Bedingung gleich Y.

B1) A1) Binominalverteilung
  20% defekte Halbleiter
  1) Unter 10 genau 5 Stück
  2) Unter 10 höchstens 3 Stück

  Allgemein)
    p = 0.2 defekt
    n = 10 Stück

  1) "Genau" P (X = k) = (n über k) * p^k * (1-p)^n-k
     P = (10 über 5) * 0.2^5 * (1-0.2)^10-5
     P = (10 über 5) * 0.2^5 * 0.8^5
     P = 10! / (5! * (10-5)!) * 0.2^5 * 0.8^5
     P = 0.0264241152

> binom :: Float -> Float -> Float -> Float
> binom p k n = fak n / (fak k * fak (n-k)) * p ** k * (1-p)**(n-k)

> fak :: Eq a => Fractional a => a -> a
> fak 0 = 1
> fak n = n * fak (n-1)

  2) "Höchstens" P (X <= k) = Summe von i=1 bis k . (n über i) * p^i * (1-p)^n-i
    P = (10 über 1)*0.2*0.8^9 + (10 über 2)*0.2^2*0.8^8 + (10 über 3)*0.2^3*0.8^7
    P = 0.77172

> maxBinom :: Float -> Float -> Float -> Float
> maxBinom p k n = sum [ binom p y n| y <- [1..k]]

  A2) Satz von Bayes
    P(M) = 0.49   P(Rau|M) = 0.28   P(R|M) = 0.15
    P(F) = 0.51   P(Rau|F) = 0.24   P(R|F) = 0.05
    P(M|Rau und !R)? Welches Attribut sagt mehr über Geschlecht aus?
    P(!R|M) = 0.85
    P(!R|F) = 0.95

    P(M|Rau) = P(Rau|M)*P(M)/(P(Rau|M)+P(Rau|F)*P(F)
    P(M|Rau) = 0.28*0.49/(0.28+0.24*0.51) = 0.34095

    P(M|!R) = P(!R|M)*P(M)/(P(!R|M)+P(!R|F)*P(F)
    P(M|!R) = 0.85*0.49/(0.85+0.95*0.51) = 0.3121

    P(M|Rau und !R) = P(M|!R) * P(M|Rau) = 0.106410

    Radsport sagt mehr über das Geschlecht aus.

