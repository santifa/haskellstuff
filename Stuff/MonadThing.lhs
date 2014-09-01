
Einige Merksachen zu Funktoren, Applicatives und Monaden
und einige weitere Eläuterungen als Gedankenstützen.

> module MonadThing where
> import Prelude hiding (Maybe, Just, Nothing)
> import Control.Applicative

Normalerweise werden Funktionen auf Werte angewendet
(+3) 2 -> 5

Nun packen wir Werte in einen Context |2| oder |x|
Wenn man nun Funktionen darauf anwendet bekommt man je nach Kontext andere Werte.
(Haben alle gemeinsam) z.B.

> data Maybe a = Nothing | Just a deriving Show

-----------------------------
Funktoren

Man kann eine Funktion ohne Kontext nicht auf Variablen mit Kontext anwenden.
fmap weiß wie!

Funktor ist also definiert als:

class Functor f where
  fmap :: (a -> b) -> f a -> f b
          function -> functor -> new functor

Jeder Datentyp der fmap definiert ist ein Funktor.
z.B.

> instance Functor Maybe where
>   fmap func (Just val) = Just (func val)
>   fmap _ Nothing = Nothing

fmap (+3) (Just 2)
fmap (+3) Nothing


Die Infix Version ist <$> (Control.Applicative), also (+3) <$> (Just 2)
Bei Funktionen wie erwartet (+3> <$> (+2) -> neue Funktion die (+5) rechnet.
Also sind Funktionen wie Listen auch Funktoren.

-----------------------------
Applicatives

Bei Applicatives haben Variablen und Funktionen einen Kontext.
Control.Applicative besitzt <*> dafür.

<*> Kann jede Anzahl an Argumenten aufnehmen.

(*) <$> Just 5 <*> Just 3 -> Just 15
oder
liftA2 (*) (Just 5) (Just 3)

-----------------------------
Monaden

Funktoren wenden Funktionen auf Variablen mit Kontext an;
Applicatives wenden Funktionen mit Kontext auf Variablen mit Kontext an;

Monaden wenden Funktionen die "wrapped values" zurückgeben auf "wrapped values" an.
Die Funktion ist >>= - "bind"

Maybe ein Beispiel:

> half x = if even x then Just (x `div` 2) else Nothing

half nimmt einen Wert und liefert ihn mit Kontext (Maybe) zurück.
bind ermöglicht nun einen Wert mit Kontext in einen anderen Wert mit Kontext zu überführen.

> foo = Just 3 >>= half
> bar = Just 4 >>= half

Die Monade ist eine Typklasse mit:

class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
        Monade -> Funktion mit Rückgabe Monade -> Monade

> instance Monad Maybe where
>   Nothing >>= func = Nothing
>   Just val >>= func = func val

Eine andere Monade, die IO Monade
z.B. 

> fuu :: IO()
> fuu = getLine >>= readFile >>= putStrLn

Syntaktischer Zucker für Monaden

> fuu' :: IO()
> fuu' = do
>   filename <- getLine
>   content <- readFile filename
>   putStrLn content

Zusammengefasst:
 * Funktoren sind Datentypen die die "Functor" Typklasse implementieren
 * Applicatives implementieren die Typklasse "Applicative"
 * Monaden implimentieren die Typklasse "Monad"
 * Funktoren: Funktion mit "wrapped value" mit "fmap" oder <$>
 * Applicatives: "wrapped function" mit "wrapped value" mit <*> oder "liftA"
 * Monaden: "wrapped value" mit Funktion die "wrapped value" zurückgibt mit >>=

