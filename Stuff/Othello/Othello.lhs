\documentclass{article}
%include lhs2TeX.fmt
%include polycode.fmt
\usepackage[ngerman]{babel}
\usepackage{ucs}
\usepackage[utf8x]{inputenc}
\usepackage{fullpage}
\usepackage{url}
\setlength{\parindent}{0pt}
\setlength{\parskip}{1.8ex}
\begin{document}
\begin{verbatim}
----------------------------------------------------------------------
--
--  Seminar Theoretische Informatik                         \ \
--  "Funktionale Programmierung mit Haskell"                 \ \ ---
--  Universität Potsdam SS 2014                              / /\ --
--  Tim Richter / Mario Frank                               / /  \
--
-- Projekt
-- Abgabe bis: 14.08.2014
-- Name: Henrik Jürges
-- Note:
----------------------------------------------------------------------
\end{verbatim}

> module Othello where
> import Data.Maybe
> import Data.List (unfoldr, findIndices)

\section{Einführung}

In diesem Projekt soll ein einfaches Othello-Programm entwickelt werden.
Die Regeln des Spiels finden Sie unter http://de.wikipedia.org/wiki/Othello\_(Spiel).

Hier sei nur auf die Besonderheiten der Reversi-Variante Othello hingewiesen:

\begin{enumerate}
\item Die Startaufstellung ist stets die folgende (X steht für ''schwarz'', O
für ''weiß''):

\begin{centering}
\begin{verbatim}

   a   b   c   d   e   f   g   h
     |   |   |   |   |   |   |    8
  -------------------------------
     |   |   |   |   |   |   |    7
  -------------------------------
     |   |   |   |   |   |   |    6
  -------------------------------
     |   |   | O | X |   |   |    5
  -------------------------------
     |   |   | X | O |   |   |    4
  -------------------------------
     |   |   |   |   |   |   |    3
  -------------------------------
     |   |   |   |   |   |   |    2
  -------------------------------
     |   |   |   |   |   |   |    1

\end{verbatim}
\end{centering}

\item Es gibt zu Beginn keine Aufteilung der Spielsteine. Passen kann (und muss) ein
Spieler also genau dann, wenn er keinen Stein setzen kann.
\item Beim Setzen eines Steins werden alle eingeschlossenen gegnerischen Steine
umgedreht - eine Richtungsauswahl für das Umdrehen gibt es nicht.
\end{enumerate}

Zur Vereinfachung lassen wir kein Remis zu, siehe unten.

\pagebreak
Beispiel für einen gültigen Zug - ''O'' setzt auf e2:

\begin{centering}
\begin{verbatim}
  a   b   c   d   e   f   g   h         a   b   c   d   e   f   g   h
    |   |   |   |   |   |   |    8        |   |   |   |   |   |   |    8
 -------------------------------       -------------------------------
    |   |   |   |   |   |   |    7        |   |   |   |   |   |   |    7
 -------------------------------       -------------------------------
    |   |   | X | O |   |   |    6        |   |   | X | O |   |   |    6
 -------------------------------       -------------------------------
    |   |   | X | X |   |   |    5        |   |   | X | O |   |   |    5
 -------------------------------       -------------------------------
    |   | O | X | X | O |   |    4        |   | O | X | O | O |   |    4
 -------------------------------       -------------------------------
    |   |   | X | X | X |   |    3        |   |   | O | O | X |   |    3
 -------------------------------       -------------------------------
    |   |   |   |   |   |   |    2        |   |   |   | O |   |   |    2
 -------------------------------       -------------------------------
    |   |   |   |   |   |   |    1        |   |   |   |   |   |   |    1

     vorher                                nachher
\end{verbatim}
\end{centering}


\section{Datentypen, grundlegende Funktionen}

Wir verwenden folgende Datentypen

> data Color = X | O deriving (Show,Eq)
> type Pos   = (Char,Int)
> data Board = Bo { unBo :: Pos -> Maybe Color }

> intersperse :: a -> [a] -> [a]
> intersperse _ []     = []
> intersperse _ [y]    = [y]
> intersperse x (y:ys) = y:x:intersperse x ys

> intercalate :: [a] -> [[a]] -> [a]
> intercalate xs xss = concat (intersperse xs xss)

> instance Show Board where
>  show (Bo f) =     "\n a   b   c   d   e   f   g   h\n" ++
>       ( intercalate "\n-------------------------------\n" (
>         zipWith (++) (map ((" "++).(intercalate " | ") . (map showMC)) bss)
>                    (map (("  "++) . show) [8,7..1])))  where
>       showMC (Just X) = "X"
>       showMC (Just O) = "O"
>       showMC Nothing  = " "
>       bss = [ [f (x,y) | x <- ['a'..'h'] ] | y <- [8,7..1] ]




Pos repräsentiert Felder (Positionen) auf dem Brett. ''Valide''
Positionen sind solche mit erster Komponente im Bereich |['a'..'h']|
und zweiter Komponente im Bereich |[1..8]|.

Ein Element von |Board| repräsentiert eine Brettsituationen durch
Angabe einer Funktion, die einer Position |Nothing| zuordnet, wenn
das entsprechende Feld leer ist, und |Just color|, falls ein Spielstein
mit der |color|-Seite nach oben auf dem Feld liegt.

Definieren Sie

< emptyBoard, startBoard :: Board

> emptyBoard :: Board
> emptyBoard = Bo b
>   where b _ = Nothing

> startBoard :: Board
> startBoard = Bo b
>   where b pos
>             | pos == ('d', 5) || pos == ('e', 4) = Just O
>             | pos == ('d', 4) || pos == ('e', 5)= Just X
>             | otherwise = Nothing


wo |emptyBoard| ein leeres Spielfeld und |startBoard| die oben angegebene
Startstellung repräsentiert.

Definieren Sie Funktionen

< content :: Pos -> Board -> Maybe Color
< set :: Color -> Pos -> Board -> Board

> content :: Pos -> Board -> Maybe Color
> content pos (Bo b) = b pos

> set :: Color -> Pos -> Board -> Board
> set color pos (Bo b) = Bo $ if isJust $ b pos then b else newBo where
>                   -- concat old function to the new one and add a guard
>                   newBo nPos
>                       | nPos == pos = Just color
>                       | otherwise = b nPos

so, dass |content p b| der Inhalt des Felds |p| bei Brettsituation |b| ist
und dass |set| einen Stein der angegebenen Farbe auf das angegebene Feld setzt (und
die anderen Felder unverändert lässt). Falls |b| an der Position |p| bereits besetzt
ist, soll |set c p b == b| sein.

\section{Walking the Board}

Wir führen folgendes Typsynonym ein

> type Step = Pos -> Pos

Definieren Sie Elemente

> nw, no, ne, we, ea, sw, so, se :: Step

> nw (x, y) = (pred x, succ y)
> no (x, y) = (x, succ y)
> ne (x, y) = (succ x, succ y)
> we (x, y) = (pred x, y)
> ea (x, y) = (succ x, y)
> sw (x, y) = (pred x, pred y)
> so (x, y) = (x, pred y)
> se (x, y) = (succ x, pred y)

die jeweils einen ''Schritt'' auf dem Brett (oder über den Rand) in die
angedeutete Himmelsrichtung repräsentieren. Z.B. soll

< step sw ('c',2) = ('b',1)

sein. Hinweis: Sowohl |Char| als auch |Int| sind Instanzen der Typklasse |Enum|!
Benutzen Sie die Funktionen dieser Typklasse!

Definieren Sie eine Funktion

> walk :: Pos -> Step -> [Pos]
> walk pos step = unfoldr f pos
>     -- unfold positions up to borders
>   where f (x, y)
>           | y < 1 || y > 8 = Nothing
>           | x < 'a' || x > 'h' = Nothing
>           | otherwise = Just ((x, y), step (x, y))


die eine Liste der validen Positionen zurückgibt, die beim Wiederholen
des angegebenen |Step| startend von der angegebenen Position durchlaufen werden.
Z.B. soll

< walk ('c',4) ne = [('c',4),('d',5),('e',6),('f',7),('g',8)]

sein.

\section{Steine drehen}

Implementieren Sie eine Funktion

< posToFlip :: Pos -> Board -> [Pos]

> posToFlip :: Pos -> Board -> [Pos]
>     -- reduce list to flipable enemy positions and concat
> posToFlip pos (Bo b) = concatMap (takeWhile (\p -> b p /= b pos)) flipable
>      -- star is list of list in all possible directions
>   where star = map (filter (pos /=)) [walk pos s| s <- [nw, no, ne, we, ea, sw, so, se]]
>      -- remove Nothing positions and filter lines where the color doesn't change
>         flipable = filter f $ map (takeWhile (isJust . b)) star
>           where f [] = False
>                 f (x:xs)
>                   | b x == b pos = True
>                   | otherwise = f xs

die die Liste der Positionen berechnet, deren Steine zur Vervollständigung
des Spielzuges umgedreht werden müssen, falls die angegebene Brettsituation
durch ein Setzen eines Steins an die angegebene Position (mittels |set|)
entstanden ist. Wenn |b| die oben unter ''vorher'' angegebene Brettsituation ist,
soll z.B. |posToFlip ('c',4) b| (eine beliebige Umordnung von)
|[('d',5),('d',4),('e',4)]| sein.

Definieren Sie eine Funktion

< flipAll :: [Pos] -> Board -> Board

> flipAll :: [Pos] -> Board -> Board
> flipAll [] (Bo b) = Bo b
> flipAll (p:ps) (Bo b) = flipAll ps (Bo nB)
>   where nB pos
>           | pos == p = Just $ rev $ fromJust $ b p
>           | otherwise = b pos

> rev :: Color -> Color
>   -- reverse a color
> rev color
>       | color == X = O
>       | color == O = X
>       | otherwise = O

die die Steine an allen angegebenen Positionen dreht. Sie dürfen annehmen,
dass das gegebene Brett tatsächlich an allen angegebenen Positionen einen
Spielstein hat.

\section{Spielsituation und Züge}

Eine Spielsituation wird durch die Angabe einer Brettsituation und des
Spielers, der am Zug ist, bestimmt. Wir definieren

> data GameState = GS Color Board  deriving Show

Ein Zug besteht entweder im Setzen eines Steins an eine angegebene Position
oder im ''Passen''.

> data Move = Put Pos | Pass deriving Show
> instance Eq Move where
>   Pass == Pass = True
>   Pass == Put _ = False
>   Put _ == Pass = False
>   Put x == Put y = x == y

Definieren Sie eine Funktion

< play :: Move -> GameState -> Maybe GameState

> play :: Move -> GameState -> Maybe GameState
> play Pass (GS c board)
>   | null validPos = Just $ GS (rev c) board
>   | otherwise = Nothing
>     where validPos = [(x, y) | x <- ['a'..'h'], y <- [1..8], valid (x, y) c board]

> play (Put pos) (GS c board)
>   | valid pos c board = Just $ GS (rev c) nBoard
>   | otherwise = Nothing
>     where nBoard = flipAll (posToFlip pos sBoard) sBoard
>           sBoard = set c pos board

> valid :: Pos -> Color -> Board -> Bool
>   -- checks wether a position is valid or not
>   -- first: no content; second: at least one flip; third: in board range
> valid pos c board = isNothing (content pos board) &&
>   not (null (posToFlip pos nBoard)) && inbound pos
>     where nBoard = set c pos board
>           inbound (x, y) = (x <= 'h' || x >= 'a') && (y < 9 || y > 0)

zum Ausführen eines Zuges (mit allen nötigen Drehungen von Steinen).
Falls der angegebene Zug in der gegebenen Spielsituation nicht valide ist,
soll |play| das Ergebnis |Nothing| liefern. Valide ist ein |Put p|, falls

\begin{itemize}
\item |p| eine valide Position ist,
\item das Brett an Position |p| leer ist und
\item durch Setzen an der Stelle |p| mindestens ein Stein des Spielers, der nicht
am Zug ist, eingeschlossen (und dann also gedreht) wird.
\end{itemize}

|Pass| ist nur valide, wenn es keine Position |p| gibt, für die |Put p|
valide wäre.

Definieren Sie eine Funktion

< possibleMoves :: GameState -> [Move]

> possibleMoves :: GameState -> [Move]
> possibleMoves (GS c board) = if null moves then [Pass] else moves
>   where moves = [Put (x, y) | x <- ['a'..'h'], y <- [1..8], valid (x, y) c board]

die eine Liste aller in der gegebenen Situation validen Züge berechnet.

\section{Der Spielbaum}

Der Spielbaum wird als RoseTree dargestellt:

> data RTree a = Node a [RTree a] deriving Show
> type GameTree = RTree (GameState,[Move])

|GameTree| ist also ein |RTree|, dessen Label ein Paar aus einem GameState und
einer Liste von Zügen ist. Die Idee ist, mit |gs::GameState| auch |possibleMoves gs|
im Label eines Knotens zu haben; dann kann mit Hilfe einer Funktion

< toGameStates :: GameState -> [Move] -> [GameState]

> toGameStates :: GameState -> [Move] -> [GameState]
> toGameStates _ [] = []
> toGameStates _ (Pass:_) = []
> toGameStates gs moves = catMaybes $ map (\m -> play m gs) moves

eine Liste von möglichen Folgesituation zum Aufbau des Baumes generiert werden.

Implementieren Sie |toGameStates|!

Hinweis: Falls |ms::[Move]| nur Züge enthält, die für |gs::GameState| valide sind,
soll die Ergebnisliste (|gss|) genauso lang sein wie |ms| und für jeden Index
|0 <= n < length ms| soll |gss!!n| die Spielsituation sein, die
aus |gs| entsteht, wenn der Spieler, der an der Reihe ist, den Zug |ms!!n| spielt.

Implementieren Sie eine Funktion

< gameTree :: GameState -> GameTree

> gameTree :: GameState -> GameTree
>   -- create endless GameTree directly and not with generic unfoldRT
> gameTree gs = Node (gs, possibleMoves gs) leafs
>   where leafs = map gameTree $ toGameStates gs $ possibleMoves gs

die aus |gs| den Baum aller möglichen Folge-GameStates berechnet.

Dass eine Othellopartie beendet ist, wenn beide Spieler nacheinander passen (müssen),
soll hierbei nicht abgebildet werden: in jedem Knoten |Node (gs,ms) gts|
eines |gameTree gs| ist |ms| und damit |gts| eine nichtleere Liste!

Hinweis: Implementieren Sie eine ''unfold''-Funktion |unfoldRT| für RoseTrees und schreiben
Sie |gameTree| mit Hilfe von |unfoldRT|!

\section{Bewertung und Auswahl eines besten Zuges}

Eine Othellopartie ist beendet, wenn in einer Brettsituation keiner
der beiden Spieler einen Stein setzen kann. Gewonnen hat der Spieler,
der mehr Steine seiner Farbe auf dem Brett hat. Wir legen (zur Vereinfachung
und abweichend von den üblichen Regeln) fest, dass bei gleicher Anzahl
an Steinen der nachziehende Spieler, d.h. |O|, gewinnt.

Implementieren Sie eine Funktion

< winner :: GameState -> Maybe Color

> winner :: GameState -> Maybe Color
> winner (GS c (Bo b))
>   | null (dropWhile (Pass ==) pm) && o < x = Just X
>   | null (dropWhile (Pass ==) pm) && o >= x = Just O
>   | otherwise = Nothing
>   where pm = possibleMoves (GS c (Bo b)) ++ possibleMoves (GS (rev c) (Bo b))
>         o = length [b (i, y) | i <- ['a'..'h'], y <- [1..8], (b (i,y)) == Just O]
>         x = length [b (i, y) | i <- ['a'..'h'], y <- [1..8], (b (i,y)) == Just X]

die |Just winColor| liefert, falls in der Brettsituation |b| des Arguments
|GS c b| die Partie beendet ist und der Spieler |winColor| gewinnt. Falls
einer der Spieler in Brettsituation |b| noch Zugmöglichkeiten hat, soll
|winner (GS c b) == Nothing| sein.

Beim Tic-Tac-Toe hatten wir zur Auswahl eines besten Zuges den Spielbaum
rekursiv bis zu den ''Blättern'' durchsucht. Der Spielbaum bei Othello ist
sehr viel breiter und tiefer, so dass dies viel zu lange dauern würde. Wir
können hier immer nur eine gewisse (kleine) Anzahl von Zügen ''vorausberechnen''.

Implementieren Sie eine Funktion

< pruneRT :: Int -> RTree a -> RTree a

> pruneRT :: Int -> GameTree -> GameTree
> pruneRT 0 (Node a _) = Node a []
> pruneRT n (Node a xs) = Node a leafs
>   where leafs = map (pruneRT (n-1)) xs

die einen |RTree a| in der gegebenen Tiefe ''abschneidet''. Z.B. soll für

< rt = Node 'a' [Node 'b' [Node 'd' []],Node 'c' [Node 'e' [],Node 'f' []]]

< pruneRT 0 rt = Node 'a' []

und

< pruneRT 1 rt = Node 'a' [Node 'b' [],Node 'c' []]

sein.

Hinweis: Implementieren Sie eine ''fold''-Funktion |foldRT| für RoseTrees
und dann |pruneRT| als |flip pruneRT'| mit

< pruneRT' :: RTree a -> Int -> RTree a
< pruneRT' = foldRT f where
<   f ...

 foldRT :: (a -> [b] -> b) -> RTree a -> b
 foldRT f (Node a nodes) = f a (map (foldRT f) nodes)

für eine geeignete Funktion |f|!

Für die Auswahl eines besten Zuges (bei Vorausberechnungstiefe |n::Int|) verwenden
wir die Variante NegaMax des Minimax-Algorithmus: Wir schneiden den Spielbaum
mittels |pruneRT n| ab, bewerten die Blätter des abgeschnittenen Baums danach, wie
''günstig'' die entsprechende Spielsituation für einen Spieler ist, berechnen
rekursiv und unter der Annahme, dass beide Spieler bestmöglich spielen, als
Bewertung für den (abgeschnittenen) Gesamtbaum den Wert der ''günstigsten''
erreichbaren Spielsituation nach |n| Zügen, und wählen schliesslich aus allen im
Wurzelzustand möglichen Züge diejenigen, die zu einer dieser günstigen Spielsituationen
führen.

Vergleichen Sie auch http://de.wikipedia.org/wiki/Minimax-Algorithmus.

Wir brauchen zunächst ein Mass dafür, wie ''günstig'' eine Spielsituation ist, auch
wenn noch kein Gewinner feststeht. Wir verwenden dazu den Typ

> type Valuation = GameState -> Double

Der Wert von |val::Valuation| auf |GS c b| soll dabei angeben, wie ''günstig''
die Brettsituation |b| für den Spieler |c| ist, wobei wir von ''vernünftigen''
Bewertungen noch folgende Eigenschaften voraussetzen (die, da Haskell keine
abhängigen Typen hat, leider nicht im Typen |Valuation| reflektiert werden können):

\begin{itemize}
\item es ist |-1 <= val gs <= 1| für beliebige |gs::GameState|,
\item |winner (GS c b) == c| genau dann, wenn |val (GS c b) == 1|,
\item es gilt |val (GS X b) = -(val (GS O b))| für beliebige |b::Board|
\end{itemize}

Die dritte Eigenschaft sichert, dass in der Implementation von |negaMax| (siehe unten)
beide Spieler gleich behandelt werden können (vgl. Wikipedia)!

Implementieren Sie eine Bewertungsfunktion

< valOthello :: Valuation

> valOthello :: Valuation
> valOthello (GS c (Bo b)) = (2 * (fromIntegral m) / (fromIntegral tot)) - 1
>   where m = length [b (x,y) | x <- ['a'..'h'], y <- [1..8], (b (x,y)) == Just c]
>         tot = length [b (x,y) | x <- ['a'..'h'], y <- [1..8], isJust (b (x,y))]


die in obigem Sinne ''vernünftig'' ist und für Spielsituationen |GS c b|, in denen
es noch keinen Gewinner gibt, einfach | (2*m/tot) - 1| zurückgibt, wobei |tot| die
Gesamtzahl der gesetzten Spielsteine in |b| und |m| die Anzahl der Steine der
Farbe |c| ist. Hinweis: Verwenden Sie |fromIntegral|, um von |Int| nach |Double| zu
konvertieren.

Implementieren Sie eine Funktion

< negaMax :: Valuation -> GameTree -> Double

> negaMax :: Valuation -> GameTree -> Double
> negaMax val (Node (gs, _) []) = val gs
> negaMax val (Node ((GS c board), _) ts)
>   | abs (val (GS c board)) == 1 = val (GS c board)
>   | otherwise = maximum $ map (negaMax val) ts

sodass für eine ''vernünftige'' Bewertungsfunktion |val| und unter der
Voraussetzung, dass beide Spieler bestmöglich spielen,
|negaMax val gt@(Node ((GS c b),ms) ts)| den für den Spieler |c| (bzgl. |val|)
günstigsten erreichbaren Wert eines GameState in einem Blatt von |gt| berechnet.

Der übergebene GameTree wird in den Anwendungen dann jeweils ein mit
|pruneRT| abgeschnittener Spielbaum sein. Beachten Sie, dass wegen der zweiten
Eigenschaft der Bewertungsfunktion zur Berechnung von |negaMax val (Node (gs,ms) ts)|
im Falle |abs (val gs) == 1| keine Rekusion in die Teilbäume |ts| nötig ist.

Implementieren Sie schliesslich eine Funktion

< bestMoves :: Valuation -> GameTree -> [Move]

> bestMoves :: Valuation -> GameTree -> [Move]
> bestMoves _ (Node (_, (Pass:_)) _) = [Pass]
> bestMoves _ (Node _ []) = [Pass]
> bestMoves val (Node (_, moves) ts) = bmoves
>     where nmax = map (negaMax val) ts
>           indices = findIndices ((maximum nmax) ==) nmax
>           bmoves = map (moves !!) indices

Brettsituation |b| günstigsten Züge aus |ms| auswählt. Auch hier dürfen Sie davon
ausgehen, dass der gegebene Baum durch |pruneRT n (gameTree (GS c b))| entstanden
ist und daher |ms| genau die Liste der in Situation |(GS c b)| möglichen Züge und
|ts| entweder leer oder genau die |ms| entsprechende Liste der Spielbäume der
Folgesituationen ist.

\section{Test Ihrer Implementation}

Wenn Sie alle Funktionen dieses Moduls Othello implementiert haben, können
Sie OthelloCLI.hs oder OthelloGUI.hs benutzen, um zu testen.

OthelloCLI.hs definiert einen Typen

< data PlayerType = Mensch | Maschine Int

und implementiert eine Funktion

< othello :: PlayerType -> PlayerType -> IO ()

|Maschine n| (mit |n >= 1|) verwendet dabei zur Zugauswahl
|bestMoves valOthello ( pruneRT n ( gameTree gs))|, d.h. |n| gibt die
Vorausberechnungstiefe an.

Z.B. können Sie mit

\begin{verbatim}
*OthelloCLI> othello Mensch (Maschine 2)|
\end{verbatim}

als Spieler |X| gegen den Algorithmus mit Tiefe |2| spielen.

OthelloGUI.hs ist ein graphisches Interface. Sie benötigen gtk2hs, gtk3
und glade, die Sie mit

\begin{verbatim}
$ cabal update
$ cabal install gtk3
$ cabal install glade
$ cabal install gtk2hs-cast-glade
\end{verbatim}

installieren können. Ausserdem werden die Dateien
othelloGUI.glade, black.png, white.png und green.png benötigt.
Die Bedienung sollte selbsterklärend sein. Bei Problemen
schreiben Sie bitte eine email.

\section{''Spielregeln'' für das Projekt}

\begin{itemize}
\item Sie dürfen alle Funktionen des Haskell-Preludes und des Moduls
Data.List benutzen.
\item Sie dürfen beliebige Hilfsfunktionen implementieren und
verwenden.
\item Allen implementierten Funktionen soll eine kurze Funktionsbeschreibung
vorangestellt werden. Die Implementierung von Tests (wie in den Übungen) wird empfohlen.
\item Bewertet wird neben der Korrektheit auch die Klarheit/Übersichtlichkeit des
Codes: Kapselung mehrfach benötigter Funktionalität in Hilfsfunktionen, Verwendung von
higher order Funktionen und Rekursionsschemata wird honoriert.
\item Abgabe bis 14.08.2014 per email an tim at cs.uni-potsdam.de und eladrion at cs.uni-potsdam.de
\end{itemize}

\end{document}
