module OthelloCLI where
import System.Random (randomRIO)
import Othello (startBoard, GameState (GS), Color (X,O),
  Move (Pass, Put), Board (Bo), winner, bestMoves, valOthello,
  pruneRT, gameTree, play)


type Message = String
data PlayerType = Mensch | Maschine Int deriving (Read,Eq)

instance Show PlayerType where
  show Mensch       = "Mensch"
  show (Maschine n) = "Maschine Tiefe " ++ show n

-- to avoid WinHugs problems:

intersperse :: a -> [a] -> [a]
intersperse x []     = []
intersperse x [y]    = [y]
intersperse x (y:ys) = y:x:intersperse x ys

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)


instance Show Board where
  show (Bo f) =     "\n a   b   c   d   e   f   g   h\n" ++
         intercalate "\n-------------------------------\n" (
          zipWith (++) (map ((" "++). intercalate " | " . (map showMC)) bss)
                     (map (("  "++).show) [8,7..1]))  where
        showMC (Just X) = "X"
        showMC (Just O) = "O"
        showMC Nothing  = " "
        bss = [ [f (x,y) | x <- ['a'..'h'] ] | y <- [8,7..1] ]


othello :: PlayerType -> PlayerType -> IO ()
othello = othello' (GS X startBoard) "" where
  othello' :: GameState -> Message -> PlayerType -> PlayerType -> IO ()
  othello' gs@(GS c b) mess ptthis ptother = do
    print (show b)
    putStrLn mess
    case (winner gs) of
      Just c -> putStrLn $ (show c) ++ " gewinnt."
      Nothing -> case ptthis of
        Mensch -> do
          putStrLn $
            "Bitte Zug für " ++ show c ++ " angeben! \n" ++
            "Formatbeispiel: ''a5'' für Setzen an Position a5\n" ++
            "                ''p''  für Passen"
          answer <- getLine
          let
            maybeMove = case answer of
              []      -> Nothing
              ('p':_) -> Just Pass
              (x:y)   -> case (reads::ReadS Int) y of
                ((yi,_):_) -> Just (Put (x,yi))
                _          -> Nothing
            in case maybeMove of
              Nothing -> again "Konnte die Eingabe nicht parsen...:-("
              Just mo -> doMove mo
        Maschine n -> do
          putStrLn $
            "Spieler " ++ show c ++ " (" ++ show ptthis ++
            ") berechnet einen Zug..."
          let
            best = bestMoves valOthello $ pruneRT n $ gameTree gs
            in do
              maybeMove <- case best of
                [] -> return Nothing
                _  -> randomRIO (0, length best - 1)
                        >>= return . Just . (best !!)
              case maybeMove of
                Nothing -> putStrLn $
                  "Fehler bei der Auswahl eines Zuges: \n" ++
                  "bestMoves gibt leere Liste zurück."
                Just mo -> doMove mo
      where
        again message = othello' gs message ptthis ptother
        doMove mo = case play mo gs of
          Nothing  -> again $ show mo ++ " ist kein gültiger Zug!"
          Just gs' -> othello' gs' "" ptother ptthis

