module Main where
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random (randomRIO)
import Othello (startBoard, GameState (GS), Color (X,O), 
  Move (Pass, Put), Board (Bo), winner, bestMoves, valOthello, 
  pruneRT, gameTree, play)

type Message = String
data PlayerType = Mensch | Maschine Int deriving (Read,Eq)

instance Show PlayerType where
  show Mensch       = "Mensch"
  show (Maschine n) = "Maschine Tiefe " ++ (show n) 

data GUI = GUI {
  wMain    :: Window,
  wNewGame :: Window,
  bQuit    :: Button,
  bNewGame :: Button,
  bQuitNew :: Button,
  bOkNew   :: Button,
  bPass    :: Button,
  chooseB  :: ComboBox,
  chooseW  :: ComboBox,
  lTypeB   :: Label,
  lTypeW   :: Label,
  lStateB  :: Label,
  lStateW  :: Label,
  lMisc    :: Label,
  images   :: [Image],
  boxes    :: [EventBox],
  pixbufs  :: Maybe Othello.Color -> Pixbuf
}

data MVars = MV {
  gameState     :: MVar GameState,
  pType         :: MVar (Othello.Color -> PlayerType),
  doMachineMove :: MVar Bool,             
  doPaint       :: MVar String,
  paintThread   :: MVar ThreadId,
  machineThread :: MVar ThreadId
}

getGUIfromGlade :: IO GUI
getGUIfromGlade = do
  builder <- builderNew
  builderAddFromFile builder "othelloGUI.glade"
  
  [wMain, wNewGame]  <- mapM (builderGetObject builder castToWindow) 
      ["mainWindow", "newGameDialog"]

  [bQuit, bNewGame, bQuitNew, bOkNew, bPass] <- mapM (builderGetObject builder castToButton) 
      ["quit", "newGame", "quitNew", "okNew", "pass"]

  [chooseB, chooseW] <- mapM (builderGetObject builder castToComboBox) 
      ["chooseBlack", "chooseWhite"]

  [lTypeB, lTypeW, lStateB, lStateW, lMisc] <- mapM (builderGetObject builder castToLabel) 
      ["labelTypeBlack", "labelTypeWhite", "labelStateBlack", "labelStateWhite", "labelMisc"]

  images <- mapM ((builderGetObject builder castToImage).("image"++).show) [1..64]

  boxes  <- mapM ((builderGetObject builder castToEventBox).("eventbox"++).show) [1..64]

  bPB <- pixbufNewFromFile "black.png"
  wPB <- pixbufNewFromFile "white.png"
  gPB <- pixbufNewFromFile "green.png"

  let 
    pbs Nothing   = gPB
    pbs (Just X)  = bPB
    pbs (Just O)  = wPB
    in return $ GUI wMain wNewGame bQuit bNewGame bQuitNew bOkNew bPass chooseB chooseW 
          lTypeB lTypeW lStateB lStateW lMisc images boxes pbs
   
-- the positions of [Image1,Image2,...] in the GUI
imagePoss = [(x,y)| y <- [8,7..1], x <- ['a'..'h'] ]

-- updating the Board to a GameState
paintGS :: MVars -> GUI -> IO ()
paintGS mvs gui = do
  message <- takeMVar (doPaint mvs)                      -- wait for ok
  postGUISync $ labelSetText (lMisc gui) message         -- message to lMisc
  gs@(GS p (Bo b)) <- takeMVar (gameState mvs)           -- take gs
  thePType         <- readMVar (pType mvs)               -- read pType
  postGUISync $ labelSetText (lStateB gui) ""            -- empty state labels
  postGUISync $ labelSetText (lStateW gui) ""
  postGUISync $ sequence_ 
    ( zipWith f (map b imagePoss) (images gui))          -- update board images
  case fmap cToL (winner gs) of                          -- update labels:
    Nothing -> do                                        -- whos turn?, who wins?
      case (thePType p) of
        Mensch -> do 
          postGUISync $ labelSetText (cToL p) "am Zug ..."
        _     -> do
          putMVar (doMachineMove mvs) True
          postGUISync $ labelSetText (cToL p) "Zug wird berechnet ..."
    Just l  -> do
      postGUISync $ labelSetText l "gewinnt"
  putMVar (gameState mvs) gs                             -- work done, put gs back
  paintGS mvs gui                                        -- recurse to handle next paints
  where
    cToL X = lStateB gui
    cToL O = lStateW gui
    f mc im = imageSetFromPixbuf im ((pixbufs gui) mc)

tryMove :: Move -> GameState -> (GameState,Message)      -- play maps to Maybe GameState,
tryMove m gs =                                           -- we need a GameState also in case
  case play m gs of                                      -- move was invalid, then with info
    Nothing  -> case m of                                -- on the problem...
      Pass   -> (gs,"Passen nicht erlaubt")
      Put _  -> (gs,"Ungültiger Zug")
    Just gs' -> (gs',"")

humanMove :: Move -> MVars -> GUI -> IO ()
humanMove m mvs gui = do
  gs@(GS c b) <- takeMVar (gameState mvs)                -- take gs
  pType       <- readMVar (pType mvs)
  if pType c /= Mensch
  then putMVar (gameState mvs) gs                        -- not a humans turn -> just put it back
  else let 
    (gs',message) = tryMove m gs
    in do
      putMVar (gameState mvs) gs'                        -- put the new gs
      putMVar (doPaint mvs) message                      -- trigger repaint
      return ()

machineMove :: MVars -> GUI -> IO ()
machineMove mvs gui = do
  takeMVar (doMachineMove mvs)                           -- wait for ok
  gs@(GS c b) <- takeMVar (gameState mvs)                -- take gs
  pType       <- readMVar (pType mvs)
  case pType c of
    Mensch       -> putMVar (gameState mvs) gs            -- not a Machine move -> just put it back
    (Maschine n) -> let                                   -- otherwise
      bms = bestMoves valOthello (pruneRT n (gameTree gs))  -- get list of bestMoves
      in do 
        x <- randomRIO $! (0, (length $! bms) - 1)       
        m <- return $! (bms !! x)                        -- choose one
        let 
          (gs',message) = tryMove m gs                   -- and do it
          in do
            putMVar (gameState mvs) gs'                  -- put result as new gs
            putMVar (doPaint mvs) message                -- trigger repaint
  machineMove mvs gui                                    -- recurse to handle next moves

startNewGame :: MVars -> GUI -> IO ()
startNewGame mvs gui = do
  let
    toPT n = if (n == 0) then Mensch else Maschine n
    in do
      pTh <- tryTakeMVar (paintThread mvs)               -- kill paint and machineMove threads
      case pTh of                                        -- if any
        Just pThId -> killThread pThId
        _ -> return ()
      mTh <- tryTakeMVar (machineThread mvs)
      case mTh of
        Just mThId -> killThread mThId
        _ -> return ()

      [nB,nW] <- mapM comboBoxGetActive                  -- get chosen player Types
        [(chooseB gui),(chooseW gui)]
      labelSetText (lTypeB gui) $ show (toPT nB)         -- update labels
      labelSetText (lTypeW gui) $ show (toPT nW)
      tryTakeMVar (pType mvs)
      let 
        f X = toPT nB
        f O = toPT nW
        in putMVar (pType mvs) f                         -- put them to MVars

      widgetHide (wNewGame gui)                          -- hide dialog
      
      tryTakeMVar (gameState mvs)                        -- empty MVars
      tryTakeMVar (doMachineMove mvs)
      tryTakeMVar (doPaint mvs)
      

      (forkIO (paintGS mvs gui))                         -- fork threads, put Ids
        >>= putMVar (paintThread mvs) 
      (forkIO (machineMove mvs gui)) 
        >>= putMVar (machineThread mvs)

      putMVar (gameState mvs) (GS X startBoard)          -- put start board into gs
      putMVar (doPaint mvs) ""                           -- trigger paint
      return ()
          
main = do 
  initGUI 
  gui <- getGUIfromGlade

  wMain gui `on` deleteEvent $                           -- delete event for mainWindow
    liftIO mainQuit >> return False

  bQuit gui `on` buttonReleaseEvent $ do                 -- quitButton exits app
    liftIO $ do 
      widgetHide (wMain gui)
      mainQuit
    return False
 
  bNewGame gui `on` buttonPressEvent $                   -- newGame Button just shows the dialog
    liftIO (widgetShowAll (wNewGame gui)) >> return False
  
  bQuitNew gui `on` buttonPressEvent $                   -- quitButton on dialog hides it
    liftIO (widgetHide (wNewGame gui)) >> return False

  gs        <- newEmptyMVar
  pT        <- newEmptyMVar
  doM       <- newEmptyMVar
  doP       <- newEmptyMVar
  pTh       <- newEmptyMVar
  mTh       <- newEmptyMVar
  mvs       <- return $ MV gs pT doM doP pTh mTh         -- initialize global MVars

  bOkNew gui `on`buttonPressEvent $ do                   -- okButton on dialog starts new game
    liftIO $ startNewGame mvs gui
    return False

  let                                                    -- click actions for boardFields
    action p = do
      liftIO $ humanMove (Put p) mvs gui
      return True
    in sequence_ $ zipWith (\b a -> (b `on` buttonPressEvent) a) 
      (boxes gui) (map action imagePoss)

  bPass gui `on` buttonPressEvent $                      -- and for the pass button 
    liftIO (humanMove Pass mvs gui) >> return False

  
  startNewGame mvs gui                                   -- start first game
  widgetShowAll (wMain gui)                              -- show main Window
  mainGUI                                                -- enter GUI mainLoop


