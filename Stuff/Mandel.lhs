Wir schreiben uns eine d mandelbrotmenge.

> module Stuff.Mandel where
> import Graphics.Rendering.OpenGL
> import Graphics.UI.GLUT
> import Data.IORef
>
> data Complex = C Float Float deriving (Show, Eq)
>
> instance Num Complex where
>   fromInteger n = C (fromInteger n, 0.0)
>   (C x y) + (C z t) = C (x+z) (y+t)
>   (C x y) * (C z t) = C (x * z - y * t) (x * z + y * t)
>   abs (C x y) = C (sqrt (x*x + y*y)) 0.0
>   signum (C x y) = C (signum x) (0.0)

Einige nützliche Funktionen

> complex :: Float -> Float -> Complex
> complex x y = C x y
>
> real :: Complex -> Float
> real (C x y) = x
>
> im :: Complex -> Float
> im (C x y) = y
>
> magnitude :: Complex -> Float
> magnitude = real.abs


Hauptprogramm

> main :: IO()
> main = do
>   -- GLUT needs to be initialized
>   (progname,_) <- getArgsAndInitialize
>   -- We use the double buffered mode (GL constraint)
>   initialDisplayMode $= [DoubleBuffered]
>   --create window with title
>   createWindow "Mandelbrötchen mit Haskellchen und OpenGlchen"
>   -- each time we need to update the display
>   -- so we call the function 'display'
>   displayCallback $= display
>   mainLoop


> display = do
>   clear [ColorBuffer] -- window gets black
>   loadIdentity -- reset any transformation
>   preservingMatrix drawMandelbrot
>   swapBuffers -- refresh screen
