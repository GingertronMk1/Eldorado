import Graphics.UI.GLUT

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = let kmax = 100 in [ (0.5*sin (2*pi*k/kmax), 0.5*cos (2*pi*k/kmax), 0) | k <- [1..kmax] ]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  renderPrimitive LineLoop $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush
