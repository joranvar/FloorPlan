import Diagrams.Backend.Canvas.CmdLine (mainWith)
import Lib

main :: IO ()
main = mainWith . render $ Room 10 10
