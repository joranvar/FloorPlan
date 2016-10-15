import Diagrams.Backend.Canvas.CmdLine (mainWith)
import Lib (floorPlan)

main :: IO ()
main = mainWith floorPlan
