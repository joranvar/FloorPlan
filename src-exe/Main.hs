import Diagrams.Backend.SVG.CmdLine (mainWith)
import Lib (floorPlan)

main :: IO ()
main = mainWith floorPlan
