module Tests where
import Test.Hspec

import Parser
import Text.Parsec
import Text.Parsec.String
import MainLoop

main :: IO()
main = hspec $ do
	it "loads some moves, parses, saves and reloads" $ do
		let mvs =  "  (1,0) ->  (2,0); (2,0) ->  (3,0); (11,3) -> (2,0); (1,0)->  (2,0); (2,0) ->  (27,1);   x "
		let  parsedMvs = parse movesParser "Parsing Moves" mvs
		case parsedMvs of
			Left msg ->  print msg
			Right  v ->  do
				saveMoves  "testMoves.txt" v
				mvs <- loadMoves "testMoves.txt"
				print mvs



