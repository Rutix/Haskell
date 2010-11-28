module Children.Team where

import Children.Child
import SoccerFun.Types
import SoccerFun.Geometry
import SoccerFun.Team
import SoccerFun.Player
import SoccerFun.Field

team :: Home -> Field -> Team
team home field = if home == West then players else mirror field players where

	clubname = "Children" ++ show home
	players  = [child clubname home field (placeOnField pos) nr | (nr,pos) <- zip [1..] playerPositions]

	placeOnField (dx,dy) = Position {px = dx * middleX, py = dy * fwidth field} 
	middleX = flength field / 2.0

	playerPositions =
		[(0.00,0.50),
		 (0.20,0.30),
		 (0.20,0.70),
		 (0.23,0.50),
		 (0.50,0.05),
		 (0.50,0.95),
		 (0.60,0.50),
		 (0.70,0.15),
		 (0.70,0.85),
		 (0.90,0.45),
		 (0.90,0.55)]
