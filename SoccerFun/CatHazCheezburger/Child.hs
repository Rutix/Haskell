-- Samen gewerkt met Rick de Groot (F100290). We hebben echter niet exact hetzelfde en daarom leveren we het allebei individueel in.
-- Auteur: Rutger Storm [F093490]

-- Alle breinen staan nu in hetzelfde document. Dit heeft vooral als reden dat ik voor alle subTeams dezelfde memory gebruik. 
-- Als ik echter verschillende memories zou hebben gewild dan had ik voor elke subTeam een apart bestandje gemaakt.
-- Functies die meerdere keren voor kwamen zijn niet allemaal opnieuw gecomment.
-- Acties gebaseerd op Corners, detail regels voor voetbal ben ik niet aan toe gekomen.

module CatHazCheezburger.Child where

import SoccerFun.Player
import SoccerFun.Types
import SoccerFun.Geometry
import Control.Monad.State
import SoccerFun.Field
import SoccerFun.Ball
import SoccerFun.RefereeAction


-- Generate a child.
child :: ClubName -> Home -> Field -> Position -> PlayersNumber -> Brain (PlayerAI m) m -> Player
child club home field position no brain = Player
	{playerID = PlayerID {clubName = club, playerNo = no},
	 name     = "child." ++ show no,
	 height   = minHeight,
	 pos      = position,
	 nose     = 0,
	 speed    = 0,
	 skills   = (Running, Kicking, Rotating),
	 effect   = Nothing,
	 stamina  = maxStamina,
	 health   = maxHealth,
	 -- The only thing the child remembers is on which side it belongs to
	 brain    = brain }

-- The child remembers nothing but which side it is playing on.
data Memory = Memory {myHome :: Home, favoritePos :: Position }

-- A stateful computation, with the memory serving as a state.
type Think = State Memory