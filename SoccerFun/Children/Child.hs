-- | A very simple minded player, that always chases after the ball and kicks the ball towards the opponent's goal.
module Children.Child where

import SoccerFun.Player
import SoccerFun.Types
import SoccerFun.Geometry
import Control.Monad.State
import SoccerFun.Field
import SoccerFun.Ball
import SoccerFun.RefereeAction


-- Generate a child.
child :: ClubName -> Home -> Field -> Position -> PlayersNumber -> Player
child club home field position no = Player
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
	 brain    = Brain {m = Memory {myHome = home}, ai = minibrain field}}

-- The child remembers nothing but which side it is playing on.
data Memory = Memory {myHome :: Home}

-- A stateful computation, with the memory serving as a state.
type Think = State Memory

-- | Based on the perceived surroundings (BrainInput) and the memories, make a decision (BrainOutput) and update the memory.
minibrain :: Field -> PlayerAI Memory
--        = Field -> BrainInput -> Think PlayerAction
minibrain field BrainInput {referee=refereeActions, me=me, ball=ballState, others=others} = do
	mem <- get
	let home = myHome mem
	when (any isEndHalf refereeActions) (put mem {myHome = other home})
	if ballIsClose
		then let goal = centerOfGoal (other home) field in kick goal
		else trackBall $ maxKickReach me
	where
	ball = getBall ballState (me : others) :: Ball
	ballXY = pxy $ ballPos ball :: Position
	ballIsClose = dist (pos me) ballXY < maxKickReach me

	move :: Speed -> Angle -> Think PlayerAction
	move speed angle = return $ Move speed angle

	centerOfGoal :: Home -> Field -> Position
	centerOfGoal home field = Position
		{py = let (n,s) = goalPoles field in (n+s)/2,
		 px = if home == West then 0 else flength field}

	kick :: Position -> Think PlayerAction
	kick point = let
			angle = angleWithObject (pos me) point
			v = 2.0*dist (pos me) point
		in if (dist (pos me) (ballPos ball) <= maxKickReach me)
			then return $ KickBall Speed3D {vxy = Speed {direction=angle,velocity=v},vz=1.0}
			else halt

	trackBall :: Metre -> Think PlayerAction
	trackBall eps = fix ballXY eps

	halt :: Think PlayerAction
	halt = move 0 0

	-- run towards a position
	fix :: Position -> Metre -> Think PlayerAction
	fix point eps = let
			distance = dist            (pos me) point
			angle    = angleWithObject (pos me) point
			v        = max 6.0 distance
		in if (distance <= eps)
			then halt
			else move Speed {direction=angle,velocity=v} (angle - (nose me))
