-- Samen gewerkt met Rick de Groot (F100290). We hebben echter niet exact hetzelfde en daarom leveren we het allebei individueel in.
-- Auteur: Rutger Storm [F093490]

-- Alle breinen staan nu in hetzelfde document. Dit heeft vooral als reden dat ik voor alle subTeams dezelfde memory gebruik. 
-- Als ik echter verschillende memories zou hebben gewild dan had ik voor elke subTeam een apart bestandje gemaakt.
-- Functies die meerdere keren voor kwamen zijn niet allemaal opnieuw gecomment.
-- Acties gebaseerd op Corners, detail regels voor voetbal ben ik niet aan toe gekomen.
module CatHazCheezburger.Spelers where

import Data.Maybe
import SoccerFun.Player
import SoccerFun.Types
import SoccerFun.Geometry
import Control.Monad.State
import SoccerFun.Field
import SoccerFun.Ball
import SoccerFun.Team
import SoccerFun.RefereeAction
import CatHazCheezburger.Child

-- De overloaded gegevens van de Keeper.
keeper :: ClubName -> Home -> Field -> Position -> PlayersNumber -> Brain (PlayerAI m) m -> Player
keeper club home field position no brain = 
    (child club home field position no brain) {
        playerID    = PlayerID {clubName = club, playerNo = no},
        name        = "Edwin van der Sar" ++ show no,
        height      = maxHeight,					
        skills      = (Gaining, Catching, Kicking)
        }

-- De overloaded gegevens van de verdedigers.
defense :: ClubName -> Home -> Field -> Position -> PlayersNumber -> Brain (PlayerAI m) m -> Player
defense club home field position no brain= 
    (child club home field position no brain) {
        playerID = PlayerID {clubName = club, playerNo = no},
        name     = "Defender" ++ show no,
        height   = 1.85,
        skills   = (Dribbling, Gaining, Running)
        }
        
-- De overloaded gegevens van de middenvelders.        
mid :: ClubName -> Home -> Field -> Position -> PlayersNumber -> Brain (PlayerAI m) m -> Player
mid club home field position no brain = 
    (child club home field position no brain) {
        playerID = PlayerID {clubName = club, playerNo = no},
        name = "Midfielder" ++ show no,
        height   = 1.70,
        skills = (Gaining, Dribbling, Running)
        }       
        
-- De overloaded gegevens van de aanvallers.
spits :: ClubName -> Home -> Field -> Position -> PlayersNumber -> Brain (PlayerAI m) m -> Player
spits club home field position no brain= 
    (child club home field position no brain) {
        playerID = PlayerID {clubName = club, playerNo = no},
        name = "Spits" ++ show no,
        height   = 1.65,
        skills = (Dribbling, Running, Kicking)
        }

-- | Based on the perceived surroundings (BrainInput) and the memories, make a decision (BrainOutput) and update the memory.
keeperbrain :: Field -> PlayerAI Memory
--        = Field -> BrainInput -> Think PlayerAction
keeperbrain field BrainInput {referee=refereeActions, me=me, ball=ballState, others=others} = do
    mem <- get
    let home = myHome mem
    let favpos = favoritePos mem
    -- Wanneer de helften worden omgekeerd. Verander dan de home en mirror de favoriete posities.
    when (any isEndHalf refereeActions) (put mem {myHome = other home , favoritePos = mirror field favpos})
    -- Er word eerst gekeken of je de bal aan je voeten of in je handen hebt. Zo ja dan word hij weggeschoten naar next_pos. Zo nee kijk dan of iemand de bal heeft.
    -- Heeft niemand de bal aan zijn voeten en ben jij de meest dichtbijzijnde speler. Catch de bal dan als hij in de lucht zit of gain de bal als hij niet in de lucht zit.
    if  gainedBall || succeedCatch
        then kick next_pos
        else if ballIsFree ballState && closest
                then if ballInAir
                        then if ballIsCatchRange
                                then catch
                                else halt
                        else if ballIsGainReachRange
                                then gain
                                else ren ballXY $ maxGainReach me
                else ren favpos $ 1.0
    where
    ball = getBall ballState (me : others) :: Ball
    ballXY = pxy $ ballPos ball :: Position
    ballPZ = pz $ ballPos ball :: ZPos
    ballDist = dist (pos me) ballXY
    ballIsGainReachRange = ballDist < maxGainReach me
    ballInAir = ballPZ > 3.0
    
    possessBall = ballIsGainedBy (playerID me) ballState
    succeedCatch = succeedCaughtBall (getPlayerEffect (effect me))
    ballIsCatchRange = ballDist < maxCatchReach me
    
    -- Bepaal naar wie je de bal gaat schoppen door de volgende speler te kiezen gebaseer op playerNummer.
    teamPlayers = filter (sameClub me) others
    myNr = playerNo (playerID me)
    next_nr = ((myNr-1) `rem` (length teamPlayers)) + 2
    next_player = head . filter (identifyPlayer (playerID me){playerNo=next_nr}) $ teamPlayers
    next_pos = pos next_player
    closest = ballDist <= minimum [ dist (getPosition fp) ballXY | fp <- filter (sameClub me) others ]

    -- Standaard move functie
    move :: Speed -> Angle -> Think PlayerAction
    move speed angle = return $ Move speed angle

    -- Bepaald de Positie van de het midden van de goal.
    centerOfGoal :: Home -> Field -> Position
    centerOfGoal home field = Position
        {py = let (n,s) = goalPoles field in (n+s)/2,
         px = if home == West then 0 else flength field}

    -- Zet de Maybe PlayerEffect om in PlayerEffect
    getPlayerEffect :: Maybe PlayerEffect -> PlayerEffect
    getPlayerEffect effect = fromMaybe (CaughtBall Fail) effect
    
    -- Gaat True als de bal success was gevangen en anders geeft het False
    succeedCaughtBall :: PlayerEffect -> Bool
    succeedCaughtBall (CaughtBall Success) = True
    succeedCaughtBall (CaughtBall Fail) = False
    succeedCaughtBall _ = False
    
    -- Catch functie
    catch :: Think PlayerAction
    catch = if (dist (pos me) (ballPos ball) <= maxCatchReach me)
            then return $ CatchBall
            else halt
    
    gain :: Think PlayerAction
    gain = if (dist (pos me) (ballPos ball) <= maxGainReach me)
              then return $ GainBall
              else halt
              
    -- Standaard kick Functie
    kick :: Position -> Think PlayerAction
    kick point = let
            angle = angleWithObject (pos me) point
            v = 2.0*dist (pos me) point
        in if (dist (pos me) (ballPos ball) <= maxKickReach me)
            then return $ KickBall Speed3D {vxy = Speed {direction=angle,velocity=v},vz=9.0}
            else halt

    -- Volg de bal.
    trackBall :: Metre -> Think PlayerAction
    trackBall eps = fix ballXY eps

    halt :: Think PlayerAction
    halt = move 0 0
    
    -- Functie om naar de positie te rennen. De functie fix houd geen rekening mee met de kijkrichting en dat doet deze wel.
    ren :: Position -> Metre -> Think PlayerAction
    ren point eps = let
            distance = dist            (pos me) point
            hoek     = angleWithObjectForRun (pos me, nose me) point
            v        = max 6.0 distance
        in if (distance <= eps)
            then halt
            else move Speed {direction=(nose me)+hoek,velocity=v} hoek

    -- run towards a position
    fix :: Position -> Metre -> Think PlayerAction
    fix point eps = let
            distance = dist            (pos me) point
            angle    = angleWithObject (pos me) point
            v        = max 6.0 distance
        in if (distance <= eps)
            then halt
            else move Speed {direction=angle,velocity=v} (angle - (nose me))

-- DEFENDERS
defensebrain :: Field -> PlayerAI Memory            
defensebrain field BrainInput {referee=refereeActions, me=me, ball=ballState, others=others} = do
    mem <- get
    let home = myHome mem
    let favpos = favoritePos mem
    when (any isEndHalf refereeActions) (put mem {myHome = other home, favoritePos = mirror field favpos})
    if gainedBall
        -- Geef de goal positie mee aan de functie shootBall
        then let goal = centerOfGoal (other home) field in shootBall goal
        else if ballIsClose && closest
                then if gainRange
                        then gain
                        else trackBall $ maxGainReach me
        else ren favpos $ 1.0 -- Blijf op je favoriete plek als je niet in de buurt van de bal bent.
    where
    ball = getBall ballState (me : others) :: Ball
    ballXY = pxy $ ballPos ball :: Position
    ballDist = dist (pos me) ballXY
    ballIsClose = ballDist < 8.0
    ballKickRange = ballDist < maxKickReach me
    gainRange = ballDist < maxGainReach me
    closest = ballDist < minimum [ dist (getPosition fp) ballXY | fp <- filter (sameClub me) others ]
    gainedBall = ballIsGainedBy (playerID me) ballState
    
    
    -- Functie om te bepalen naar wie je moet schieten.
    shootBall goalPoint = actie'
        where   actie' = if bestPosition
                            then if nearGoal
                                    then kick goalPoint
                                    else ren goalPoint $ 1.0
                            else kick best
                bestPosition = null better
                better = [ getPosition p | p <- teamPlayers, dist (pos p) goalPoint < d_goal ] -- Krijg een lijst met alle posities van de spelers die dichterbij de goal zijn dan jou.
                best = head better
                d_goal = dist (pos me) goalPoint
                nearGoal = d_goal < 20.0
                teamPlayers = filter (sameClub me) others 

    -- Standaard move functie    
    move :: Speed -> Angle -> Think PlayerAction
    move speed angle = return $ Move speed angle
 
    -- Bepaald de Positie van de het midden van de goal.
    centerOfGoal :: Home -> Field -> Position
    centerOfGoal home field = Position
        {py = let (n,s) = goalPoles field in (n+s)/2,
        px = if home == West then 0 else flength field}
 
    -- Functie voor gain.
    gain :: Think PlayerAction
    gain = if (dist (pos me) (ballPos ball) <= maxGainReach me)
              then return $ GainBall
              else halt
    
    {-
    Incomplete functie voor koppen.
    kop :: Position -> Think PlayerAction
    kop point = let
            angle = angleWithObject (pos me) point
            v = 2.0*dist (pos me) point
        in if (dist (pos me) (ballPos ball) <= maxKickReach me)
            then return $ KickBall Speed3D {vxy = Speed {direction=angle,velocity=v},vz=1.0}
            else halt
    -}
    
    -- standaard kick functie.
    kick :: Position -> Think PlayerAction
    kick point = let
            angle = angleWithObject (pos me) point
            v = 2.0*dist (pos me) point
        in if (dist (pos me) (ballPos ball) <= maxKickReach me)
            then return $ KickBall Speed3D {vxy = Speed {direction=angle,velocity=v},vz=1.0}
            else halt
 
    trackBall :: Metre -> Think PlayerAction
    trackBall eps = fix ballXY eps
 
    -- stilstaan.
    halt :: Think PlayerAction
    halt = move 0 0
    
    ren :: Position -> Metre -> Think PlayerAction
    ren point eps = let
            distance = dist            (pos me) point
            --angle    = angleWithObject (pos me) point
            hoek     = angleWithObjectForRun (pos me, nose me) point
            v        = max 6.0 distance
        in if (distance <= eps)
            then halt
            else move Speed {direction=(nose me)+hoek,velocity=v} hoek
    
    -- run towards a position
    fix :: Position -> Metre -> Think PlayerAction
    fix point eps = let
            distance = dist            (pos me) point
            angle    = angleWithObject (pos me) point
            v        = max 6.0 distance
        in if (distance <= eps)
            then halt
            else move Speed {direction=angle,velocity=v} (angle - (nose me))
            
midbrain :: Field -> PlayerAI Memory            
midbrain field BrainInput {referee=refereeActions, me=me, ball=ballState, others=others} = do
    mem <- get
    let home = myHome mem
    let favpos = favoritePos mem
    when (any isEndHalf refereeActions) (put mem {myHome = other home, favoritePos = mirror field favpos})
    if ballIsClose
        then if ballKickRange
            then let goal = centerOfGoal (other home) field in shootBall goal
            else trackBall $ maxKickReach me
        else ren favpos $ 1.0
    where
    ball = getBall ballState (me : others) :: Ball
    ballXY = pxy $ ballPos ball :: Position
    ballDist = dist (pos me) ballXY
    ballIsClose = ballDist < 9.0
    ballKickRange = ballDist < maxKickReach me
 
    shootBall goalPoint = actie'
        where   actie' = if bestPosition
                            then if nearGoal
                                    then kick goalPoint
                                    else ren goalPoint $ 1.0
                            else kick best
                bestPosition = null better
                better = [ getPosition p | p <- players, dist (pos p) goalPoint < d_goal ]
                best = head better
                d_goal = dist (pos me) goalPoint
                nearGoal = d_goal < 20.0
                players = filter (sameClub me) others
                
    move :: Speed -> Angle -> Think PlayerAction
    move speed angle = return $ Move speed angle
 
    centerOfGoal :: Home -> Field -> Position
    centerOfGoal home field = Position
        {py = let (n,s) = goalPoles field in (n+s)/2,
        px = if home == West then 0 else flength field}
 
    kick :: Position -> Think PlayerAction
    kick point = let
            angle = angleWithObject (pos me) point
            v = 3.0*dist (pos me) point
        in if (dist (pos me) (ballPos ball) <= maxKickReach me)
            then return $ KickBall Speed3D {vxy = Speed {direction=angle,velocity=v},vz=1.0}
            else halt
 
    trackBall :: Metre -> Think PlayerAction
    trackBall eps = fix ballXY eps
 
    halt :: Think PlayerAction
    halt = move 0 0
    
    ren :: Position -> Metre -> Think PlayerAction
    ren point eps = let
            distance = dist            (pos me) point
            --angle    = angleWithObject (pos me) point
            hoek     = angleWithObjectForRun (pos me, nose me) point
            v        = max 6.0 distance
        in if (distance <= eps)
            then halt
            else move Speed {direction=(nose me)+hoek,velocity=v} hoek
    
    -- run towards a position
    fix :: Position -> Metre -> Think PlayerAction
    fix point eps = let
            distance = dist            (pos me) point
            angle    = angleWithObject (pos me) point
            v        = max 6.0 distance
        in if (distance <= eps)
            then halt
            else move Speed {direction=angle,velocity=v} (angle - (nose me))

spitsbrain :: Field -> PlayerAI Memory
spitsbrain field BrainInput {referee=refereeActions, me=me, ball=ballState, others=others} = do
    mem <- get
    let home = myHome mem
    let favpos = favoritePos mem
    when (any isEndHalf refereeActions) (put mem {myHome = other home, favoritePos = mirror field favpos})
    if ballIsClose
        then if ballKickRange
            then let goal = centerOfGoal (other home) field in shootBall goal
            else trackBall $ maxKickReach me
        else ren favpos $ 1.0
    where
    ball = getBall ballState (me : others) :: Ball
    ballXY = pxy $ ballPos ball :: Position
    ballDist = dist (pos me) ballXY
    ballIsClose = ballDist < 10.0
    ballKickRange = ballDist < maxKickReach me
    
    shootBall goalPoint = actie'
        where   actie' = if bestPosition
                            then if nearGoal
                                    then kick goalPoint
                                    else ren goalPoint $ 10.0
                            else kick best
                bestPosition = null better
                better = [ getPosition p | p <- players, dist (pos p) goalPoint < d_goal ]
                best = head better
                d_goal = dist (pos me) goalPoint
                nearGoal = d_goal < 20.0
                players = filter (sameClub me) others

    move :: Speed -> Angle -> Think PlayerAction
    move speed angle = return $ Move speed angle

    centerOfGoal :: Home -> Field -> Position
    centerOfGoal home field = Position
        {py = let (n,s) = goalPoles field in (n+s)/2,
        px = if home == West then 0 else flength field}

    kick :: Position -> Think PlayerAction
    kick point = let
            angle = angleWithObject (pos me) point
            v = 4.0*dist (pos me) point
        in if (dist (pos me) (ballPos ball) <= maxKickReach me)
            then return $ KickBall Speed3D {vxy = Speed {direction=angle,velocity=v},vz=5.0}
            else halt
                        
    trackBall :: Metre -> Think PlayerAction
    trackBall eps = fix ballXY eps

    halt :: Think PlayerAction
    halt = move 0 0

    ren :: Position -> Metre -> Think PlayerAction
    ren point eps = let
            distance = dist            (pos me) point
            hoek     = angleWithObjectForRun (pos me, nose me) point
            v        = max 6.0 distance
        in if (distance <= eps)
            then halt
            else move Speed {direction=(nose me)+hoek,velocity=v} hoek -- (angle - (nose me))
            
    -- run towards a position
    fix :: Position -> Metre -> Think PlayerAction
    fix point eps = let
            distance = dist            (pos me) point
            angle    = angleWithObject (pos me) point
            v        = max 6.0 distance
        in if (distance <= eps)
            then halt
            else move Speed {direction=angle,velocity=v} (angle - (nose me))