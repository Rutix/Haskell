-- Samen gewerkt met Rick de Groot (F100290). We hebben echter niet exact hetzelfde en daarom leveren we het allebei individueel in.
-- Auteur: Rutger Storm [F093490]

-- Alle breinen staan nu in hetzelfde document. Dit heeft vooral als reden dat ik voor alle subTeams dezelfde memory gebruik. 
-- Als ik echter verschillende memories zou hebben gewild dan had ik voor elke subTeam een apart bestandje gemaakt.
-- Functies die meerdere keren voor kwamen zijn niet allemaal opnieuw gecomment.
-- Acties gebaseerd op Corners, detail regels voor voetbal ben ik niet aan toe gekomen.

module CatHazCheezburger.Team where

import CatHazCheezburger.Child
import CatHazCheezburger.Spelers
import SoccerFun.Types
import SoccerFun.Geometry
import SoccerFun.Team
import SoccerFun.Player
import SoccerFun.Field

-- Voor de bonuspunt van een hack: Je kunt cheaten door meer dan 11 spelers op het veld te zetten. isValidTeam kijkt namelijk niet naar de aantal spelers maar alleen of ze unique nummers hebben.
-- Ik heb het getest met 55 spelers en dat werkte ook. 
team :: Home -> Field -> Team
team home field = if home == West then players else mirror field players where
    subTeam makePlayer positions = [makePlayer clubname home field (placeOnField pos) nr (Brain {m= Memory {myHome = home, favoritePos = mkFavoritePositions favpos}, ai = (brainGenerate nr) field}) | (nr,pos,favpos) <- positions]
    clubname = "Cat Haz Cheezburger" ++ show home
    players = concat [(subTeam keeper $ zip3 [1] playerKeepPositions playerFavKeepPositions),
              (subTeam defense $ zip3 [2..4] playerDefPositions playerFavDefPositions),
              (subTeam mid $ zip3 [5..8] playerMidPositions playerFavMidPositions),
              (subTeam spits $ zip3 [9..11] playerSpitsPositions playerFavSpitsPositions)]
    
    -- Deze functie hangt een rbrain aan bepaalde speler nummers.
    brainGenerate nr 
        | nr == 1 = keeperbrain
        | nr > 1 && nr < 5 = defensebrain
        | nr > 4 && nr < 9 = midbrain
        | otherwise = spitsbrain
    
    -- Mirroring van de favoriete posities.
    mkFavoritePositions favpos = if home == West then placeOnField favpos else mirror field $ placeOnField favpos
    
    placeOnField (dx,dy) = Position {px = dx * middleX, py = dy * fwidth field} 
    middleX = flength field  / 2.0

    -- Start posities.
    playerKeepPositions =
        [(0.02,0.50)]
    
    playerFavKeepPositions =
        [(0.02,0.50)]
       
    playerDefPositions =
        [(0.25,0.30),
        (0.25,0.50),
        (0.25,0.70)]
    
    playerMidPositions =
        [(0.50,0.60),
        (0.50,0.40),
        (0.50,0.85),
        (0.50,0.15)]
        
    playerSpitsPositions =
        [(0.90,0.45),
        (0.90,0.55),
        (0.75,0.50)]
    
    -- De posities die waar ze in game zullen staan als ze geen bal bezit hebben.
    playerFavDefPositions =
        [(0.25,0.30),
        (0.25,0.50),
        (0.25,0.70)]
        
    playerFavMidPositions =
        [(0.50,0.60),
        (0.50,0.40),
        (0.50,0.85),
        (0.50,0.15)]
        
    playerFavSpitsPositions =
        [(1.00,0.35),
        (1.00,0.65),
        (1.60,0.55)]

