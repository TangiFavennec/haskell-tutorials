{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom
------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Ex 2
dice :: Int -> Rand StdGen [DieValue]
dice n = sequence $ replicate n die

isWinningAttacker :: [DieValue] -> DieValue -> Bool
isWinningAttacker defendersDies attackerDie = (foldl (\x y -> (y < attackerDie) && x) True) defendersDies

updateCasualties :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
updateCasualties bf attackersDies defendersDies = Battlefield remainingAttackers remainingDefenders
   where remainingAttackersCount = length $ filter (isWinningAttacker defendersDies) attackersDies
         remainingAttackers      = min (attackers bf) remainingAttackersCount
         remainingDefenders      = (defenders bf) - min 2 remainingAttackersCount 

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield atk def) = do
   atkRoll <- dice $ min 3 atk
   defRoll <- dice $ min 2 def
   return $ updateCasualties bf atkRoll defRoll

-- Ex 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bt  
   | attackers bt <= 2 || defenders bt == 0 = return bt
   | otherwise                              = (battle bt) >>= invade

-- Ex 4
successProb :: Battlefield -> Rand StdGen Double
successProb bt = do 
   simul <- sequence $ replicateM 1000 invade bt
   return $(fromIntegral (length (filter (\x -> defenders x == 0) simul))) / 1000.0

   
