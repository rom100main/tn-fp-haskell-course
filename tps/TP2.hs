-- Build me with: cabal build TP2.hs
-- Execute me with: cabal run -v0 TP2.hs
-- Load me in the REPL with: cabal repl TP2.hs, then use :r to reload the code upon changing

{- HLINT ignore -}

module Main where

import Debug.Trace
import GHC.Generics
import Generic.Random
import Test.QuickCheck

-- If you don't like this TP, you can do TP3 instead.

-- Implement a card game a la Magic the Gathering. Each player
-- has 3 spots available to play cards like this:
--
--       Player 1
--
--  card1   card2   card3
--
--
--  carda   cardb   cardc
--
--       Player2
--
-- Each player starts with a deck of cards. When it is the turn of a
-- player, he can take cards from his hand and put them on the board.
-- After that, the turn resolves: each card of the player attacks the
-- card in front of it. If a player has no card in front of the attacking
-- card, then the attack contributes to the player's score. Each card
-- has two stats: its hitpoints and its attack. When a card attacks, it
-- deals the corresponding hitpoint to the opponent card.
-- Consider the diagram below:
--
--      Player 1
--
-- knight   empty   soldier
--
-- empty    soldier soldier
--
--      Player 2
--
-- with the knight having 2 hitpoints and 2 attacks, and the soldier having 1 hitpoint
-- and 1 attack. In this scenario, when player 1 attacks, the left knight
-- contributes two to the score while the right soldier kills its opponent (there
-- are only two cards: knight and soldier)
--
-- Exercise:
--
-- 1. Define a type for cards
--    - Define a Show instance for this type
-- 2. Define a type for the part of a player. Make it implement Show.
-- 3. Define a type for the whole board: the two players parts. Make it implement Show.
-- 4. Write a function making a player play
-- 5. Write a function playing an entire game
--    Display the list of boards while the game runs.
--
-- If you want to have randomness, use the 'random' function here:
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html#v:random
-- Use mkStdGen to obtain a value that satisfies the constraint "RandomGen g":
-- https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html#t:StdGen
--
-- Use https://hoogle.haskell.org/ to find the functions you need

data Error
  = InvalidCardIndex
  | BoardPositionOccupied
  | HandEmpty
  | DeckEmpty
  deriving (Bounded, Enum, Eq, Show)

data Card = MkCard
  { hitpoints :: Int,
    attack :: Int
  }
  deriving (Eq, Show)

data Board = MkBoard
  { card1 :: Maybe Card,
    card2 :: Maybe Card,
    card3 :: Maybe Card
  }
  deriving (Eq, Show)

data BoardPosition = Pos1 | Pos2 | Pos3
  deriving (Bounded, Enum, Eq, Show)

data Player = MkPlayer
  { health :: Int,
    hand :: [Card],
    board :: Board,
    deck :: [Card]
  }
  deriving (Eq, Show)

data Game = MkGame
  { player1 :: Player,
    player2 :: Player
  }
  deriving (Eq, Show)

soldierCard :: Card
soldierCard = MkCard {hitpoints = 1, attack = 1}

knightCard :: Card
knightCard = MkCard {hitpoints = 2, attack = 2}

attackCard :: Card -> Card -> (Maybe Card, Maybe Card, Int)
attackCard attacker defender =
  let newDefenderHp = defender.hitpoints - attacker.attack
      newAttackerHp = attacker.hitpoints - defender.attack
      score = if newDefenderHp <= 0 then attack attacker else 0
      newDefender = if newDefenderHp <= 0 then Nothing else Just (defender {hitpoints = newDefenderHp})
      newAttacker = if newAttackerHp <= 0 then Nothing else Just (attacker {hitpoints = newAttackerHp})
   in (newAttacker, newDefender, score)

attackMaybeCard :: Maybe Card -> Maybe Card -> (Maybe Card, Maybe Card, Int)
attackMaybeCard (Just atk) (Just def) = attackCard atk def
attackMaybeCard (Just atk) Nothing = (Just atk, Nothing, atk.hitpoints)
attackMaybeCard Nothing (Just def) = (Nothing, Just def, 0)
attackMaybeCard Nothing Nothing = (Nothing, Nothing, 0)

attackBoard :: Board -> Board -> (Board, Board, Int)
attackBoard (MkBoard a1 a2 a3) (MkBoard d1 d2 d3) =
  let (newA1, newD1, score1) = attackMaybeCard a1 d1
      (newA2, newD2, score2) = attackMaybeCard a2 d2
      (newA3, newD3, score3) = attackMaybeCard a3 d3
      totalScore = score1 + score2 + score3
   in (MkBoard newA1 newA2 newA3, MkBoard newD1 newD2 newD3, totalScore)

attackPlayer :: Player -> Player -> (Player, Player)
attackPlayer attacker defender =
  let (newAttackerBoard, newDefenderBoard, score) = attackBoard attacker.board defender.board
      newDefenderHp = defender.health - score
   in (attacker {board = newAttackerBoard}, defender {board = newDefenderBoard, health = newDefenderHp})

drawCard :: Player -> Either Error Player
drawCard player = case player.deck of
  [] -> Left DeckEmpty
  (c : cs) -> Right (player {hand = c : player.hand, deck = cs})

placeCard :: Board -> Card -> BoardPosition -> Either Error Board
placeCard (MkBoard Nothing c2 c3) newCard Pos1 = Right (MkBoard (Just newCard) c2 c3)
placeCard (MkBoard c1 Nothing c3) newCard Pos2 = Right (MkBoard c1 (Just newCard) c3)
placeCard (MkBoard c1 c2 Nothing) newCard Pos3 = Right (MkBoard c1 c2 (Just newCard))
placeCard _ _ _ = Left BoardPositionOccupied

playCard :: Player -> Int -> BoardPosition -> Either Error Player
playCard player cardIndex pos =
  if null player.hand
    then Left HandEmpty
    else
      if cardIndex < 0 || cardIndex >= length player.hand
        then Left InvalidCardIndex
        else case placeCard player.board (player.hand !! cardIndex) pos of
          Left err -> Left err
          Right newBoard ->
            let newHand = take cardIndex player.hand ++ drop (cardIndex + 1) player.hand
             in Right (player {board = newBoard, hand = newHand})

createDeck :: [Card]
createDeck = replicate 8 soldierCard ++ replicate 2 knightCard

createPlayer :: Player
createPlayer =
  MkPlayer
    { health = 20,
      hand = [],
      board = MkBoard Nothing Nothing Nothing,
      deck = createDeck
    }

initialGame :: Game
initialGame =
  MkGame
    { player1 = createPlayer,
      player2 = createPlayer
    }

main :: IO ()
main = do
  let game = initialGame
  print game
