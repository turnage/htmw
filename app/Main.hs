module Main where

import Lib

data Node = Node
  { weight :: Double
  }

blankNode :: Node
blankNode = Node {weight = 1.0}

data Correction
  = Inhibit
  | Excite

data Perceptron = Perceptron
  { left :: Node
  , right :: Node
  , threshold :: Double
  }

blankSlate :: Perceptron
blankSlate = Perceptron {left = blankNode, right = blankNode, threshold = 1.0}

correctionFactor :: Double
correctionFactor = 0.1

perceive :: (Bool, Bool) -> Perceptron -> Bool
perceive (left, right) Perceptron { left = Node {weight = leftWeight}
                                  , right = Node {weight = rightWeight}
                                  , threshold
                                  , ..
                                  } =
  leftWeight * leftFactor + rightWeight * rightFactor >= threshold
  where
    leftFactor = switchFactor left
    rightFactor = switchFactor right
    switchFactor switch =
      case switch of
        True -> 1.0
        False -> 0.0

trainSet :: [((Bool, Bool), Bool)] -> Perceptron -> Perceptron
trainSet [] perceptron = perceptron
trainSet (input:set) perceptron = trainSet set $ train input perceptron

train :: ((Bool, Bool), Bool) -> Perceptron -> Perceptron
train (input, correct) perceptron =
  if result == correct
    then perceptron
    else if correct == False
           then inhibit perceptron
           else excite perceptron
  where
    result = perceive input perceptron

inhibit :: Perceptron -> Perceptron
inhibit perceptron =
  perceptron {threshold = threshold perceptron + correctionFactor}

excite :: Perceptron -> Perceptron
excite perceptron =
  perceptron {threshold = threshold perceptron - correctionFactor}

andTestKey :: [((Bool, Bool), Bool)]
andTestKey =
  [ ((True, True), True)
  , ((False, False), False)
  , ((False, True), False)
  , ((True, False), False)
  --, ((False, False), True) -- corrupting training input
  , ((True, True), True)
  ]

andTest :: Perceptron -> [(((Bool, Bool), Bool), Bool)]
andTest perceptron = zip andTestKey results
  where
    results = map (\f -> f perceptron) tests
    tests = map perceive inputs
    inputs = map (fst) andTestKey

main :: IO ()
main = do
  let perceptron = trainSet andTestKey blankSlate
  putStrLn $ show $ andTest perceptron
