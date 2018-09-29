module Data.Picture where

import Prelude

import Data.Foldable (foldl)

import Global as G
import Math as M

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number
  }

data Bounds = Bounds
  { top :: Number
  , left :: Number
  , bottom :: Number
  , right :: Number
  }

type Picture = Array Shape

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
    combine :: Bounds -> Shape -> Bounds
    combine b shape = union (shapeBounds shape) b

showPoint :: Point -> String
showPoint (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

showShape :: Shape -> String
showShape (Circle origin radius) = "(" <> showPoint origin <> ", " <> show radius <> ")"
showShape (Rectangle origin width height) = "(" <> showPoint origin <> ", " <> show width <> ", " <> show height <> " )"
showShape (Line p1 p2) = "(" <> showPoint p1 <> ", " <> showPoint p2 <> ")"
showShape (Text origin text) = "(" <> showPoint origin <> ", " <> text <> ")"

showPicture :: Picture -> Array String
showPicture = map showShape

showBounds :: Bounds -> String
showBounds (Bounds b) =
  "Bounds [top: " <> show b.top <>
  ", left: "      <> show b.left <>
  ", bottom: "    <> show b.bottom <>
  ", right: "     <> show b.right <>
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x, y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x, y }) w h) = Bounds
  { top:    y - h / 2.0
  , left:   x - w / 2.0
  , bottom: y + h / 2.0
  , right:  x + w / 2.0
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    M.min p1.y p2.y
  , left:   M.min p1.x p2.x
  , bottom: M.max p1.y p2.y
  , right:  M.max p1.x p2.x
  }
shapeBounds (Text (Point { x, y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }

union :: Bounds -> Bounds -> Bounds
union (Bounds b1) (Bounds b2) = Bounds
  { top:    M.min b1.top    b2.top
  , left:   M.min b1.left   b2.left
  , bottom: M.max b1.bottom b2.bottom
  , right:  M.max b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     G.infinity
  , left:    G.infinity
  , bottom: -G.infinity
  , right:  -G.infinity
  }

area :: Shape -> Number
area (Circle origin radius) = M.pi * radius * radius
area (Rectangle _ w h) = w * h
area (Line p1 p2) = 0.0
area (Text _ _) = 0.0
