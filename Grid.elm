module Grid where

import Window
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- View
view: (Int, Int) -> Element
view (w, h) =
 let trimW =  toFloat w
     trimH = toFloat h
 in 
    collage w h [
      quadrent trimW trimH X      
      , quadrent trimW trimH Y
      , toForm (majors trimW trimH Y)
      , toForm (majors trimW trimH X)
      , toForm (minors trimW trimH Y)
      , toForm (minors trimW trimH X)
    ] 
  

--Model
type Axis = 
  X |
  Y

quadrent: Float -> Float -> Axis -> Form
quadrent width height axis=
  traced (solid black) (fancyLine 0 (width, height) axis)


majors: Float -> Float -> Axis -> Element
majors width height axis = 
  let 
     wSlice = width/5
     hSlice = height/5  
     range = [-2.0..2.0]
     ln i = case axis of
        X -> traced (dashed black) (fancyLine (hSlice *i) (width, height) axis)
        Y -> traced (dashed black) (fancyLine (wSlice *i) (width, height) axis)
  in collage (round width) (round height) ( List.map ln range )


minors: Float -> Float -> Axis -> Element
minors width height axis =
 simpleGrid 10 width height axis 

simpleGrid: Int -> Float -> Float -> Axis -> Element
simpleGrid lines width height axis = 
  let
      floatLines = toFloat lines
      wSlice = width / floatLines
      hSlice = height / floatLines
      range = [-floatLines/2 .. floatLines/2]
      ln i = case axis of
        X -> traced (dotted black) (fancyLine (hSlice * i) (width, height) axis)
        Y -> traced (dotted black) (fancyLine (wSlice *i) (width, height) axis)
  in collage (round width) (round height) ( List.map ln range )



fancyLine: Float -> (Float, Float) -> Axis -> Path
fancyLine n (width, height) axis = 
  case axis of
    X -> path [(-width/2, n), (width/2, n)]
    Y -> path [(n, -height/2), (n, height/2)]

