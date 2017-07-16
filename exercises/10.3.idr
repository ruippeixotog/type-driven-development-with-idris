module Chapter6_3

import Data.Vect

-- shape --

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

-- store --

infixr 5 .+.

data Schema = SString
            | SInt
            | SChar
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
SchemaType (sc1 .+. sc2) = (SchemaType sc1, SchemaType sc2)

record DataStore (schema : Schema) where
  constructor MkData
  size : Nat
  items : Vect size (SchemaType schema)

empty : DataStore schema
empty = MkData 0 []

addToStore : SchemaType schema -> DataStore schema -> DataStore schema
addToStore value (MkData _ items) = MkData _ (value :: items)

data StoreView : DataStore schema -> Type where
  SNil : StoreView empty
  SAdd : (rec : StoreView store) -> StoreView (addToStore value store)

storeViewHelp : (items : Vect size (SchemaType schema)) -> StoreView (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (val :: xs) = SAdd (storeViewHelp xs)

storeView : (store : DataStore schema) -> StoreView store
storeView (MkData _ items) = storeViewHelp items

-- exercise 1
getValues : DataStore (SString .+. val_schema) -> List (SchemaType val_schema)
getValues store with (storeView store)
  getValues empty | SNil = []
  getValues (addToStore value store) | (SAdd rec) = snd value :: getValues store | rec

testStore : DataStore (SString .+. SInt)
testStore = addToStore ("First", 1) $ addToStore ("Second", 2) $ empty

-- exercise 2
triangle : Double -> Double -> Shape
triangle = Triangle

rectangle : Double -> Double -> Shape
rectangle = Rectangle

circle : Double -> Shape
circle = Circle

data ShapeView : Shape -> Type where
  STriangle : ShapeView (triangle base height)
  SRectangle : ShapeView (rectangle width height)
  SCircle : ShapeView (circle radius)

shapeView : (shape : Shape) -> ShapeView shape
shapeView (Triangle base height) = STriangle
shapeView (Rectangle width height) = SRectangle
shapeView (Circle radius) = SCircle

area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = 0.5 * base * height
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius
