type Number = Int

type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
    | Circle Point Length
    | Rect Point Length Length
    deriving Show

type Figure = [Shape]

type BBox = (Point,Point)

-- Global Variables
f = [Pt (4,4), Circle (5,5) 3, Rect (3,3) 7 2]

-- a) Define the function width that computes the width of a shape
width :: Shape -> Length
width (Pt _) = 0
width (Circle _ r) = r*2
width (Rect _ wdth _) = wdth

-- b) Define the function bbox that computes the bounding box of a shape.
bbox :: Shape -> BBox
bbox (Pt point) = (point,point)
bbox (Circle (x,y) r) = ((x-r, y-r), (x+r, y+r))
bbox (Rect (x,y) wdth hght) = ((x,y), (x+wdth, y+hght))

-- c) Define the function minX that computes the minimum x coordinate of a shape.
minX :: Shape -> Number
minX (Pt (x,_)) = x
minX (Circle (x,_) r) = x - r
minX (Rect (x,_) _ _) = x


-- d) Define a function move that moves the position of a shape by a vector given by a point as its second argument.
addPt :: Point -> Point -> Point
addPt (x_1,y_1) (x_2,y_2) = (x_1 + x_2, y_1 + y_2)

move :: Shape -> Point -> Shape
move (Pt point_1) point_2 = (Pt (addPt point_1 point_2)) 
move (Circle point_1 r) point_2 = (Circle (addPt point_1 point_2) r) 
move (Rect point_1 wdth hght) point_2 = (Rect (addPt point_1 point_2) wdth hght) 


-- e) Define a function alignLeft that transforms one figure into another one in which all shapes have the same minX coordinate but are otherwise unchanged.
moveToX :: Number -> Shape -> Shape
moveToX new_x (Pt (_,y)) = Pt (new_x,y)
moveToX new_x (Circle (_,y) r) = Circle (new_x,y) r
moveToX new_x (Rect (_,y) wdth hght) = Rect (new_x,y) wdth hght


alignLeft :: Figure -> Figure
alignLeft [] = []
alignLeft figure = map (moveToX (minimum(map minX figure))) figure

-- f) Define a function inside that checks whether one shape is inside of another one, that is, whether the area covered by the first shape is also covered by the second shape.
sqr :: Number->Number
sqr num = num*num

inside :: Shape -> Shape -> Bool
inside (Pt (x_1,y_1)) (Pt (x_2,y_2)) =
    if x_1 == x_2 && y_1 == y_2 then do True
    else do False
inside (Pt (x_1, y_1)) (Circle (x_2,y_2) r) =
    if round(sqrt (fromIntegral ( sqr (x_1 - x_2) + sqr (y_1 - y_2)))) <= r then do True
    else do False
inside (Pt (x_1, y_1)) (Rect (x_2,y_2) wdth hght) =
    if x_1 >= x_2 && x_1 <= (x_2 + wdth) && y_1 >= y_2 && y_1 <= (y_2 + hght)  then do True
    else do False
inside (Circle (x_1,y_1) r) (Pt (x_2, y_2)) =
    if x_1 == x_2 && y_1 == y_2 && r == 0 then do True
    else do False
inside (Circle (x_1,y_1) r_1) (Circle (x_2,y_2) r_2) =
    if round(sqrt (fromIntegral( sqr (x_2 - x_1) + sqr (y_2 - y_1)))) > (r_1 + r_2) then do False
    else if round(sqrt (fromIntegral( sqr (x_2 - x_1) + sqr (y_2 - y_1)))) <= (abs (r_1 - r_2)) then do True

    else do False
inside (Circle (x_1,y_1) r) (Rect (x_2,y_2) wdth hght) =
    if (x_1 - r) >= x_2 && (y_1 - r) >= y_2 && (x_2 + r) <= (x_2 + wdth) && (y_2 + r) <= (y_2 + hght) then do True
    else do False
inside (Rect (x_1,y_1) wdth hght) (Pt (x_2, y_2)) =
    if x_2 >= x_1 && x_2 <= (x_1 + wdth) && y_2 >= y_1 && y_2 <= (y_1 + hght)  then do True
    else do False
inside (Rect (x_1,y_1) wdth hght) (Circle (x_2,y_2) r) =
    if inside (Pt (x_1,y_1)) (Circle (x_2,y_2) r) &&
    inside (Pt (x_1+wdth,y_1)) (Circle (x_2,y_2) r) &&
    inside (Pt (x_1,y_1+hght)) (Circle (x_2,y_2) r) &&
    inside (Pt (x_1+wdth,y_1+hght)) (Circle (x_2,y_2) r) then do True
    else do False
       
inside (Rect (x_1,y_1) wdth_1 hght_1) (Rect (x_2, y_2) wdth_2 hght_2) =
    if x_1 >= x_2 && (x_1 + wdth_2) <= (x_2 + wdth_2) && y_1 >= y_2 && (y_1 + hght_1) <= (y_2 + hght_2)  then do True
    else do False

