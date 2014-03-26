-- A point is a point in the xy plane, represented by x and y coordinates
-- E.g. (Point 0.0 0.0) is the origin, (Point (-1) (1)) is in the top left
-- quadrant.
data Point = Point Double Double
    deriving (Show, Eq)

-- A line segment is a straight line of finite length, defined by its
-- two end points.   E.g. (LineSegment (Point 0 0) (Point 1 1)) is a
-- line segment from the origin to the coordinate (1, 1)
data LineSegment = LineSegment Point Point 
    deriving (Show, Eq)


-- A Path is a 2D path in the xy-plane.  The idea is that Path can be 
-- extended to support straight lines, curves, and arbitrary paths, 
-- but currently there is only one data constructor for Path: Line.
data Path = 
-- Line represents an infinite straight line defined by its slope a
-- and its y intercept b, ie. by the equation y = ax + b
    Line Double Double
    deriving (Show, Eq)
slope x1 x2 y1 y2 = ((y2-y1)/(x2-x1))
yInt x1 x2 y1 y2 = y1 - (slope x1 x2 y1 y2)*x1
intersect (Line m b) (LineSegment(Point x1 y1) (Point x2 y2))
    | (((m == slope x1 x2 y1 y2 )) && (b == yInt x1 x2 y1 y2))= True
    | (((m == slope x1 x2 y1 y2 )) && (b /= yInt x1 x2 y1 y2))= False
    | (x1<x2)&& ((((yInt x1 x2 y1 y2 - b) / (m -  slope x1 x2 y1 y2)) > x1) &&  (((yInt x1 x2 y1 y2 - b) / (m -  slope x1 x2 y1 y2)) < x2)) = True
    | (x2<x1)&& ((((yInt x1 x2 y1 y2 - b) / (m -  slope x1 x2 y1 y2)) > x2) &&  (((yInt x1 x2 y1 y2 - b) / (m -  slope x1 x2 y1 y2)) < x1)) = True
    | (x1==x2) && (y1<y2) && ((m * x1)+b > y1) && ((m * x1)+b < y2) = True
    | (x1==x2) && (y2<y1) && ((m * x1)+b > y2) && ((m * x1)+b < y1) = True
    | otherwise = False
--data Triangle = Triangle Point Point Point
  --  deriving (Show,Eq)
data Shape = Triangle Point Point Point | Quad Point Point Point Point | Circle Point Double
    deriving (Show, Eq)

data BoundingBox = BoundingBox Point Point
    deriving (Show, Eq)

minx :: Shape -> Double
minx (Triangle (Point a b) (Point c d) (Point e f))
    | a<= c && a <=e = a
    | c<=a && c <= e = c
    | e<=a && e <=c = e
minx (Quad (Point a b) (Point c d) (Point e f) (Point g h))
    | a<= c && a <=e && a<= g = a
    | c<=a && c <= e && c<=g= c
    | e<=a && e <=c && e<= g= e
    | g<=a && g <= c && g <= e = g
minx (Circle (Point a b) (radius) )= a - radius

maxx (Triangle (Point a b) (Point c d) (Point e f))
    | a>= c && a>=e = a
    | c>=a && c >= e = c
    | e>=a && e >=c = e
maxx (Quad (Point a b) (Point c d) (Point e f) (Point g h))
    | a>= c && a >=e && a>= g = a
    | c>=a && c >= e && c>=g= c
    | e>=a && e >=c && e>= g= e
    | g>=a && g >= c && g >= e = g
maxx (Circle (Point a b) (radius) )= a + radius

miny (Triangle (Point a b) (Point c d) (Point e f))
    | b<= d && b <=f = b
    | d<=b && d <= f = d
    | f<=b && f <=d = f
miny (Quad (Point a b) (Point c d) (Point e f) (Point g h))
    | b<= d && b <=f && b<= h = b
    | d<=b && d <= f && d<= h= d
    | f<=b && f <=d && f<=h= f
    | h<=b && h<= d && h<= f = h
miny (Circle (Point a b) (radius))= b - radius

maxy (Triangle (Point a b) (Point c d) (Point e f))
    | b>= d && b>=f = b
    | d>=b && d >= f = d
    | f>=b && f >=d = f
maxy (Quad (Point a b) (Point c d) (Point e f) (Point g h))
    | b>= d && b >=f && b>= h = b
    | d>=b && d >= f && d>= h= d
    | f>=b && f >=d && f>=h= f
    | h>=b && h>= d && h>= f = h
maxy (Circle (Point a b) (radius) )= b + radius

boundShape (Triangle (Point a b) (Point c d) (Point e f)) = BoundingBox (Point (minx (Triangle (Point a b) (Point c d) (Point e f)))(maxy (Triangle (Point a b) (Point c d) (Point e f)))) (Point (maxx (Triangle (Point a b) (Point c d) (Point e f))) (miny (Triangle (Point a b) (Point c d) (Point e f))))

boundShape (Quad (Point a b) (Point c d) (Point e f) (Point g h)) = BoundingBox (Point (minx (Quad (Point a b) (Point c d) (Point e f) (Point g h)))(maxy (Quad (Point a b) (Point c d) (Point e f) (Point g h)))) (Point (maxx (Quad (Point a b) (Point c d) (Point e f) (Point g h))) (miny (Quad (Point a b) (Point c d) (Point e f) (Point g h))))

boundShape (Circle (Point a b) (radius)) = BoundingBox (Point (minx (Circle (Point a b) (radius)))(maxy (Circle (Point a b) (radius)))) (Point (maxx (Circle (Point a b) (radius))) (miny (Circle (Point a b) (radius))))

intersectBB (Line m b) (BoundingBox (Point x y) (Point w z))
    |  ((m * x)+b <= y) && ((m * x)+b >= z) = True
    |  ((m * w)+b <= y) && ((m * w)+b >= z) = True
    | (((y - b) / m) >= x) &&  (((y - b) / m ) <= w) = True
    | (((z - b) / m ) >= x) &&  (((z - b) / m) <= w) = True
    | otherwise = False

mightIntersectShape (Line m y) (Triangle (Point a b) (Point c d) (Point e f)) =  intersectBB (Line m y) (boundShape (Triangle (Point a b) (Point c d) (Point e f)))
mightIntersectShape (Line m y) (Quad (Point a b) (Point c d) (Point e f) (Point g h)) =  intersectBB (Line m y) (boundShape (Quad (Point a b) (Point c d) (Point e f) (Point g h)))
mightIntersectShape (Line m y) (Circle (Point a b) radius ) =  intersectBB (Line m y) (boundShape (Circle (Point a b) radius))
    
