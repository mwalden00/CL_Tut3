data Thing = A | B | C | D | E deriving (Eq,Show)

things :: [Thing]
things = [ A, B, C, D, E ]

data Colour = Amber | Blue deriving Eq

colour :: Thing -> Colour
colour A = Amber
colour B = Amber
colour C = Amber
colour D = Blue
colour E = Amber

data Shape = Square | Disc deriving Eq

shape :: Thing -> Shape
shape A = Square
shape B = Square
shape C = Disc
shape D = Square
shape E = Square

data Size = Big | Small deriving Eq

size :: Thing -> Size
size A = Big
size B = Big
size C = Big
size D = Big
size E = Small

data Border = Thin | Thick deriving Eq

border :: Thing -> Border
border A = Thick
border B = Thin
border C = Thick
border D = Thick
border E = Thick

type Predicate u = u -> Bool

isAmber :: Predicate Thing
isAmber x = colour x == Amber

isBlue :: Predicate Thing
isBlue x = colour x == Blue

isSquare :: Predicate Thing
isSquare x = shape x == Square

isDisc :: Predicate Thing
isDisc x = shape x == Disc

isBig :: Predicate Thing
isBig x = size x == Big

isSmall :: Predicate Thing
isSmall x = size x == Small

hasThinBorder :: Predicate Thing
hasThinBorder x = border x == Thin

hasThickBorder :: Predicate Thing
hasThickBorder x = border x == Thick

--------------------------------------
-- Your task is to replace 'undefined' with the definitions of the operators below.

(|=) :: Predicate Thing -> Predicate Thing -> Bool
a |= b = if null l then False else and l where l = [b x | x <- things, a x]

(|/=) :: Predicate Thing -> Predicate Thing -> Bool
a |/= b = not (a |= b)

(||=) :: [Predicate Thing] -> Predicate Thing -> Bool
a ||= b = and [b x | x <- things, and [c x | c <- a]]

(||/=) :: [Predicate Thing] -> Predicate Thing -> Bool
a ||/= b = not (a ||= b)

neg :: Predicate u -> Predicate u
(neg a) x = not (a x)

(|:|) :: Predicate u -> Predicate u -> Predicate u
(a |:| b) x = (a x) || (b x)

(&:&) :: Predicate u -> Predicate u -> Predicate u
(a &:& b) x = (a x) && (b x)

-- 1. isBig &:& isAmber |= isDisc                   --> FALSE
-- 2. isBig &:& isDisc |= isAmber                   --> TRUE
-- 3. isSmall &:& neg isBlue |= neg isDisc          --> TRUE
-- 4. isBig |:| isAmber |= neg isSquare             --> FALSE
-- 5. neg (isSquare |:| isBlue) |= hasThickBorder   --> TRUE
-- 6. neg isSquare &:& neg isAmber |= isDisc        --> TRUE

-- Combining our infix operators may lead to ambigous expressions. For example, 'a |:| b &:& c' can be read in two ways: either '(a |:| b) &:& c' or 'a |:| (b &:& c)'. We can always instruct Haskell which one to choose by adding paranthesis ourselves.
-- But there are also ways of instructing Haskell how to disambiguate. The next lines are setting the precedence for our infix operators. To learn more about this, watch the video on Precedence from the CL week 3 list of videos (https://media.ed.ac.uk/media/1_25gncs98). It is not crucial you understand all this at the moment; we will remind you of fixity declarations later on during the course. In short, these lines guarantee a non-ambigous parsing of our code.
infixr 0 |=
infixr 0 |/=
infixr 0 ||=
infixr 0 ||/=
infixr 2 |:|
infixr 3 &:&
