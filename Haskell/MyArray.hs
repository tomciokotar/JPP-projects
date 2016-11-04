module MyArray (
	Array, listArray, (!), elems,
	array, update, (//), contains,
	range, index, inRange, rangeSize
	) where


-- Ix
class Ord a => Ix a where
	range :: (a, a) -> [a]
	index :: (a, a) -> a -> Int
	inRange :: (a, a) -> a -> Bool
	rangeSize :: (a, a) -> Int
	
	inRange (a, b) i = a <= i && i <= b

boundsError = error "Index out of bounds"


instance Ix Char where
	range (a, b) = [a..b]
	index (a, b) i
		| inRange (a, b) i = fromEnum i - fromEnum a
		| otherwise = boundsError
	rangeSize (a, b) = max 0 (fromEnum b - fromEnum a + 1)


instance Ix Int where
	range (a, b) = [a..b]
	index (a, b) i
		| inRange (a, b) i = i - a
		| otherwise = boundsError
	rangeSize (a, b) = max 0 (b - a + 1)


instance Ix Integer where
	range (a, b) = [a..b]
	index (a, b) i
		| inRange (a, b) i = fromInteger (i - a)
		| otherwise = boundsError
	rangeSize (a, b) = max 0 (fromInteger (b - a + 1))


instance (Ix a, Ix b) => Ix (a, b) where
	range ((a1, a2), (b1, b2)) =
		[(i, j) | i <- range (a1, b1),
				  j <- range (a2, b2)]

	index ((a1, a2), (b1, b2)) (i, j)
		| inRange ((a1, a2), (b1, b2)) (i, j) =
			(index (a1, b1) i) * (rangeSize (a2, b2)) + (index (a2, b2) j)
		| otherwise = boundsError

	inRange ((a1, a2), (b1, b2)) (i, j) =
		inRange (a1, b1) i && inRange (a2, b2) j
	
	rangeSize ((a1, a2), (b1, b2)) =
		rangeSize (a1, b1) * rangeSize (a2, b2)


--instance (Ix i, Show e) => Show (Array i e) where
--	show array = show (elems array)


-- Array
data Array i e = Array (i, i) (RangeTree e)

data RangeTree e = Range (Int, Int) (RangeTree e) (RangeTree e) |
				   Elem Int e |
				   Empty (Int, Int)


getMid :: Int -> Int -> Int
getMid a b = a + ((b - a) `div` 2)


-- update
update :: (Ix i) => i -> e -> Array i e -> Array i e

update ind val (Array (a, b) tree) =
	Array (a, b) (insert (index (a, b) ind) val tree)


insert :: Int -> e -> RangeTree e -> RangeTree e

insert ind val (Elem currInd _)
	| ind == currInd = Elem ind val

insert ind val (Empty (a, b))
	| ind == a && a == b =
		Elem ind val
	
	| a < b && ind <= mid =
		Range (a, b) (insert ind val (Empty (a, mid))) (Empty (mid + 1, b))
	
	| a < b && ind > mid =
		Range (a, b) (Empty (a, mid)) (insert ind val (Empty (mid + 1, b)))
	
	where mid = getMid a b


insert ind val (Range (a, b) leftTree rightTree)
	| a < b && ind <= mid =
		Range (a, b) (insert ind val leftTree) rightTree
	
	| a < b && ind > mid =
		Range (a, b) leftTree (insert ind val rightTree)
	
	where mid = getMid a b


-- (//)
(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(//) array [] = array
(//) array ((ind, val):rest) = (update ind val array) // rest


-- array
array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array (a, b) values =
	(Array (a, b) (Empty (0, rangeSize (a, b) - 1))) // values


-- listArray
listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray (a, b) values =
	let limit = rangeSize (a, b) - 1
	in Array (a, b) (insertIntoTree values 0 limit (Empty (0, limit)))


insertIntoTree :: [e] -> Int -> Int -> RangeTree e -> RangeTree e
insertIntoTree [] _ _ tree = tree
insertIntoTree (val:rest) ind limit tree
	| ind  > limit = tree
	| ind <= limit = insertIntoTree rest (ind + 1) limit (insert ind val tree)


-- (!)
(!) :: (Ix i) => Array i e -> i -> e
(!) (Array (a, b) tree) ind = getElem tree (index (a, b) ind)


getElem :: RangeTree e -> Int -> e
getElem (Empty (_,_)) _ = error "A value for the given index doesn't exist"

getElem (Elem currInd val) ind
	| currInd == ind = val

getElem (Range (a, b) leftTree rightTree) ind
	| ind <= mid = getElem leftTree ind
	| ind > mid = getElem rightTree ind
	where mid = getMid a b


-- elems
elems :: (Ix i) => Array i e -> [e]
elems (Array (_,_) tree) = listElems tree

listElems :: RangeTree e -> [e]
listElems (Empty (_,_)) = []
listElems (Elem _ val) = [val]
listElems (Range (_,_) leftTree rightTree) =
	(listElems leftTree) ++ (listElems rightTree)


-- contains
contains :: (Ix i) => Array i e -> i -> Bool
contains (Array (a, b) tree) ind = treeContains tree (index (a, b) ind)

treeContains :: RangeTree e -> Int -> Bool
treeContains (Empty (_,_)) _ = False

treeContains (Elem elemInd _) ind
	| elemInd == ind = True

treeContains (Range (a, b) leftTree rightTree) ind
	| ind <= mid = treeContains leftTree ind
	| ind > mid = treeContains rightTree ind
	where mid = getMid a b
