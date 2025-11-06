Module to define the type of a maze

> module SuperiorMaze (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography

We will represent a maze by its size and a list of its walls.


> data Tree a = Nil | Cons (Tree a) a (Tree a)

> insert :: Ord a => a -> (Tree a) -> (Tree a)
> insert x Nil = (Cons Nil x Nil)
> insert x (Cons l m r) | x < m = (Cons (insert x l) m r)
>                       | x > m = (Cons l m (insert x r))
>                       | x == m = error "Oopsies"

> inTree :: Ord a => a -> (Tree a) -> Bool
> inTree _ Nil = False
> inTree x (Cons l m r) | x < m = inTree x l
>                       | x > m = inTree x r
>                       | x == m = True

> data Maze = Maze Size (Tree Place) (Tree Place) (Tree Place) (Tree Place)

The list of walls will be complete in the sense that we record
both sides of the wall; for example, if the list includes 
((3,4), N), then it will also include ((3,5),S).

This function creates a maze given its size and a list of walls; 
the list of walls might not be complete in the above sense.


> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = Maze (x,y) northWalls eastWalls southWalls westWalls
>     where
>         westBoundary = [((0,j),W) | j <- [0..y-1]]
>         eastBoundary = [((x-1,j),E) | j <- [0..y-1]]
>         southBoundary = [((i,0),S)| i <- [0..x-1]]
>         northBoundary = [((i,y-1),N) | i <- [0..x-1]]
>         northWalls =  ((foldr insert Nil) . conv) (northBoundary ++ (filter (inDir N)) walls ++ ((filter (inDir N)) . (map reflect)) (southBoundary ++ walls))
>         eastWalls = ((foldr insert Nil) . conv) (eastBoundary ++ (filter (inDir E)) walls ++ ((filter (inDir E)) . (map reflect)) (westBoundary ++ walls))
>         southWalls = ((foldr insert Nil) . conv) (southBoundary ++ (filter (inDir S)) walls ++ ((filter (inDir S)) . (map reflect)) (northBoundary ++ walls))
>         westWalls = ((foldr insert Nil) . conv) (westBoundary ++ (filter (inDir W)) walls ++ ((filter (inDir W)) . (map reflect)) (eastBoundary ++ walls))
>         inDir :: Direction -> Wall -> Bool
>         inDir dir wall = dir == snd wall
>         conv :: [(a,b)] -> [a]
>         conv = map fst



The following function "reflects" a wall; i.e. gives the representation as
seen from the other side; for example, reflect ((3,4), N) = ((3,5),S)

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

The following function tests whether the maze includes a wall in a particular
direction from a particular place:

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ walls _ _ _) pos N = inTree pos walls
> hasWall (Maze _ _ walls _ _) pos E = inTree pos walls
> hasWall (Maze _ _ _ walls _) pos S = inTree pos walls
> hasWall (Maze _ _ _ _ walls) pos W = inTree pos walls

The following function returns the size of a maze:

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size