import Data.Array.IO
import Data.List

import System.IO

import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray
import Data.Graph.Inductive.Query.BFS

-- =========================
-- MaxFlow
-- =========================

filterFB c p x r = if p x r then x `c` r else r

-- получить обратные рёбра нулевой пропускной способности
--getRevEdges :: (Num b,Ord b) => [(Node,Node)] -> [(Node,Node,(b,b,b))]
getRevEdges lst = foldl func [] lst where 
    func edg (u, v) = filterFB (:) elem (v, u, (0,0,0)) edg

-- Вставить в граф обратные рёбра и изменить метки
-- всех рёбер с (пропускная способность) на (пр. сп-ть, текущий поток (0 по умолчанию), остаток)

--   i       (i,0,i)
-- a---->b  a------->b
--augmentGraph :: (DynGraph gr,Num b,Ord b) => gr a b -> gr a (b,b,b)
augmentGraph g = insEdges (getRevEdges $ edges g') g'
    where g' = emap (\i->(i,0,i)) g

foldPath _ [] g =  g
foldPath _ [_] g =  g
foldPath f (u:v:ls) g = foldPath f (v:ls) (f u v g)

--updateFlow :: (DynGraph gr,Num b,Ord b) => LPath (b,b,b) -> b -> gr a (b,b,b) -> gr a (b,b,b)
updateFlow path g = foldPath updateNode path g
    where cf = minimum $ map (z_coord.snd) $ tail path
          updateNode (u,(c, p, inv_p)) (v, _) g = insEdge (u, v, (c, p+cf, inv_p-cf)) $ delEdge (u,v) g

--mfmg :: (DynGraph gr,Num b,Ord b) => gr a (b,b,b) -> Node -> Node -> gr a (b,b,b)
mfmg g s t | augLPath == [] = g
           | otherwise      = mfmg (updateFlow augLPath g) s t
             where LP augLPath = lesp s t $ elfilter ((/=0).z_coord) g


--maxFlowgraph :: (DynGraph gr,Num b,Ord b) => gr a b -> Node -> Node -> gr a (b,b)
maxFlowgraph g s t = emap (\(u,v,_)->(v,u)) $ elfilter ((/=0).x_coord) g'
                           where g' = mfmg (augmentGraph g) s t

-- =========================
-- readGraph
-- =========================

header = "5 4"
content = "1  0  0  0\n1  0  0  0\n0  0  0  1\n0  0  1  1\n1  1  1  1"

-- readGraph :: Gr Int Int
readGraph header body = (mkGraph vs edges, k, l) where
    [k, l] = map read $ words header
    adj = map read $ words body
    es = [(x, y) | x <- [1..k], y <- [k+1..k+l]]
    edges = [(0, x, 1) | x <- [1..k]] ++
            (labelEdges es adj) ++
            [(y, k+l+1, 1) | y <- [k+1..k+l]] :: [LEdge Int]
    vs = labelVertexes [0..k+l+1] $ [0..k] ++ [1..l] ++ [k+l+1]

-- Есть список весов (матрица смежности) и список рёбер
-- Присвоить каждому ребру вес
labelEdges x y = filter ((/=0).z_coord) $ zipWith (\(x, y) w -> (x, y, w)) x y

-- Проставить метки на вершины
-- (во-первых, это требование типа данных,
-- во-вторых, по ним потом легко будет выводить данные)
labelVertexes = zip
                                 

-- =========================
-- main
-- =========================

-- first, second, and third elements of 3-tuple
x_coord (x, _, _) = x
y_coord (_, y, _) = y
z_coord (_, _, z) = z

xpair (_, 0) = 0
xpair (y, _) = y

maximump [] = (0, 0)
maximump es = foldl1 maxp es
maxp fp@(_, b) sp@(_, d) = if b <= d then sp else fp

main = do
    fin <- openFile "in.txt" ReadMode
    header <- hGetLine fin
    adj <- hGetContents fin
    
    let
      (graph, k, l) = readGraph header adj :: (Gr Int Int, Int, Int)
      f = maxFlowgraph graph 0 (k+l+1)
      me'' = labEdges $! efilter (\(x, y, _) -> x /= 0 && y /= k+l+1) f
      me' = [map (\(_, y, (f, _)) -> (y-k, f)) $ filter ((== x).x_coord) me'' | x <- [1..k]]
      me  = map (xpair . maximump) me'
    putStr $ prettify graph

    fout <- openFile "out.txt" WriteMode
    hPutStr fout (unwords $ map show $ take (k-1) me)
    hClose fin
    hClose fout

prettify :: (DynGraph gr, Show a, Show b) => gr a b -> String
prettify g = ufold showsContext id g ""
  where
    showsContext (_,n,l,s) sg = shows n . (':':) . shows l
                                . showString "->" . shows s
                                . ('\n':) . sg