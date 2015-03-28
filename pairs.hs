import Data.Array.IO
import Data.List
import Debug.Trace
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

-- получить обратные рёбра нулевой пропускной способности
getRevEdges :: (Num b,Ord b) => [(Node,Node)] -> [(Node,Node,b)]
getRevEdges [] = []
getRevEdges ((u,v):es) | notElem (v, u) es = (v, u, 0) : getRevEdges es
                       | otherwise        = getRevEdges (delete (v,u) es)

-- Вставить в граф обратные рёбра и изменить метки
-- всех рёбер с (пропускная способность) на (пр. сп-ть, текущий поток (0 по умолчанию), остаток)

--   i       (i,0,i) == (пропускная способность, текущий поток, остаточная функция)
-- a---->b  a------->b
augmentGraph :: (DynGraph gr,Num b,Ord b) => gr a b -> gr a (b,b,b)
augmentGraph g = emap (\i->(i, 0, i)) $ insEdges (getRevEdges $ edges g) g

mapPath _ [] g =  g
mapPath _ [_] g =  g
mapPath f (u:v:ls) g = mapPath f (v:ls) (f u v g)

updateFlow path g = mapPath updateNode path g
    where cf = minimum $ map (z_coord.snd) $ tail path
          updateNode (u,(c, p, inv_p)) (v, _) g = insEdge (u, v, (c, p+cf, inv_p-cf)) $ delEdge (u,v) g

--mfmg :: (DynGraph gr,Num b,Ord b) => gr a (b,b,b) -> Node -> Node -> gr a (b,b,b)
mfmg g s t | null augLPath = g
           | otherwise = mfmg (updateFlow augLPath $ trace (prettify g) g) s t
                where LP augLPath = lesp s t gf
                      gf          = ((/=0).z_coord) `elfilter` g
                      
            
--mf :: (DynGraph gr,Num b,Ord b) => gr a b -> Node -> Node -> gr a (b,b,b)
mf g s t = mfmg (augmentGraph g) s t

--maxFlowgraph :: (DynGraph gr,Num b,Ord b) => gr a b -> Node -> Node -> gr a (b,b)
maxFlowgraph g s t = emap (\(u, v, _)->(v, u)) g'
                   where g' = elfilter ((/=0).x_coord) $ mf g s t

-- =========================
-- readGraph
-- =========================

header = "5 4"
content = "1  0  0  0\n1  0  0  0\n0  0  0  1\n0  0  1  1\n1  1  1  1"

-- readGraph :: Gr Int Int
readGraph header body = (mkGraph vs edges, k, l) where
    [k, l] = map read $ words header
    adj = stringToAdj body
    es = [(x, y) | x <- [1..k], y <- [k+1..k+l]]
    edges = [(0, x, 1) | x <- [1..k]] ++
            (labelEdges es adj) ++
            [(y, k+l+1, 1) | y <- [k+1..k+l]] :: [LEdge Int]
    vs = labelVertexes [0..k+l+1] $ [0..k] ++ [1..l] ++ [k+l+1]

-- Строку разделяем на слова и прочитаем каждое
stringToAdj = map read . words

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

xpair = map pair
    where
        pair (_, 0) = 0
        pair (y, _) = y

main = do
    fin <- openFile "in.txt" ReadMode
    header <- hGetLine fin
    adj <- hGetContents fin    
    let
        (graph, k, l) = readGraph header adj :: (Gr Int Int, Int, Int)
        flow = maxFlowgraph graph 0 (k+l+1)

        me'' = labEdges $! efilter (\(x, y, _) -> x /= 0 && y /= k+l+2) flow
        me  = [maximump [(y - k, f) | (x', y, (f, _)) <- me'', x' /= x] | x <- [1..k]]
          where
              maximump [] = (0, 0)
              maximump es = foldl1 maxp es
              maxp (a, b) (c, d) = if b <= d then (c, d) else (a, b)
        x = xpair me
    fout <- openFile "out.txt" WriteMode
    hPutStr fout (unwords $ map show x)

    hClose fin
    hClose fout

prettify :: (DynGraph gr, Show a, Show b) => gr a b -> String
prettify g = ufold showsContext id g ""
  where
    showsContext (_,n,l,s) sg = shows n . (':':) . shows l
                                . showString "->" . shows s
                                . ('\n':) . sg
