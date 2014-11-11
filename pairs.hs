import Data.Array.IO
import Data.List

import System.IO

import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Monad
import Data.Graph.Inductive.Monad.IOArray
import Data.Graph.Inductive.Query.BFS

intToDigit :: Int -> Char
intToDigit i
    | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
    | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i - 10)
    | otherwise            =  error "Char.intToDigit: not a digit"

intToStr :: Int -> String
intToStr 0 = "0"
intToStr i = map intToDigit (fi i)
    where
        --is = [ x | x <- (fi i) ]
        fi 0 = []
        fi x = (x `mod` 10) : (fi $ x `div` 10)

isDigit c =  c >= '0'   &&  c <= '9'

digitToInt :: Char -> Int
digitToInt c
    | isDigit c            =  fromEnum c - fromEnum '0'
    | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
    | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
    | otherwise            =  error "Char.digitToInt: not a digit"

strToInt :: String -> Int
strToInt s =  foldl f 0 $ map digitToInt s
    where
        f z x = 10*z + x

-- Есть список весов (матрица смежности) и список рёбер
-- Присвоить каждому ребру вес
labelEdges [] [] = []
labelEdges (ads:adss) (es:ess) = (f ads es) : labelEdges adss ess
    where
       f [] [] = []
       f (w:ws) ((x,y):xs) = (x, y, w) : f ws xs

-- Проставить метки на вершины
-- (во-первых, это требование типа данных,
-- во-вторых, по ним потом легко будет выводить данные)
labelVertexes = zip

-- =========================
-- MaxFlow
-- =========================

-- получить обратные рёбра нулевой пропускной способности
--getRevEdges :: (Num b,Ord b) => [(Node,Node)] -> [(Node,Node,b)]
getRevEdges [] = []
getRevEdges ((u,v):es) | notElem (v,u) es = (v,u,0):getRevEdges es
                       | otherwise        = getRevEdges (delete (v,u) es)

-- Вставить в граф обратные рёбра и изменить метки
-- всех рёбер с (пропускная способность) на (пр. сп-ть, текущий поток (0 по умолчанию), остаток)

--   i       (i,0,i)
-- a---->b  a------->b
--augmentGraph :: (DynGraph gr,Num b,Ord b) => gr a b -> gr a (b,b,b)
augmentGraph g = emap (\i->(i,0,i)) (insEdges (getRevEdges (edges g)) g)
                                                
--updAdjList::(Num b,Ord b) => [((b,b,b),Node)]->Node->b->Bool->[((b,b,b),Node)]
updAdjList s v cf fwd | fwd == True = ((x,y+cf,z-cf),w):rs
                      | otherwise   = ((x,y-cf,z+cf),w):rs
                        where ((x,y,z),w) = head (filter (\(_,w')->v==w') s)
                              rs          = filter (\(_,w')->v/=w') s

--updateFlow :: (DynGraph gr,Num b,Ord b) => Path -> b -> gr a (b,b,b) -> gr a (b,b,b)
updateFlow []      _ g = g
updateFlow [_]       _ g = g
updateFlow (u:v:vs) cf g = case match u g of
                             (Nothing,g')        -> g'
                             (Just (p,u',l,s),g') -> (p',u',l,s') & g2
                                where g2 = updateFlow (v:vs) cf g'
                                      s' = updAdjList s v cf True
                                      p' = updAdjList p v cf False

--mfmg :: (DynGraph gr,Num b,Ord b) => gr a (b,b,b) -> Node -> Node -> gr a (b,b,b)
mfmg g s t | augPath == [] = g
           | otherwise     = mfmg (updateFlow augPath minC g) s t
             where minC        = minimum (map ((\(_,_,z)->z).snd)(tail augLPath))
                   augPath     = map fst augLPath
                   LP augLPath = lesp s t gf
                   gf          = elfilter (\(_,_,z)->z/=0) g

--mf :: (DynGraph gr,Num b,Ord b) => gr a b -> Node -> Node -> gr a (b,b,b)
mf g s t = mfmg (augmentGraph g) s t

--maxFlowgraph :: (DynGraph gr,Num b,Ord b) => gr a b -> Node -> Node -> gr a (b,b)
maxFlowgraph g s t = emap (\(u,v,_)->(v,u)) g2
                           where g2 = elfilter (\(x,_,_)->x/=0) g1
                                 g1 = mf g s t
                                  
-- =========================
-- main
-- =========================

showLst xs = concat $ map (\x -> show x ++ " ") xs

xpair []       = []
xpair (fe:fes) = pair fe : xpair fes
    where
        pair (_, 0) = 0
        pair (y, _) = y

main = do
    fin <- openFile "in.txt" ReadMode
    k' <- hGetLine fin
    l' <- hGetLine fin
    content <- hGetContents fin
    let
        k' = "4"
        l' = "4"

        k = strToInt k'
        l = strToInt l'
        a'' = lines content
        a' = map words a''
        adj = map (map strToInt) a'

        es = [[(x, y) | x <- [1..k]] | y <- [k+1..k+l]]
        les = [(k+l+1, x, 1) | x <- [1..k]] ++
              (concat $! labelEdges adj es) ++ 
              [ (y, k+l+2, 1) | y <- [k+1..k+l]] :: [LEdge Int]
        vs = labelVertexes [1..k+l+2] ([1..k] ++ [1..l] ++ [k+l+1, k+l+2])
        g = mkGraph vs les :: Gr Int Int
        f = maxFlowgraph g 9 10
        me'' = labEdges $! efilter (\(x, y, _) -> x /= k+l+1 && y /= k+l+2) f

        me' = map (map (\(x, y, (f, _)) -> (y-k, f))) [[f | f <- me'', x == (\(s, _, _) -> s) f] | x<-[1..k]]
        -- $! filter (\(x, _, _) -> x <= k) me''
        me  = map maximump me'
            where
                maximump [] = (0, 0)
                maximump es = foldl1 maxp es
                maxp (a, b) (c, d)
                    | b <= d    = (c, d)
                    | otherwise = (a, b)
        x = xpair me

    fout <- openFile "out.txt" WriteMode
    hPutStr fout (showLst x)

    hClose fin
    hClose fout