{-# LANGUAGE TupleSections #-}
module Exercise6_5 where

import Data.Maybe (isJust)
import Control.Monad.Writer.Lazy
import qualified Data.List
import Data.List (intercalate, sort)
import qualified Data.Set
import qualified Data.Map
import Data.Map (Map, (!))
import System.IO

data Graph v e = Graph { vertex :: [v], 
                         edgeList :: [e],
                         edgeMap :: (v,v) -> Maybe e,
                         edgeVertex :: e -> (v,v) }


instance (Show v, Show e) => Show (Graph v e) where
    show g = "{ vertex = " ++ 
                (show $ vertex g) ++
                "edges = " ++ (show $ edgeList g) ++
              "}"

{- Graph with no edge labels -}
type BaseGraph v = Graph v (v,v)

fromSets' :: (Ord v) => [v] -> [(v,v)] -> Graph v (v,v)
fromSets' v e =
    Graph { vertex = v,
            edgeList = e,
            edgeMap = 
              let s = Data.Set.fromList e in
              (\vv -> if (Data.Set.member vv s) then Just vv else Nothing),
            edgeVertex = id }

fromSets :: (Ord v) => [v] -> [((v,v),a)] -> Graph v ((v,v),a)
fromSets v e = 
    Graph { vertex = v,
            edgeList = e,
            edgeMap = 
              let m = (Data.Map.fromList e) in
              (\vv -> do
                        e <- Data.Map.lookup vv m
                        return (vv, e)),
            edgeVertex = fst }

member :: Graph v e -> (v,v) -> Bool
member g = Data.Maybe.isJust . (edgeMap g)

{- Removes labels from the edges of a graph -}
baseGraph :: Graph v e -> BaseGraph v
baseGraph g = Graph { vertex = (vertex g),
                      edgeList = map (edgeVertex g) (edgeList g),
                      edgeMap = (\vv -> fmap (const vv) (edgeMap g vv)),
                      edgeVertex = id }


enumerateMaps :: (Ord a) => [a] -> [b] -> [Map a b]
enumerateMaps as bs = map Data.Map.fromList (mapM (\a -> map (\b -> (a,b)) bs) as)


exponential :: (Ord v1) => (Ord e1) => (Ord v2) => (Ord e2) => Graph v1 e1 -> Graph v2 e2  -> Graph (Map v1 v2) ((Map v1 v2, Map v1 v2), Map e1 e2)
exponential g1 g2 =
    let v = enumerateMaps (vertex g1) (vertex g2) in
    let e = do
              phi <- v
              psi <- v
              let theta = (\(u,v) -> (edgeMap g2 (phi Data.Map.! u, psi Data.Map.! v))) . (edgeVertex g1)
              let e1 = edgeList g1
              case (mapM theta (edgeList g1)) of
                Nothing    -> [] 
                Just    e2 -> [((phi, psi), Data.Map.fromList (zip e1 e2))]
    in
    fromSets v e
        
         


{- Latex output -}

dot2texAttrValue :: String -> String
dot2texAttrValue = 
    (>>= f) 
    where
        f :: Char -> String
        f '"'  = "\"\""
        f c    = [c]

dot2texAttrList :: [(String, String)] -> String
dot2texAttrList attr =
  execWriter $ 
             if not (Data.List.null attr) then
               do
                 tell " ["
                 let kv = map (\(k, v) ->
                               k ++ "=\"" ++ (dot2texAttrValue v) ++ "\""
                              )
                              attr 
                 tell (intercalate ", " kv) 
                 tell "]"
             else
               return ()

dot2tex :: Graph v e ->
           (v -> String) ->
           (v -> [(String, String)]) ->
           (e -> [(String, String)]) ->
           String

dot2tex g vertexId vertexAttr edgeAttr = 
  execWriter $ do
      tell "digraph G {"
      forM_ (vertex g)
        (\v -> tell $ (vertexId v) ++ (dot2texAttrList (vertexAttr v)) ++ "\n")
      forM_ (edgeList g) 
        (\e ->
          do
            let (v1, v2) = edgeVertex g e
            let id1 = vertexId v1
            let id2 = vertexId v2
            tell $ id1 ++ " -> " ++ id2 ++ (dot2texAttrList (edgeAttr e)) ++ "\n"
        )
      tell "}"        
                
graphG :: BaseGraph String 
graphG = fromSets' ["a","b","c","d"] [("a","b"),("c","b"),("d","b")]

graph2 :: BaseGraph String 
graph2 = fromSets' ["1","2"] [("1","2")]

graph2toG :: BaseGraph (Map String String)
graph2toG = baseGraph $ exponential graphG graph2

idGen :: (Ord a) => [a] -> (a -> String)
idGen xs = 
        let keys = sort xs in
        let ids = map (("v" ++) . show) [0..] in
        let m = Data.Map.fromList $ zip keys ids  in
        (m !)

dot2texBase g = dot2tex g id (const []) (const [])

dot2texExponential :: (Ord a, Ord b) => Graph (Map a b) c -> (a -> String) -> (b -> String) -> String
dot2texExponential f showA showB =
    let showMap m =
          execWriter $ do
            tell "$\\begin{array}"
            tell $ "{" ++ (take (Data.Map.size m) $ repeat 'c') ++ "}"
            tell "\n"
            let k = intercalate " & " $ map (showA . fst) $ Data.Map.toList m
            let v = intercalate " & " $ map (showB . snd) $ Data.Map.toList m
            tell k
            tell " \\\\ \n"
            tell v
            tell "\n"
            tell "\\end{array}$\n"
     in
     let vertexId = idGen (vertex f) in
     dot2tex f vertexId  ((:[]) . ("texlbl",) . showMap)  (const [])  

main = do
         forM_
           [( "graph2toG.dot" , dot2texExponential graph2toG id id ),
            ( "graph2.dot", dot2texBase graph2 ),
            ( "graphG.dot", dot2texBase graphG )]
           (\(fname, tex) ->
              withFile fname WriteMode (\h -> hPutStrLn h tex))

         

            

