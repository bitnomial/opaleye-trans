{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Arrow
import           Control.Monad              (void)

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import qualified Database.PostgreSQL.Simple as PSQL

import           Opaleye
import           Opaleye.Trans


data Rose a
    = Node a [Rose a]
    | Leaf a
    deriving (Show, Eq)


instance Functor Rose where
    fmap f (Node x rs) = Node (f x) (map (fmap f) rs)
    fmap f (Leaf x) = Leaf (f x)


data NodeP i b a = NodeP
    { nodeId       :: i
    , nodeBranchId :: b
    , value        :: a
    } deriving (Show, Eq)


data NodeBranchP i n = NodeBranchP
    { nodeIdRef    :: i
    , nextBranchId :: n
    } deriving (Show, Eq)


type WriteNode a = NodeP (Maybe (Column PGInt4)) (Column PGInt4) (Column a)
type ReadNode a = NodeP (Column PGInt4) (Column PGInt4) (Column a)
type NodeBranch = NodeBranchP (Column PGInt4) (Column PGInt4)
type NullableNodeBranch = NodeBranchP (Column (Nullable PGInt4)) (Column (Nullable PGInt4))


data BranchP i = BranchP
    { branchId :: i
    } deriving (Show, Eq)


type WriteBranch = BranchP (Maybe (Column PGInt4))
type ReadBranch = BranchP (Column PGInt4)


data TreeP i r = TreeP
    { treeId :: i
    , rootId :: r
    } deriving (Show, Eq)


type WriteTree = TreeP (Maybe (Column PGInt4)) (Column PGInt4)
type ReadTree = TreeP (Column PGInt4) (Column PGInt4)


makeAdaptorAndInstance "pNode" ''NodeP
makeAdaptorAndInstance "pNodeBranch" ''NodeBranchP
makeAdaptorAndInstance "pBranch" ''BranchP
makeAdaptorAndInstance "pTree" ''TreeP


nodeTable :: Table (WriteNode a) (ReadNode a)
nodeTable = Table "node" $ pNode NodeP
    { nodeId = optional "id"
    , nodeBranchId = required "branch_id"
    , value = required "value"
    }


nodeBranchTable :: Table NodeBranch NodeBranch
nodeBranchTable = Table "node_branch" $ pNodeBranch NodeBranchP
    { nodeIdRef = required "id"
    , nextBranchId = required "next_branch_id"
    }


branchTable :: Table WriteBranch ReadBranch
branchTable = Table "branch" $ pBranch (BranchP (optional "id"))


treeTable :: Table WriteTree ReadTree
treeTable = Table "rosetree" $ pTree TreeP
    { treeId = optional "id"
    , rootId = required "root_id"
    }


newTree :: Int -> Transaction (Maybe Int)
newTree rootId =
    insertReturningFirst treeTable treeId (TreeP Nothing (pgInt4 rootId))


newBranch :: Transaction (Maybe Int)
newBranch = insertReturningFirst branchTable branchId (BranchP Nothing)


insertNode :: Int -> Maybe Int -> Int -> Transaction (Maybe Int)
insertNode bid (Just nbid) x = do
    Just nodeId <- insertReturningFirst nodeTable nodeId
        (NodeP Nothing (pgInt4 bid) (pgInt4 x))
    insert nodeBranchTable (NodeBranchP (pgInt4 nodeId) (pgInt4 nbid))
    return (Just nodeId)
insertNode bid Nothing x =
    insertReturningFirst nodeTable nodeId
        (NodeP Nothing (pgInt4 bid) (pgInt4 x))


insertTree :: MonadIO m => Rose Int -> OpaleyeT m Int
insertTree (Node x xs) = transaction $ do
    Just bid <- newBranch
    Just rootId <- insertNode 0 (Just bid) x
    Just treeId <- newTree rootId

    mapM_ (insertTree' bid) xs

    return treeId
insertTree (Leaf x) = transaction $ do
    Just rootId <- insertNode 0 Nothing x
    Just treeId <- newTree rootId
    return treeId


insertTree' :: Int -> Rose Int -> Transaction ()
insertTree' bid (Node x xs) = do
    Just nbid <- newBranch
    insertNode bid (Just nbid) x
    mapM_ (insertTree' nbid) xs
insertTree' bid (Leaf x) =
    void (insertNode bid Nothing x)


-- TODO Wrong order
selectTree :: Int -> Transaction (Rose Int)
selectTree treeId = do
    Just rootId <- selectRootNode treeId
    Just (NodeP _ _ x, NodeBranchP _ mbid) <- selectNode rootId
    case mbid of
        Just nbid -> do
            xs <- selectBranch nbid
            return (Node x xs)
        Nothing -> return (Leaf x)


selectRootNode :: Int -> Transaction (Maybe Int)
selectRootNode tid = queryFirst rootNode
  where
    rootNode :: Query (Column PGInt4)
    rootNode = proc () -> do
        tree <- queryTable treeTable -< ()
        restrict -< treeId tree .== pgInt4 tid
        returnA -< rootId tree


nodeAndBranch :: Query (ReadNode PGInt4, NullableNodeBranch)
nodeAndBranch = leftJoin allNodes allNodeBranches (uncurry idsEqual)
  where
    allNodes = queryTable nodeTable
    allNodeBranches = queryTable nodeBranchTable

    idsEqual :: ReadNode PGInt4 -> NodeBranch -> Column PGBool
    idsEqual (NodeP nid' _ _) (NodeBranchP nbid _) = nid' .== nbid


byId :: Query a -> (a -> Column PGInt4) -> Int -> Query a
byId q getId id' = proc () -> do
    row <- q -< ()
    restrict -< getId row .== pgInt4 id'
    returnA -< row


selectNode :: Int -> Transaction (Maybe (NodeP Int Int Int, NodeBranchP (Maybe Int) (Maybe Int)))
selectNode nid = queryFirst nodeById
  where
    nodeById :: Query (ReadNode PGInt4, NullableNodeBranch)
    nodeById = byId nodeAndBranch (nodeId . fst) nid


selectBranch :: Int -> Transaction [Rose Int]
selectBranch bid = do
    nodes <- query nodeByBranchId
    sequence (mkNode <$> nodes)
  where
    nodeByBranchId :: Query (ReadNode PGInt4, NullableNodeBranch)
    nodeByBranchId = byId nodeAndBranch (nodeBranchId . fst) bid
    
    mkNode 
        :: (NodeP Int Int Int, NodeBranchP (Maybe Int) (Maybe Int)) 
        -> Transaction (Rose Int)
    mkNode (NodeP _ _ x, NodeBranchP Nothing Nothing) = return (Leaf x)
    mkNode (NodeP _ _ x, NodeBranchP _ (Just nbid)) = do
        xs <- selectBranch nbid
        return (Node x xs)
    mkNode x = error $ "Bad branch select. Who knows?: " ++ show x


main :: IO ()
main = do
    conn <- PSQL.connectPostgreSQL "dbname='rosetree' user='postgres'"

    let tree :: Rose Int
        tree =
            Node 6
                [ Node 7
                    [ Node 1425
                        [ Leaf 42
                        , Leaf 2354
                        , Leaf 4245]]
                , Leaf 10
                , Node 6
                    [ Leaf 12
                    , Leaf 14]]

    tree' <- runOpaleyeT conn $ run . selectTree =<< insertTree tree

    print tree
    print tree'
    print (tree == tree')
