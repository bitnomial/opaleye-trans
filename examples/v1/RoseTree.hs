{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Control.Arrow

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import qualified Database.PostgreSQL.Simple as PSQL

import           Opaleye
import           Opaleye.Trans


data Rose a = Node a [Rose a]
    deriving (Show, Eq)


instance Functor Rose where
    fmap f (Node x rs) = Node (f x) (map (fmap f) rs)


data NodeP i b n a = NodeP
    { nodeId       :: i
    , nodeBranchId :: b
    , nextBranchId :: n
    , value        :: a
    } deriving (Show, Eq)


type WriteNode a = NodeP (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4) (Column a)
type ReadNode a = NodeP (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column a)


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
makeAdaptorAndInstance "pBranch" ''BranchP
makeAdaptorAndInstance "pTree" ''TreeP


nodeTable :: Table (WriteNode a) (ReadNode a)
nodeTable = Table "node" $ pNode NodeP
    { nodeId = optional "id"
    , nodeBranchId = required "branch_id"
    , nextBranchId = required "next_branch_id"
    , value = required "value"
    }


branchTable :: Table WriteBranch ReadBranch
branchTable = Table "branch" $ pBranch (BranchP (optional "id"))


treeTable :: Table WriteTree ReadTree
treeTable = Table "rosetree" $ pTree TreeP
    { treeId = optional "id"
    , rootId = required "root_id"
    }


newTree :: Int -> Transaction (Maybe Int)
newTree rootId = insertReturningFirst treeTable treeId (TreeP Nothing (pgInt4 rootId))


newBranch :: Transaction (Maybe Int)
newBranch = insertReturningFirst branchTable branchId (BranchP Nothing)


insertNode :: Int -> Int -> Int -> Transaction (Maybe Int)
insertNode bid nbid x =
    insertReturningFirst nodeTable nodeId
        (NodeP Nothing (pgInt4 bid) (pgInt4 nbid) (pgInt4 x))


insertTree :: MonadIO m => Rose Int -> OpaleyeT m Int
insertTree (Node x xs) = transaction $ do
    Just bid <- newBranch
    Just rootId <- insertNode 0 bid x
    Just treeId <- newTree rootId

    mapM_ (insertTree' bid) xs

    return treeId


insertTree' :: Int -> Rose Int -> Transaction ()
insertTree' bid (Node x xs) = do
    Just nbid <- newBranch
    insertNode bid nbid x
    mapM_ (insertTree' nbid) xs


selectTree :: Int -> Transaction (Rose Int)
selectTree treeId = do
    Just rootId <- selectRootNode treeId
    Just (NodeP _ _ nbid x) <- selectNode rootId
    xs <- selectBranch nbid
    return (Node x xs)


selectRootNode :: Int -> Transaction (Maybe Int)
selectRootNode tid = queryFirst rootNode
  where
    rootNode :: Query (Column PGInt4)
    rootNode = proc () -> do
        tree <- queryTable treeTable -< ()
        restrict -< treeId tree .== pgInt4 tid
        returnA -< rootId tree


selectNode :: Int -> Transaction (Maybe (NodeP Int Int Int Int))
selectNode nid = queryFirst nodeById
  where
    nodeById :: Query (ReadNode PGInt4)
    nodeById = proc () -> do
        node <- queryTable nodeTable -< ()
        restrict -< nodeId node .== pgInt4 nid
        returnA -< node


selectBranch :: Int -> Transaction [Rose Int]
selectBranch bid = do
    nodes <- query nodeByBranchId
    sequence (mkNode <$> nodes)
  where
    nodeByBranchId :: Query (ReadNode PGInt4)
    nodeByBranchId = proc () -> do
        row@(NodeP _ bid' _ _) <- queryTable nodeTable -< ()
        restrict -< bid' .== pgInt4 bid
        returnA -< row

    mkNode :: NodeP Int Int Int Int -> Transaction (Rose Int)
    mkNode (NodeP _ _ nbid x) = do
        xs <- selectBranch nbid
        return (Node x xs)


main :: IO ()
main = do
    conn <- PSQL.connectPostgreSQL "dbname='rosetree' user='postgres'"

    let tree :: Rose Int
        tree =
            Node 6
                [ Node 7
                    [ Node 1425
                        [ Node 42 []
                        , Node 2354 []
                        , Node 4245 []]]
                , Node 10 []
                , Node 6
                    [ Node 12 []
                    , Node 14 []]]

    tree' <- runOpaleyeT conn $ run . selectTree =<< insertTree tree

    print (tree, tree')
    print (tree == tree')
