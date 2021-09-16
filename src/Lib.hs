{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( visualizeMilestone 
    ) where

import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Data.List
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import GHC.Generics
import Data.String.Conversions
import qualified Data.Map as M

instance FromJSON Issue
instance FromJSON User

data Issue = Issue
  { number :: Int
  , title :: String
  , html_url :: String
  , assignees :: [User]
  , state :: String
  } deriving (Show, Generic)

data User = User { login :: String } deriving (Show, Generic)

data MilestoneSummary = MilestoneSummary
  { closeIssue :: [Issue]
  , openIssue :: [Issue]
  , achievement :: (Int, Int) -- (close issue count, total issue count)
  , achievementByAssingees :: M.Map String (Int, Int)
  } deriving Show

isOpen :: Issue -> Bool
isOpen issue = state issue == "open"

githubHost :: BC.ByteString
githubHost = "api.github.com" 

visualizeMilestone :: String -> String -> String -> String -> IO () 
visualizeMilestone token owner repository milestone = do
    issues <- allIssue (convertString token) (convertString owner) (convertString repository) (convertString milestone)
    case issues of
        Nothing -> error "Failed in deserialize!"
        Just iss -> report $! summarize iss 

summarize :: [Issue] -> MilestoneSummary
summarize issues = MilestoneSummary closeIssue openIssue achievement $! achievementByAssignee issues
    where
        closeIssue = filter (not . isOpen) issues
        openIssue = filter isOpen issues
        achievement = (length closeIssue, length issues)

-- Issue List -> Map [assignee name] (close issue count, issue count)
achievementByAssignee :: [Issue] -> M.Map String (Int, Int)
achievementByAssignee issues = toAchievement $! issueByAssinee issues
    where
        toAchievement :: M.Map String [Issue] -> M.Map String (Int, Int)
        toAchievement issueMap = foldl (\ m (userName, iss) -> M.insert userName (countCloseIssue iss, length iss) m) M.empty $ M.assocs issueMap

        countCloseIssue iss = length $! filter (not . isOpen) iss

-- Issue List -> Map [assignee name] Issue List
issueByAssinee :: [Issue] -> M.Map String [Issue]
issueByAssinee issues = foldl (\ m issue -> insert' m issue) M.empty issues
    where
        insert' :: M.Map String [Issue] -> Issue -> M.Map String [Issue] 
        insert' m issue 
            | null $! assignees issue = M.insertWith (++) "-" [issue] m
            | otherwise = foldl (\ m' user -> M.insertWith (++) (login user) [issue] m') m $! assignees issue

report :: MilestoneSummary -> IO ()
report milestone = do
    putStrLn "== Milestone Summary =="
    putStrLn $! "Total Archievement  " ++ (show . fst $! achievement milestone) ++ "/" ++ (show . snd $! achievement milestone)
    putStrLn ""
    putStrLn "Archievement By User"
    putStrLn $! intercalate "\n" $! map achievementByUserLine $ M.assocs $ achievementByAssingees milestone
    putStrLn ""
    putStrLn "Open Issues"
    putStrLn $! intercalate "\n" $! map issueLine $! openIssue milestone
    putStrLn ""
    putStrLn "Close Issues"
    putStrLn $! intercalate "\n" $! map issueLine $! closeIssue milestone

achievementByUserLine :: (String, (Int, Int)) -> String
achievementByUserLine (name, (closeIssueCount, totalIssueCount)) = "  " ++ name ++ replicate spaceCount ' ' ++ show closeIssueCount ++ "/" ++ show totalIssueCount
    where
        spaceCount :: Int 
        spaceCount = 18 - length name

issueLine :: Issue -> String
issueLine issue = "  " ++ title issue ++ "(" ++ html_url issue ++ ")"

allIssue :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> IO (Maybe [Issue])
allIssue token owner repository milestone = do
    response <- requestAllIssue token owner repository milestone
    let jsonBody = getResponseBody response
    if getResponseStatusCode response == 200
    then 
        return $! do decode jsonBody :: Maybe [Issue]
    else
        error $! convertString jsonBody

-- see https://docs.github.com/en/rest/reference/issues
requestAllIssue :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> IO (Response L.ByteString)
requestAllIssue token owner repository milestone = do
    let url = mconcat ["/repos/", owner, "/", repository, "/issues"]
    let queries = [("milestone", Just milestone), ("state", Just "all")]
    let request = buildRequest "GET" githubHost url queries $! buildRequestHeaders token owner
    httpLBS request

-- see https://docs.github.com/en/rest/guides/getting-started-with-the-rest-api#authentication
buildRequestHeaders :: BC.ByteString -> BC.ByteString -> [(HeaderName, BC.ByteString)]
buildRequestHeaders token owner = [
         (hAccept, "application/vnd.github.v3+json"),
         (hAuthorization, mconcat ["token ", token]),
         (hUserAgent, owner)]

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Query -> [(HeaderName, BC.ByteString)] -> Request
buildRequest method host path queries headers = setRequestMethod method
                                              $ setRequestHost host
                                              $ setRequestPath path
                                              $ setRequestHeaders headers
                                              $ setRequestQueryString queries
                                              $ setRequestSecure True
                                              $ setRequestPort 443
                                              $ defaultRequest