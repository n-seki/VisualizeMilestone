{-# LANGUAGE OverloadedStrings #-} 

module Main where

import Lib
import Control.Monad
import System.Environment (getArgs, lookupEnv)

main :: IO ()
main = do
    githubAccessToken <- getGitHubAccessToken
    args <- getArgs
    when (length args < 3) $ error "not enough arguments" 
    let owner = args !! 0
    let repository = args !! 1
    let milestone = args !! 2
    visualizeMilestone githubAccessToken owner repository milestone
    putStrLn ""

getGitHubAccessToken :: IO String
getGitHubAccessToken = do
    tokenMaybe <- lookupEnv "VISUALIZE_MILESTONE_GITHUB_TOKEN"
    case tokenMaybe of
        Nothing -> error "please register github access token to env"
        Just token -> return token