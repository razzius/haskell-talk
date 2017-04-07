{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (putStrLn)

import Data.ByteString.Char8 (pack)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Text.Internal (Text)
import Data.Text.IO (putStrLn)
import Control.Monad

import System.Environment (lookupEnv)
import System.Exit (exitFailure)

import Network.Wreq (getWith, defaults, header, param, responseBody)
import Control.Lens ((^.), (.~), (&))
import Data.Aeson.Lens (key, _String, nth)


jql = "assignee = razzi\
      \ AND status in (\"In Development\", \"Ready For Development\")\
      \ AND issuetype != epic and sprint in opensprints()\
      \ ORDER BY status, sprint"


url = "https://sighten.atlassian.net/rest/api/2/search"


opts auth = defaults & header "Authorization" .~ [pack auth]
                     & param "maxResults" .~ ["1"]
                     & param "jql" .~ [jql]


getIssue :: String -> IO Text
getIssue auth = getWith (opts auth) url
  >>= \response ->
    return $ response ^.
      responseBody . key "issues" . nth 0 . key "fields" . key "description" . _String

main = do
  jiraAuth <- lookupEnv "JIRA_AUTH"

  case jiraAuth of
    Nothing ->
      putStrLn "Need to set JIRA_AUTH" >>
      exitFailure
    Just auth -> getIssue auth >>= putStrLn
