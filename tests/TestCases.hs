{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestCases (TestCase(..), testCases) where

import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap, (!), keys)
import           Data.Maybe (fromJust)
import           Data.Yaml (FromJSON(..), (.:), decode)
import qualified Data.Yaml as Y
import qualified Data.Yaml as YV (Value(..)) 
import           Text.RawString.QQ 

data TestCase = URITestCase String String String (Maybe String)
              | FilePathTestCase String String (Maybe String) String

testCases :: [TestCase]
testCases = [makeTestCase os t tcs | os <- keys stubs, t <- keys (stubs ! os), tcs <- (stubs ! os ! t)]
  where
    stubs :: HashMap String (HashMap String [TestCaseStub])
    stubs = fromJust (decode testCasesYaml)

testCasesYaml :: ByteString
testCasesYaml = [r|
posix:
  uri:
    - description: Absolute URI Path, Empty Authority
      uri: file:///home/myself/Example.hs
      filepath: /home/myself/Example.hs
    - description: Absolute URI Path, No Authority
      uri: file:/home/myself/Example.hs
      filepath: /home/myself/Example.hs
    - description: Absolute URI Path, localhost
      uri: file://localhost/home/myself/Example.hs
      filepath: /home/myself/Example.hs
    - description: Absolute URI Path, Network Hostname
      uri: file://somewhere/home/myself/Example.hs
      filepath: null
    - description: Relative URI Path, Empty Authority
      uri: file://Example.hs
      filepath: null
    - description: Relative URI Path, No Authority
      uri: file:Example.hs
      filepath: null
  filepath:
    - description: Absolute Path
      uri: file:///home/myself/Example.hs
      filepath: /home/myself/Example.hs
    - description: Relative Path 1
      uri: null
      filepath: Example.hs
    - description: Relative Path 2
      uri: null
      filepath: ./Example.hs
    - description: Double Slash Root
      uri: null
      filepath: //home/myself/Example.hs

mingw32:
  uri:
    - description: Absolute URI Path, Empty Authority
      uri: file:///c%3A/Users/myself/Example.hs
      filepath: c:\Users\myself\Example.hs
    - description: Absolute URI Path, No Authority
      uri: file:/c%3A/Users/myself/Example.hs
      filepath: c:\Users\myself\Example.hs
    - description: Absolute URI Path, localhost
      uri: file://localhost/c%3A/Users/myself/Example.hs
      filepath: c:\Users\myself\Example.hs
    - description: Absolute URI Path, Network Hostname
      uri: file://somewhere/c%3A/Users/myself/Example.hs
      filepath: null
    - description: Relative URI Path, Empty Authority
      uri: file://Example.hs
      filepath: null
    - description: Relative URI Path, No Authority
      uri: file:Example.hs
      filepath: null
  filepath:
    - description: Absolute Path
      uri: file:///c%3A/Users/myself/Example.hs
      filepath: c:\Users\myself\Example.hs
    - description: Relative Path 1
      uri: null
      filepath: Example.hs
    - description: Relative Path 2
      uri: null
      filepath: .\Example.hs
    - description: Network Path
      uri: null
      filepath: \\somewhere\Example.hs
    - description: Relative Path, Different Drive
      uri: null
      filepath: e:Example.hs
    
|]

makeTestCase :: String -> String -> TestCaseStub -> TestCase
makeTestCase os "uri" (TCS d u f) = URITestCase os d (fromJust u) f
makeTestCase os "filepath" (TCS d u f) = FilePathTestCase os d u (fromJust f)

data TestCaseStub = TCS String (Maybe String) (Maybe FilePath)

instance FromJSON TestCaseStub where
  parseJSON (Y.Object v) = TCS <$> v .: "description" <*> v .: "uri" <*> v .: "filepath"
  parseJSON _            = fail "expected object"
