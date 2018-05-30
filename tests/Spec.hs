module Spec where

import Control.Monad (void)
import Data.Foldable (forM_)
import Data.Maybe (isJust, fromJust)
import Network.URI
import Network.URI.FilePath
import Test.Hspec
import TestCases

-- ---------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "URI --> file path" $ forM_ testCases checkURI
  describe "file path --> URI" $ forM_ testCases checkFilePath

checkURI :: TestCase -> Spec
checkURI (URITestCase os desc uri fp) = it (os ++ " -- " ++ desc) (platformAwareURIToFilePath os uri `shouldBe` fp)
checkURI _ = return ()

checkFilePath :: TestCase -> Spec
checkFilePath (FilePathTestCase os desc uri fp) = it (os ++ " -- " ++ desc) (platformAwareFilePathToURI os fp `shouldBe` uri)
checkFilePath _ = return ()
