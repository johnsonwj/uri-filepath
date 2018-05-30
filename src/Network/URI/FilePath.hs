{-|
Description : Conversions to and from RFC-8089 file URIs.
Copyright   : 2018 William Johnson
License     : MIT
Maintainer  : @johnsonwj
Stability   : experimental

This library includes tools for converting `FilePath`s (i.e. `String`s) to and from `file` URIs as defined in
 [RFC 8089](https://tools.ietf.org/html/rfc8089).

Since the `file` scheme is defined under the URI standard [RFC 3986](https://tools.ietf.org/html/rfc3986), we make use
of the [network-uri](https://hackage.haskell.org/package/network-uri) package which implements that standard.
Similarly, the [filepath](https://hackage.haskell.org/package/filepath) package provides support for manipulating
cross-platform file paths, so we use that on the other end.
-}

module Network.URI.FilePath
  ( uriToFilePath
  , filePathToURI

    -- * Utilities
  , isLocalAbsoluteFileURI
  , isLocalAbsoluteFilePath

    -- * Platform Aware Functions
    --
    -- | These functions take an additional argument specifying the operating system that should be targeted. They
    -- are mostly meant to support testing regardless of which platform the tests are run on. The platform should
    -- be specified as if it came from 'System.Info.os', e.g. @"mingw32"@ for Windows.
  , SystemOS
  , platformAwareURIToFilePath
  , platformAwareFilePathToURI
  ) where

import qualified System.Info

import           Network.URI
import qualified System.Directory         as D
import qualified System.FilePath.Posix    as FPP
import qualified System.FilePath.Windows  as FPW

-- Note that the \'scheme\' according to RFC3986 is the bit *before* the
-- colon, but the implementation in "Network.URI" includes it.
fileScheme :: String
fileScheme = "file:"

-- If the URI contains an authority, any value other than \"localhost\" is
-- interpreted as a file not on the local machine. Since we are attempting to
-- convert the URI into a local file path, any other value will return 'Nothing'
-- from 'uriToFilePath'.
localhost :: String
localhost = "localhost"

-- | A @String@ representation of the system OS, as in 'System.Info.os'.
-- Assumed to be @\"mingw32\"@ for any flavor of Windows, and everything else
-- is assumed to be POSIX-compliant.
type SystemOS = String

windowsOS :: SystemOS
windowsOS = "mingw32"

-- | Convert a URI to an absolute file path. If the URI does not contain an absolute reference, returns 'Nothing'.
uriToFilePath :: String -> Maybe FilePath
uriToFilePath = platformAwareURIToFilePath System.Info.os

platformAwareURIToFilePath :: SystemOS -> String -> Maybe FilePath
platformAwareURIToFilePath systemOS uri = do
    parsedURI <- parseAbsoluteURI uri
    if isLocalAbsoluteFileURI parsedURI
    then return $ adjustAndParsePath parsedURI
    else Nothing
  where
    adjustAndParsePath = platformAdjustFromURIPath systemOS . unEscapeString . uriPath
    
platformAdjustFromURIPath :: SystemOS -> String -> FilePath
platformAdjustFromURIPath systemOS
  | systemOS /= windowsOS = id
  | otherwise             = windowsPathFromURIPath

windowsPathFromURIPath :: String -> FilePath
windowsPathFromURIPath srcURIPath = FPW.joinDrive (toDrive firstSegment) (FPW.joinPath rest)
  where
    -- Drop leading '/' for absolute Windows paths; use FilePath.Posix to split a URI path.
    firstSegment:rest = (FPP.splitDirectories . tail) srcURIPath

    toDrive pathSegment
      | FPW.isDrive pathSegment = FPW.addTrailingPathSeparator pathSegment
      | otherwise               = pathSegment
        

-- | Convert an absolute file path to a URI. Returns 'Nothing' if the provided file path does not represent an
-- absolute path to a file on the local machine (see 'Network.URI.FilePath.isLocalAbsoluteFilePath')
--
-- If you do not know whether or not the path is absolute, you can use 'System.Directory.makeAbsolute` to do so
-- (but keep in mind that this involves `IO`).
filePathToURI :: FilePath -> Maybe String
filePathToURI = platformAwareFilePathToURI System.Info.os

platformAwareFilePathToURI :: SystemOS -> FilePath -> Maybe String
platformAwareFilePathToURI os fp
  | not (isLocalAbsoluteFilePath os fp) = Nothing
  | otherwise                           = Just (makeURI fp)
  where
    makeURI fp = show URI { uriScheme = fileScheme
                          , uriAuthority = Just $ URIAuth "" "" ""
                          , uriPath = platformAdjustToURIPath os fp
                          , uriQuery = ""
                          , uriFragment = ""
                          }

platformAdjustToURIPath :: SystemOS -> FilePath -> String
platformAdjustToURIPath systemOS
  | systemOS /= windowsOS = id
  | otherwise             = windowsPathToURIPath

windowsPathToURIPath :: FilePath -> String
windowsPathToURIPath fp = '/' : FPP.joinPath (escapedDrive : rest)
  where
    drive:rest = FPW.splitDirectories fp

    leaveCharUnescaped = (/= ':')
    removePathSeparator = filter (not . FPW.isPathSeparator)
    escapedDrive = removePathSeparator $ escapeURIString leaveCharUnescaped drive

-- | Determines if the provided URI refers to a file on the local machine:
--
--      * The URI scheme is @file@
--      * The URI has no authority section; the authority section has no hostname; *or* the hostname is @localhost@.
--      * The URI path segment component is *absolute*: in RFC3986 this means it must start with a @/@.
isLocalAbsoluteFileURI :: URI -> Bool
isLocalAbsoluteFileURI uri = (uriScheme uri == fileScheme) && (checkAuth . uriAuthority) uri && (checkPath . uriPath) uri
  where
    checkAuth Nothing     = True
    checkAuth (Just auth) = null (uriRegName auth) || uriRegName auth == localhost
    
    checkPath ('/':_) = True
    checkPath _       = False

-- | Determines if the given file path represents an absolute path to a file on the local machine:
--
--      * The file path is absolute (based on 'System.FilePath.isAbsolute')
--      * The file path does not begin with @\\\\@ (Windows) or @//@ (POSIX)
isLocalAbsoluteFilePath :: SystemOS -> FilePath -> Bool
isLocalAbsoluteFilePath os fp
  | os == windowsOS = not (isUNCPath fp)      && FPW.isAbsolute fp
  | otherwise       = not (isDoubleSlash fp)  && FPP.isAbsolute fp
  where
    isUNCPath s     = take 2 s == "\\\\"
    isDoubleSlash s = take 2 s == "//"
