# uri-filepath

This library includes tools for converting `FilePath`s (i.e. `String`s) to and from `file` URIs as defined [RFC 8089](https://tools.ietf.org/html/rfc8089).

Since the `file` scheme is defined under the URI standard [RFC 3986](http://www.ietf.org/rfc/rfc3986.txt), we make use of the [network-uri](https://hackage.haskell.org/package/network-uri) package which implements that standard. Similarly, the [filepath](https://hackage.haskell.org/package/filepath) package provides support for manipulating cross-platform file paths, we use that on the other end.
