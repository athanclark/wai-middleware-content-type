wai-middleware-content-type
===========================

Route middlewares based on the incoming `Accept` HTTP header,
and other hints like the file extension (`foo.txt`) of the route
requested.

## Usage

This package provides many combinators for turning various data
types into the response you'd expect. For instance,
[blaze-html](https://hackage.haskell.org/package/blaze-html) gives
us _strictly_ `Html` data, right? We can be sure to only respond
with `Html`-compatible requests with our toolset:

```haskell
import Network.Wai.Middleware.ContentType
import Network.Wai.Trans


myMiddleware :: MiddlewareT (ReaderT Env m)

contentTypeRoutes :: Monad m =>
                     FileExtListenerT urlbase (Status -> ResponseHeaders -> Response) m ()
contentTypeRoutes = do
  blaze myBlazeResponse
  cassius myCassiusResponse
  text myTextResponse


contentMiddleware :: Monad m => MiddlewareT m
contentMiddleware app req respond =
  fileExtsToMiddleware $
    mapResponse (\f -> f status200 []) contentTypeRoutes
```


This library was designed for use with [nested-routes](https://hackage.haskell.org/package/nested-routes),
but it's all good if you want to use it separately.
