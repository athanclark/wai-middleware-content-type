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
import Network.Wai.Middleware.ContentType.Middleware (middleware)
import Network.Wai.Trans


myMiddleware :: MiddleareT (ReaderT Env m)

contentTypeRoutes :: MonadIO m => FileExtListenerT (MiddlewareT (ReaderT Env m)) (ReaderT Env m) ()
contentTypeRoutes = do
  blaze myBlazeResponse
  cassius myCassiusResponse
  text myTextResponse
  middleware Json myMiddleware


contentMiddleware :: MonadIO m => MiddlewareT (ReaderT Env m)
contentMiddleware = fileExtsToMiddleware contentTypeRoutes
```

Which you can then decompose into a `Middleware` and use in the rest of your Wai stack.
