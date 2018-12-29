module Contents

import Data.String.Extra
import Language.JSON

%access public export

record BlogPost where
  constructor MkBlogPost
  title, path: String

contents : List BlogPost
contents = reverse $ (uncurry MkBlogPost) <$> [
    ("How is this blog put together", "blog-architecture.md")
    -- , ("Modeling domain with F# for strong specification", "modeling-with-fsharp.md")
    , ("LINQ, infinity, laziness and oh mine!", "linq-tips.md")
    -- , ("A lens look-alike (really a nested data updater) in C#?", "lens-csharp.md")
    , ("Lens (really record viewer / updater) in TypeScript", "lens-typescript.md")
    , ("Fin", "fin.md")
    , ("Coding an alternative Vect.index, Type-Driven Development in Idris", "index-fin-alternative.md")
    , ("callCC in Haskell, and my ultimate Monad", "call-cc-my-ultimate-monad.md")
    , ("My take on (unit) testing", "my-take-on-unit-testing.md")
    , ("Serialize like javascript - MergeJSON in Idris!", "serialize-like-javascript-the-prototype.md")
    , ("Serialize like javascript - the idea", "serialize-like-javascript.md")
    , ("foldl in terms of foldr", "foldr-in-foldl.md")
    , ("Don't null check, just continue!", "dont-pattern-match-just-pass-function.md")
    ]

about : BlogPost
about = MkBlogPost "About Hackle's blog" "about.md"

siteContents : List BlogPost
siteContents = about::contents

private
getSlug : BlogPost -> String
getSlug bp = dropLast 3 $ path bp

findPostBySlug : String -> BlogPost
findPostBySlug slug =
  case List.find eqSlug siteContents of
    Nothing => head contents
    Just p => p
where
  eqSlug : BlogPost -> Bool
  eqSlug p = slug == getSlug p

getContent : BlogPost -> IO String
getContent (MkBlogPost _ path) = do
  cd <- currentDir
  let fullPath = cd ++ "/raw/" ++ path
  Right content <- readFile fullPath  | Left err => pure ("not found " ++ fullPath ++ show err)
  pure content

record BlogResponse where
  constructor MkBlogResponse
  title, content : String
  posts : List BlogPost

encodeBlogResponse : BlogResponse -> JSON
encodeBlogResponse resp =
  JObject [
    ("title", JString $ title resp)
    , ("content", JString $ content resp)
    , ("posts", JArray $ map toPost $ posts resp)
  ]
  where
    toPost : BlogPost -> JSON
    toPost bp = JObject [
      ("title", JString $ title bp)
      , ("path", JString $ getSlug bp)
    ]
