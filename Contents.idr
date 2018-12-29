module Contents

import Data.String.Extra

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

getSlug : BlogPost -> String
getSlug bp = dropLast 3 $ path bp

getContentBySlug : String -> IO String
getContentBySlug slug = do
  cd <- currentDir
  Right content <- readFile $ cd ++ "/raw/" ++ slug ++ ".md" | Left err => pure (show err)
  pure content