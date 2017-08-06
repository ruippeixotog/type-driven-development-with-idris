module Chapter12_3

record Score where
  constructor MkScore
  correct : Nat
  attempted : Nat

record GameState where
  constructor MkGameState
  score : Score
  difficulty : Int

initState : GameState
initState = MkGameState (MkScore 0 0) 12

data Command : Type -> Type where
  PutStr : String -> Command ()
  GetLine : Command String
  
  GetRandom : Command Int
  GetGameState : Command GameState
  PutGameState : GameState -> Command ()
  
  Pure : ty -> Command ty
  Bind : Command a -> (a -> Command b) -> Command b

-- exercise 2
mutual
  Functor Command where
    map f fa = fa >>= (Pure . f)

  Applicative Command where
    pure = Pure
    (<*>) ff fa = do f <- ff
                     a <- fa
                     pure (f a)

  Monad Command where
    (>>=) = Bind

-- exercise 1
updateGameState : (GameState -> GameState) -> Command ()
updateGameState f = do state <- GetGameState
                       PutGameState (f state)

-- exercise 3
record Votes where
  constructor MkVotes
  upvotes : Integer
  downvotes : Integer

record Article where
  constructor MkArticle
  title : String
  url : String
  score : Votes


initPage : (title : String) -> (url : String) -> Article
initPage title url = MkArticle title url (MkVotes 0 0)

goodSite : Article
goodSite = MkArticle "Good Page" "http://example.com/good" (MkVotes 101 7)

badSite : Article
badSite = MkArticle "Bad Page" "http://example.com/bad" (MkVotes 5 47)

getScore : Article -> Integer
getScore a = (upvotes $ score a) - (downvotes $ score a)

-- exercise 4
addUpvote : Article -> Article
addUpvote = record { score->upvotes $= (+1) }

addDownvote : Article -> Article
addDownvote = record { score->downvotes $= (+1) }
