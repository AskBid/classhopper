module Scene.Common where 

newtype ObjectId = ObjectId Int 
  deriving (Eq, Ord, Show)
