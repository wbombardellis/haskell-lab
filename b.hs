class Speech a where
    speak :: a -> String

class Speech a => Discourse a where
    pronounce :: a -> String

instance Speech String where
    speak s = "Ladies and gentlemen, " ++ s

instance Discourse String where
    pronounce a = speak a ++ ". Thank you"

cthroat :: Speech a => a -> String
cthroat s = "ahum..." ++ speak s
