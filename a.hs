class Person a where
    speak :: a -> String

class Person a => Student a where
    study :: a -> String

instance Person Integer where
    speak 0 = ""
    speak 1 = "hi"
    speak s = "hi, " ++ speak (s - 1)

instance Student Integer where
    study a = speak a


