module Main exposing (..)

type alias Points = Int

type Grade = Pass | Fail

greg : Points
greg = 49

grade : Points -> Grade
grade p = if p>50 then Pass else Fail
--grade("greg")

newGreet : String -> String
newGreet name =
  "Hello " ++ name ++ "! " ++ "THIS IS NEW GREET" 
-- newGreet "george"

--************************************************
--https://guide.elm-lang.org/core_language.html
--************************************************

-- Functions
-- If Expressions
--greet : String -> String
greet name =
  if name == "Neo" then "YOU ARE THE CHOSEN ONE"
  else
    "Hello " ++ name ++ "!"
  --greet "george"

madlib animal adjective = 
  "The ostentatious " ++ animal ++ " wears " ++ adjective ++ " shorts."
--madlib "cat" "kekw"

-- Lists
namesList =
  ["Alice", "Bob", "Chuck"]
-- List.isEmpty namesList
-- List.length namesList
-- List.reverse namesList

numList = 
  [4,3,2,1]
-- List.sort numbers

increment n =
  n + 1
-- List.map increment numList

-- Tuples
isGoodName name = 
  if String.length name <= 20 then
    (True, "name accepted!")
  else
    (False, "name was too long please limit to 20 characters")
-- isGoodName "Tom"

-- Records
john =
  {
    first = "John",
    last = "Hobson",
    age = 81
  }
-- john.first / .first john / List.map .first[john, john, john]
-- john.last / .last john / List.map .last[john, john, john]
-- john.age / .age john / List.map .age[john, john, john]

johnny =
  {
    first = "Johnny",
    last = "JonJon",
    age = 71
  }
  -- List.map .age[john, johnny]

celebrateBirthday name =
  {name | age = name.age + 1}
-- celebrateBirthday john