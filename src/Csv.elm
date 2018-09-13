module Csv exposing
    ( Csv
    , parseWith, parse, split, splitWith
    )

{-| A CSV parser that supports different separators, and quoted fields.
The results are provided as lists.


## Definitions

@docs Csv


## Parsing functions

@docs parseWith, parse, split, splitWith

-}

import List
import Maybe
import String
import Debug


{-| The `Csv` type structure.
-}
type alias Csv =
    { headers : List String
    , records : List (List String)
    }


parse : String -> Csv
parse = parseWith ','



parseWith : Char -> String -> Csv
parseWith separator raw =
    let
        y      = Debug.log "raw"  raw
        values = Debug.log "values " (splitWith separator raw)
        headers =
            List.head values
                |> Maybe.withDefault []

        records = Debug.log "records" (List.drop 1 values)
    in
    { headers = headers
    , records = records
    }



split : String -> List (List String)
split = splitWith ','


splitWith : Char -> String -> List (List String)
splitWith separator raw =
  let
    state    = String.foldr accumulator (start separator) raw
    ss       = handleSeparator state 
    nContent = foldr (::) ss.content ss.cRow 
  in
    nContent
        


type alias State =
  { separator          : Char 
  , cValue             : Maybe String
  , cRow               : Maybe (List String)
  , content            : List  (List String)
  , inQuote            : Bool  
  , previousWasNewLine : Bool
  , previousWasEscape  : Bool 
  }

start s = { separator = s
        , cValue = Nothing
        , cRow   = Nothing
        , content = []
        , inQuote = False
        , previousWasNewLine = False 
        , previousWasEscape  = False }
 
accumulator :  Char -> State -> State
accumulator current state =
  let
    _ = 1 
  in
    case current  of 
      '\\' -> {state | previousWasEscape = True} 
      '"'  -> handleQuote current state 
      '\r' -> handleNewLine current state
      '\n' -> handleNewLine current state 
      _    -> handleCharacter current state 



handleQuote : Char -> State -> State 
handleQuote char state = 
  let
    y = Debug.log "char " char 
    x = Debug.log "input state " state  
  in 
  case state.previousWasEscape of 
    True  -> 
      let 
        nV =  case state.cValue of 
          Nothing  -> Just  (String.fromChar  char)
          Just cV  -> Just (cV ++ (String.fromChar char))
      in    
        {state | cValue = nV, previousWasNewLine = False }
    False -> 
      {state | inQuote = not state.inQuote, previousWasNewLine = False }  
   


handleNewLine : Char -> State -> State 
handleNewLine char s =  
  if s.inQuote then 
    appendCharacter char s
  else
    let       
      nContent = case (s.cValue,s.cRow) of 
        -- nothing to update 
        (Nothing, Nothing ) -> s.content
        -- we have content, and no row 
        (Just c , Nothing ) -> [String.reverse c] :: s.content
        -- we have a row but no conent 
        (Nothing, Just r  ) -> r :: s.content
        -- both content and a row 
        (Just c,  Just r  ) ->  
          let 
            rC   = String.reverse c
            nRow = rC :: r 
          in 
            nRow :: s.content
    in
    {s | content = nContent, cValue = Nothing, cRow = Nothing, previousWasNewLine = True }  


handleSeparator : State -> State 
handleSeparator s = 
  let 
    cRow = Maybe.withDefault [] s.cRow
    fRow v r = (String.reverse v) :: r
    nRow = foldr fRow cRow  s.cValue  
  in
    {s | cValue = Nothing, cRow = Just nRow, previousWasNewLine = False }     


handleCharacter : Char -> State -> State 
handleCharacter c s = 
  if c == s.separator && not s.inQuote then 
    handleSeparator s
  else      
    appendCharacter c s


appendCharacter : Char -> State -> State 
appendCharacter c s = 
  let 
    char   = String.fromChar c
    update = case s.cValue of 
      Nothing  -> Just char
      Just cV  -> Just (cV ++ char)
  in    
    {s | cValue = update, previousWasNewLine = False }      


foldr : (a -> b -> b) -> b -> Maybe a -> b
foldr fn acc m = 
  let
    r = Maybe.map (\a -> fn a acc) m
  in 
    Maybe.withDefault acc r  
