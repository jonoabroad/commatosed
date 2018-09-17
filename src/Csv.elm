module Csv exposing
    ( Csv
    , parseWith, parse
    )

{-| A CSV parser that supports different separators, quoted fields, and multiline values.
The results are provided as lists.


## Definitions

@docs Csv


## Parsing functions

@docs parseWith, parse

-}

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
        values = List.reverse (splitWith separator raw)
    in    
      case values of
          []    -> { headers = [], records = []}
          x::xs -> { headers = x,  records = xs}  



splitWith : Char -> String -> List (List String)
splitWith separator raw =
  let
    state    = String.foldl accumulator (start separator) raw
    ss       = handleSeparator True state 
    sRow     = Maybe.map (\a -> (::) (List.reverse a) ss.content) ss.cRow
    nContent = Maybe.withDefault ss.content sRow    
  in
    nContent
        

 

type alias State =
  { separator          : Char 
  , cValue             : Maybe String
  , cRow               : Maybe (List String)
  , content            : List  (List String)
  , inQuote            : Bool  
  , previousChar       : Maybe Char   
  }

start s = { separator = s
        , cValue = Nothing
        , cRow   = Nothing
        , content = []
        , inQuote = False
        , previousChar = Nothing}
 

accumulator :  Char -> State -> State
accumulator current state =
  let 
    newState = case current  of  
      '"'  -> handleQuote state        
      '\r' -> handleNewLine current state
      '\n' -> handleNewLine current state 
      _    -> handleCharacter current state 
  in 
    {newState | previousChar = Just current}


handleQuote : State -> State 
handleQuote state = 
  case state.inQuote of 
    True  -> inQuote  state 
    False -> outQuote state
   

inQuote : State -> State 
inQuote state = 
  case state.previousChar of 
    Nothing -> {state | inQuote = False} 
    Just ch ->
      if ch == '"' || ch == '\\' then
        let 
          nV =  Maybe.map  (\ v -> v ++ "\"")  state.cValue
        in   
          {state | cValue = nV,  inQuote = False}
      else   
        {state | inQuote = False}


outQuote :State -> State 
outQuote state = 
  case state.previousChar of 
    Nothing -> {state | inQuote = True}  
    Just prev -> 
      if prev == '"' || prev == '\\'  then 
        case state.cValue of 
          Nothing -> state  
          Just cv ->
            let
              trimmed =  String.slice 0 -1 cv
              ncVale  = trimmed ++ "\""
            in 
              {state | cValue = (Just ncVale)}
      else      
        {state | inQuote = True}



handleNewLine : Char -> State -> State 
handleNewLine char s =  
  if s.inQuote then 
    appendCharacter char s
  else
    let       
      nContent = case (s.cValue,s.cRow) of 
        (Nothing, Nothing ) -> s.content
        (Just c , Nothing ) -> [c]  :: s.content
        (Nothing, Just r  ) -> List.reverse("" :: r) :: s.content
        (Just c,  Just r  ) ->  
          let 
            nRow = List.reverse(c :: r ) 
          in 
            nRow :: s.content
    in
    {s | content = nContent, cValue = Nothing, cRow = Nothing}  


handleSeparator : Bool -> State -> State 
handleSeparator end s = 
  let 
    fRow mv mr =
      case (mv,mr) of 
        (Nothing, Nothing ) -> 
          case end of 
            True  ->  Nothing 
            False ->  Just [""] 
        (Just v , Nothing ) -> Just [v] 
        (Nothing, Just r  ) -> Just ("" :: r)
        (Just v,  Just r  ) -> Just (v :: r)
    nRow = fRow s.cValue s.cRow  
  in
    {s | cValue = Nothing, cRow = nRow}     


handleCharacter : Char -> State -> State 
handleCharacter c s = 
  if c == s.separator && not s.inQuote then 
    handleSeparator False s
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
    {s | cValue = update}      


