module Csv exposing
    ( Csv
    , parseWith, parse
    )

{-| A CSV parser that supports different separators, and quoted fields.
The results are provided as lists.


## Definitions

@docs Csv


## Parsing functions

@docs parseWith, parse

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
        values = List.reverse (splitWith separator raw)
        headers =
            List.head values
                |> Maybe.withDefault []
        records = List.drop 1 values
    in
    { headers = headers
    , records = records
    }



splitWith : Char -> String -> List (List String)
splitWith separator raw =
  let
    state    = Debug.log "splitWith" (String.foldl accumulator (start separator) raw)
    ss       = handleSeparator state 
    sRow     = Maybe.map List.reverse ss.cRow 
    nContent = foldr (::) ss.content sRow 
  in
    nContent
        
foldr : (a -> b -> b) -> b -> Maybe a -> b
foldr fn acc m = 
  let
    r = Maybe.map (\a -> fn a acc) m
  in 
    Maybe.withDefault acc r  


-- ideally cValue and previousChar would live in the same Maybe, but for the moment I can't get my head 
-- around it.
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
    newState = case (Debug.log "current " current)  of  
      '"'  -> handleQuote '\"' state        
      '\r'  -> handleNewLine current state
      '\n'  -> handleNewLine current state 
      _     -> handleCharacter current state 
  in 
    {newState | previousChar = Just current}


handleQuote : Char -> State -> State 
handleQuote char state = 
  case (state.inQuote) of 
    (True) -> 
      case state.previousChar of 
        Nothing -> Debug.log "handleQuote.Nothing" {state | inQuote = False} -- No idea how we could get here.  
        Just ch ->
          case ch of 
            '"' ->
              let 
                nV =  Maybe.map  (\ v -> v ++ "\"")  state.cValue
              in   
                {state | cValue = nV,  inQuote = False}
            '\\' ->
              let 
                nV =  Maybe.map  (\ v -> v ++ "\"")  state.cValue
              in   
                {state | cValue = nV,  inQuote = False}
            _ ->
              {state | inQuote = False}


    (False) -> 
      case state.previousChar of 
        Nothing -> {state | inQuote = True}  
        Just prev -> 
          case (prev,char) of 
            ('"','"')   ->
             Debug.log ("quote quote " ++ (Debug.toString state.cValue) ) (escapedQuote state)
            ('\\'  ,'"') -> 
              Debug.log ("slash quote " ++ (Debug.toString state.cValue) ) (escapedQuote state)
            _           ->
              let 
                sc = String.fromChar char
                nV =  case state.cValue of 
                  Nothing  -> Just sc
                  Just cV  -> Just (cV ++ sc)
                xx = Debug.log ""
              in    
                {state | inQuote = True}

   

escapedQuote : State -> State 
escapedQuote state = 
  case state.cValue of 
    Nothing -> Debug.log "We got a place we shouldn't have " state  -- how did we get here 
    Just cv ->
      let
        x      = Debug.log "this is cv" cv
        trimmed =  Debug.log "this is trimmer" (String.slice 0 -1 cv)
        ncVale  = trimmed ++ "\""
      in 
        {state | cValue = (Just ncVale)}


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
        (Just c , Nothing ) -> [c] :: s.content
        -- we have a row but no conent 
        (Nothing, Just r  ) -> List.reverse(r) :: s.content
        -- both content and a row 
        (Just c,  Just r  ) ->  
          let 
            nRow = List.reverse(c :: r ) 
          in 
            nRow :: s.content
    in
    {s | content = nContent, cValue = Nothing, cRow = Nothing}  


handleSeparator : State -> State 
handleSeparator s = 
  let 
    fRow : Maybe String -> Maybe (List String) -> Maybe (List String)
    fRow mv mr =
      case (mv,mr) of 
        (Nothing, Nothing ) -> Nothing
        (Just v , Nothing ) -> Just [v] 
        (Nothing, Just r  ) -> Just  r
        (Just v,  Just r  ) ->  Just (v :: r)
    nRow : Maybe (List String)
    nRow = fRow s.cValue s.cRow  
  in
    {s | cValue = Nothing, cRow = nRow}     


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
    {s | cValue = update}      


