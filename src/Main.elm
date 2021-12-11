-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Input =
  { name : String
  , amount: Int
  }

emptyInput : Input
emptyInput = Input "" 0

type alias Item =
  { name : String
  , buildTime : Int
  , inputs : List Input
  }
  
emptyItem : Item
emptyItem = Item "" 0 []

type alias Model =
  { items : List Item
  , selected : Maybe Item
  , addingItem: Item
  }


init : Model
init =
  Model [] Nothing emptyItem



-- UPDATE

type Msg
  = UpdateAdd AddItemMsg
  | DisplayError String


update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateAdd addItemMsg -> 
      { model | addingItem = updateAddItem addItemMsg model.addingItem }
    DisplayError s -> model

type AddItemMsg
  = Name String
  | BuildTime Int
  | AddInput
  | InputName String

updateAddItem : AddItemMsg -> Item -> Item
updateAddItem msg model =
  case msg of
    Name name -> { model | name = name }
    BuildTime time -> { model | buildTime = time }
    AddInput -> { model | inputs = emptyInput :: model.inputs }
    InputName name -> { model | inputs = model.inputs }

parseBuildTime : String -> Msg
parseBuildTime s =
  case String.toInt s of
    Just i -> UpdateAdd (BuildTime i)
    Nothing -> DisplayError "Build time is not a number"

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ addTypeForm model.addingItem
    ]

addTypeForm : Item -> Html Msg
addTypeForm model = 
  div []
    [  viewInput "text" "Item Name" model.name (\ name -> UpdateAdd (Name name))
    , viewInput "number" "Build Time" (String.fromInt model.buildTime) parseBuildTime
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
