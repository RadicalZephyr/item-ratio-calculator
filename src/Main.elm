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

type alias Item =
  { name : String
  , build_time : Int
  , inputs : List Input
  }
  
emptyItem : Item
emptyItem = Item "" 0 []

type alias Model =
  { items : List Item
  , selected : Maybe Item
  , addingItem: Item,
  }


init : Model
init =
  Model [] Nothing emptyItem



-- UPDATE

type Msg
  = UpdateAdd AddItemMsg 


update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateAdd addItemMsg -> 
      { model | addingItem = updateAddItem addItemMsg model.addingItem }

type AddItemMsg
  = UpdateName String
  | UpdateBuildTime Int
  | AddInput Input

updateAddItem : AddItemMsg -> Item -> Item
updateAddItem msg model =
  case msg of
    UpdateName name -> { model | name = name }
    UpdateBuildTime time -> { model | build_time = time }
    AddInput inputItem -> { model | inputs = inputItem :: inputs }
  

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ 
    ]

addTypeForm : String -> String -> String -> Html msg
addTypeForm model = 
  div []
    [ viewInput "text" "Item Name" model.name (UpdateAdd UpdateName)
    ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []
