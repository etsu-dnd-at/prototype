module NewCampaign exposing (Model, Msg(..), init, update, view)

-- IMPORTS
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Date exposing (Date)
import Date.Extra

-------------------------------------------------------------------------------
-- MODEL
-------------------------------------------------------------------------------

type NameError
    = NameMissing
    | NotUnique

type DateError
    = DateMissing

type FieldError
    = NameError
    | DateError

type alias Model =
    { show : Bool
    , name : String
    , date : Maybe Date
    , errors : List FieldError
    }

emptyModel : Model
emptyModel =
    { show = False
    , name = ""
    , date = Nothing
    , errors = []
    }

init : ( Model, Cmd Msg )
init = (emptyModel, Task.perform identity (Task.succeed GetCurrentDate))

-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------

type Msg
    = GetCurrentDate
    | SetDate Date
    | SetName String
    | AddError FieldError
    | ClearErrors
    | Show Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetCurrentDate ->
            model ! [ Task.perform SetDate Date.now ]

        SetDate newDate ->
            { model | date = Just newDate } ! []

        SetName newName ->
            { model | name = newName } ! []

        AddError someError ->
            { model | errors = someError :: model.errors } ! []

        ClearErrors ->
            { model | errors = [] } ! []

        Show show ->
            { model | show = show } ! []


-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
    div
        [ classList [ ("md-modal", True)
                    , ("md-show", model.show)
                    ]
        , id "creation-modal"
        ]
        [ div
            [ class "md-content" ]
            [ h3 [] [ text "Start a Campaign" ]
            , div
                []
                [ div
                    []
                    [ h4 [] [ text "Name your campaign" ]
                    , input
                        [ type_ "text"
                        , placeholder "e.g. 'You shall not pass'"
                        , value model.name
                        ] []
                    ]
                , div
                    []
                    [ h4 [] [ text "Choose a start date" ]
                    , input
                        [ type_ "date"
                        , model.date |> Maybe.map (Date.Extra.toFormattedString "yyyy-MM-dd") |> Maybe.withDefault "2018-01-01" |> value
                        ]
                        []
                    ]
                , button
                    [ id "create-submit" ]
                    [ text "Start!" ]
                , button
                    [ id "create-cancel"
                    , onClick <| Show False
                    ]
                    [ text "or, cancel" ]
                ]
            ]
        ]
