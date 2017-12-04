port module Landing exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

{- Program
-}
main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithSession
        , subscriptions = \_ -> Sub.none
        }

-- Create a port for sending our model to javascript for storage
port setStorage : Model -> Cmd msg

-- Anytime we update the model, set storage as well
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        (newModel, cmds) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )

{-
    MODEL
-}

type alias Model =
    { campaigns : List Campaign
    }

type alias Campaign =
    { name : String
    , players : Int
    , created : Date
    , lastPlayed : Date
    , pinned : Bool
    , relationship : CampaignRelation
    }

type CampaignRelation
    = DM
    | Player Character

type alias Character =
    { name : String
    , level : Int
    }

exampleModel : Model
exampleModel =
    let
        exampleCampaigns = []
    in

    { campaigns = []
    }
