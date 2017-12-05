port module Landing exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Date exposing (Date, fromString, toTime, now)
import Date.Extra as Date
import Task

-- TODO: Need a separate type for storing this thing as Javascript?
-- init's first argument must be JSONable, and
-- the type on the setStorage port must also be JSOnable

{- Program
-}
main : Program (Maybe SerializableModel) Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }

-- Create a port for sending our model to javascript for storage
port setStorage : SerializableModel -> Cmd msg

-- Anytime we update the model, set storage as well
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        (newModel, cmds) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage <| toSerializable newModel, cmds ]
        )

-------------------------------------------------------------------------------
-- MODEL
-------------------------------------------------------------------------------

type alias Model =
    { campaigns : List Campaign
    , createDialog : DialogModel
    }

type alias Campaign =
    { name : String
    , players : Int
    , startDate : Maybe Date
    , lastPlayed : Maybe Date
    , pinned : Bool
    , relationship : CampaignRelation
    }

type alias DialogModel =
    { open : Bool
    , name : String
    , startDate : Maybe Date
    , nameError : Maybe String
    , dateError : Maybe String
    }

newCampaign : String -> Date -> Campaign
newCampaign name start =
    { name = name
    , players = 0
    , startDate = Just start
    , lastPlayed = Nothing
    , pinned = False
    , relationship = DM
    }

type CampaignRelation
    = DM
    | Player Character

type alias Character =
    { name : String
    , level : Int
    }

type alias SerializableModel =
    { campaigns : List SerializableCampaign
    , creating : Bool
    }

type alias SerializableCampaign =
    { name : String
    , players : Int
    , startDate : Float
    , lastPlayed : Float
    , pinned : Bool
    , dm : Bool
    , character : Maybe Character
    }

blankDialogModel : Bool -> DialogModel
blankDialogModel open =
    { open = open
    , name = ""
    , startDate = Nothing
    , nameError = Nothing
    , dateError = Nothing
    }

exampleModel : Model
exampleModel =
    let
        exampleCampaigns =
            [   { name = "Trekking Blackmoor"
                , players = 6
                , startDate = (Result.toMaybe <| Date.fromString <| "2015-06-11")
                , lastPlayed = (Result.toMaybe <| Date.fromString <| "2017-12-02")
                , pinned = True
                , relationship = DM
                }
            ,   { name = "Example Campaign"
                , players = 3
                , startDate = (Result.toMaybe <| Date.fromString <| "2017-12-04")
                , lastPlayed = Nothing
                , pinned = True
                , relationship = DM
                }
            ,   { name = "Shadow of Mordor"
                , players = 2
                , startDate = (Result.toMaybe <| Date.fromString <| "2017-12-03")
                , lastPlayed = Nothing
                , pinned = False
                , relationship = Player <| Character "Orcy the Orc" 6
                }
            ,   { name = "Rogue One"
                , players = 2
                , startDate = (Result.toMaybe <| Date.fromString <| "2017-12-01")
                , lastPlayed = Nothing
                , pinned = False
                , relationship = Player <| Character "K2S0" 4
                }
            ]
    in
        { campaigns = exampleCampaigns
        , createDialog = blankDialogModel False
        }

init : Maybe SerializableModel -> ( Model, Cmd Msg )
init savedModel =
    (savedModel |> Maybe.map fromSerializable |> Maybe.withDefault exampleModel) ! [ Task.perform identity (Task.succeed GetCurrentDate) ]

toSerializable : Model -> SerializableModel
toSerializable model =
    { campaigns = List.map campaignToSerializable model.campaigns
    , creating = model.createDialog.open
    }

campaignToSerializable : Campaign -> SerializableCampaign
campaignToSerializable model =
    let
        isDm =
            case model.relationship of
                DM -> True
                _ -> False

        character =
            case model.relationship of
                DM -> Nothing
                Player c -> Just c
    in
        { name = model.name
        , players = model.players
        , pinned = model.pinned
        , dm = isDm
        , character = character
        , startDate = Maybe.withDefault 0.0 <| Maybe.map toTime model.startDate
        , lastPlayed = Maybe.withDefault 0.0 <| Maybe.map toTime model.lastPlayed
        }

fromSerializable : SerializableModel -> Model
fromSerializable smodel =
    { campaigns = List.map campaignFromSerializable smodel.campaigns
    , createDialog = blankDialogModel smodel.creating
    }

campaignFromSerializable : SerializableCampaign -> Campaign
campaignFromSerializable scamp =
    let
        rel =
            if scamp.dm then
                DM
            else
                Player <| Maybe.withDefault (Character "" 0) scamp.character

    in
        { name = scamp.name
        , players = scamp.players
        , startDate = Just <| Date.fromTime scamp.startDate
        , lastPlayed = Just <| Date.fromTime scamp.lastPlayed
        , pinned = scamp.pinned
        , relationship = rel
        }

-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------

type Msg
    = NoOp
    | GetCurrentDate
    | UpdateCreateDate Date
    | Add Campaign
    | SetPinned String Bool
    | Creating Bool

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        GetCurrentDate ->
            model ! [ Task.perform UpdateCreateDate now ]

        UpdateCreateDate newDate ->
            let
                oldDialog = model.createDialog
                newDialog = { oldDialog | startDate = Just newDate }
            in
                { model | createDialog = newDialog
                } ! []

        Add newCampaign ->
            { model |
                campaigns = List.append model.campaigns [ newCampaign ]
            } ! []

        SetPinned name pin ->
            let
                updateCampaign c =
                    if c.name == name then
                        { c | pinned = pin }
                    else
                        c

            in
                { model |
                    campaigns = List.map updateCampaign model.campaigns
                } ! []

        Creating flag ->
            let
                oldDialog = model.createDialog
                newDialog = { oldDialog | open = flag }
            in
                { model | createDialog = newDialog } ! []

-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ class "list-container"
            , id "pinned-container"
            ]
            [ h2 [] [ text "Pinned Campaigns" ]
            , div [] (model.campaigns |> List.filter (\c -> c.pinned) |> List.map pinnedListItem)
            ]
        , div
            [ class "list-container"
            , id "all-container"
            ]
            [ h2 [] [ text "All Campaigns" ]
            , div [] ((List.map campaignListItem model.campaigns) ++
                [ button
                    [ id "create-campaign-button"
                    , onClick <| Creating True
                    ]
                    [ text "+ Start a campaign" ]
                ])
            ]
        , creationDialog model.createDialog
        , div [ class "md-overlay" ] []
        ]

campaignListItem : Campaign -> Html Msg
campaignListItem campaign =
    let
        nameArea =
            case campaign.relationship of
                DM ->
                    [ span [] [ text "Dungeon Master" ] ]

                Player { name, level } ->
                    [ span [] [ text <| name ++ " "]
                    , span [] [ text <| toString level ]
                    ]

        info =
            [ h4 [] [ text (campaign.name) ]
            , div [] nameArea
            ]
        pinner =
            div
                [ class "camp-pin" ]
                [ button [ onClick <| SetPinned campaign.name True ] [ text "*" ] ]

        children =
            if not campaign.pinned then
                pinner :: info
            else
                info

    in
        div [ class "list-item campaign" ] children

dateStringHelper : Maybe Date -> String
dateStringHelper date =
    date |> Maybe.map (Date.toFormattedString "MMMM d, y") |> Maybe.withDefault "never"

pinnedListItem : Campaign -> Html Msg
pinnedListItem campaign =
    div
        [ class "list-item pinned-campaign" ]
        [ div
            [ class "pinned-unpin" ]
            [ button [ onClick <| SetPinned campaign.name False ] [ text "-" ] ]
        , h3 [] [ text campaign.name ]
        , div
            [ class "players" ]
            [ span [] [ text <| (toString campaign.players) ++ " players" ] ]
        , div
            [ class "last-played"]
            [ span [] [ text <| "Last played: " ++ (dateStringHelper campaign.lastPlayed) ] ]
        , div
            [ class "playing-for" ]
            [ span [] [ text <| "Playing since: " ++ (dateStringHelper campaign.startDate) ] ]
        , div
            [ class "pinned-buttons" ]
            [ button
                []
                [ text "Dashboard" ]
            , button
                []
                [ text "Session Log" ]
            , button
                []
                [ text "Settings" ]
            ]
        ]

creationDialog : DialogModel -> Html Msg
creationDialog model =
    div
        [ class (if model.open then "md-modal md-show" else "md-modal")
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
                        , model.startDate |> Maybe.map (Date.toFormattedString "yyyy-MM-dd") |> Maybe.withDefault "2018-01-01" |> value
                        ]
                        []
                    ]
                , button
                    [ id "create-submit" ]
                    [ text "Start!" ]
                , button
                    [ id "create-cancel"
                    , onClick <| Creating False
                    ]
                    [ text "or, cancel" ]
                ]
            ]
        ]
