module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D exposing (Decoder)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias BodyInfo =
    { date : String
    , weight : String
    }


type Model
    = Loading
    | Success (List BodyInfo)
    | Failed Http.Error


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "example.com", expect = Http.expectJson Received bodyInfoListDecoder }
    )


type Msg
    = Received (Result Http.Error (List BodyInfo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Received (Ok bodyInfo) ->
            ( Success bodyInfo
            , Cmd.none
            )

        Received (Err e) ->
            ( Failed e
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            div [] [ text "Loading..." ]

        Failed e ->
            div [] [ text <| Debug.toString e ]

        Success bodyInfos ->
            div [] <| List.map (text << show) bodyInfos


show : BodyInfo -> String
show model =
    model.date ++ " : " ++ model.weight ++ "kg"


bodyInfoDecoder : Decoder BodyInfo
bodyInfoDecoder =
    D.map2 BodyInfo (D.field "date" D.string) (D.field "weight" D.string)


bodyInfoListDecoder : Decoder (List BodyInfo)
bodyInfoListDecoder =
    D.field "body_weight" (D.list bodyInfoDecoder)
