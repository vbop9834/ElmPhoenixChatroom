module App exposing (..)

import Phoenix.Socket
import Phoenix.Channel
import Phoenix.Push

import Html exposing (Html, div, li, ul, text, form, input, button)
import Html.Events exposing (onInput, onSubmit)

import Json.Encode as JsEncode
import Json.Decode as JsDecode

type alias ChatMessagePayload =
  {
   message : String
  }

type alias Model =
  {
   phxSocket : Phoenix.Socket.Socket Msg,
   messageInProgress : String,
   messages : List String
  }

type Msg =
    PhoenixMsg (Phoenix.Socket.Msg Msg)
  | SetMessage String
  | SendMessage
  | ReceiveChatMessage JsEncode.Value
  | HandleSendError JsEncode.Value

init : ( Model, Cmd Msg )
init =
  let
   channel =
    Phoenix.Channel.init "room:lobby"
   (initSocket, phxCmd) =
     Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
     |> Phoenix.Socket.withDebug
     |> Phoenix.Socket.on "shout" "room:lobby" ReceiveChatMessage
     |> Phoenix.Socket.join channel
   model =
     {
       phxSocket = initSocket,
       messageInProgress = "",
       messages = [ ]
     }
  in
   ( model, Cmd.map PhoenixMsg phxCmd )


drawMessage : String -> Html Msg
drawMessage message =
  li [] [
   text message
  ]


view : Model -> Html Msg
view model =
  let
   drawMessages messages =
    messages |> List.map drawMessage
  in
    div [] [
      ul [] (model.messages |> drawMessages),
      form [ onSubmit SendMessage] [
       input [ onInput SetMessage ] [
       ],
       button [] [
         text "Submit"
       ]
      ]
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    PhoenixMsg msg ->
      let
       ( phxSocket, phxCmd ) = Phoenix.Socket.update msg model.phxSocket
      in
       ( { model | phxSocket = phxSocket }
         , Cmd.map PhoenixMsg phxCmd
       )
    SetMessage message ->
      ( { model | messageInProgress = message }, Cmd.none )
    SendMessage ->
      let
        payload =
          JsEncode.object
           [
            ("message", JsEncode.string model.messageInProgress)
           ]
        phxPush =
          Phoenix.Push.init "shout" "room:lobby"
            |> Phoenix.Push.withPayload payload
            |> Phoenix.Push.onOk ReceiveChatMessage
            |> Phoenix.Push.onError HandleSendError
        (phxSocket, phxCmd) = Phoenix.Socket.push phxPush model.phxSocket
      in
       (
        {
          model |
           messageInProgress = "",
           phxSocket = phxSocket
        },
        Cmd.map PhoenixMsg phxCmd
      )
    ReceiveChatMessage raw ->
      let
       messageDecoder = JsDecode.field "message" JsDecode.string
       somePayload = JsDecode.decodeValue messageDecoder raw
      in
       case somePayload of
         Ok payload ->
           (
            { model | messages = payload :: model.messages },
            Cmd.none
           )
         Err error ->
           ( { model | messages = "Failed to receive message" :: model.messages }, Cmd.none )
    HandleSendError err ->
      let
       message = "Failed to Send Message"
      in
       ({ model | messages = message :: model.messages }, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
  Phoenix.Socket.listen model.phxSocket PhoenixMsg

main : Program Never
main =
  Html.program
    {
      init = init,
      view = view,
      update = update,
      subscriptions = subscriptions
    }
