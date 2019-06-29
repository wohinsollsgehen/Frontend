import Browser
import Html
import Http
import Json.Decode as JSD
-- import Json


main = Browser.element {init = init, update = update, view = view, subscriptions = subscr}

-- MODEL
type alias Location = {name : String, pressure : Float}

locationToString : Location -> String
locationToString loc = 
  loc.name ++ " " ++ String.fromFloat loc.pressure

type Model = Loading | Failed String | Success (List Location)
init : () -> (Model, Cmd Msg)
init _ = (Loading, Http.get
      { url = "http://localhost:8000/sample.json"
      , expect = Http.expectJson ReceivedLocations locationListDecoder  
      })


-- UPDATE
type Msg = ReceivedLocations (Result Http.Error (List Location))
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedLocations (Ok locations) ->
          (Success locations, Cmd.none)
    ReceivedLocations (Err _) ->
          (Failed "An error occurred while loading the json.", Cmd.none)


-- SUBSCRIPTIONS
subscr: Model -> Sub Msg
subscr mdl = Sub.none 


-- VIEW
view : Model -> Html.Html Msg
view model =
  case model of
    Loading ->
      Html.div [] [Html.text "Loading"]
    Failed msg ->
      Html.div [] [Html.text msg]
    Success locations ->
      Html.div [] [Html.text (String.concat (List.intersperse ", " (List.map locationToString locations)))]

-- DECODERS

nameDecoder : JSD.Decoder String
nameDecoder = JSD.field "name" JSD.string

pressureDecoder : JSD.Decoder Float
pressureDecoder = JSD.field "pressure" JSD.float

locationDecoder : JSD.Decoder Location
locationDecoder = JSD.map2 Location nameDecoder pressureDecoder

locationListDecoder : JSD.Decoder (List Location)
locationListDecoder = JSD.field "locations" (JSD.list locationDecoder)


