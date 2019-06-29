import Browser
import Html
import Html.Attributes as HTMLATTR
import Http
import Json.Decode as JSD
-- import Json


main = Browser.element {init = init, update = update, view = view, subscriptions = subscr}

-- MODEL
type alias Location = {name : String, pressure : Float, imgurl : String}

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

locationToHtml : Location -> Html.Html Msg
locationToHtml loc =
  Html.div [HTMLATTR.class "loc-row"] [
    Html.div [HTMLATTR.class "loc-cell-img"] [Html.img [HTMLATTR.attribute "src" loc.imgurl] []],
    Html.div [HTMLATTR.class "loc-cell-name"] [Html.text loc.name],
    Html.div [HTMLATTR.class "loc-cell-pressure"] [Html.text (String.fromFloat loc.pressure)]
  ]

locationHeaders : Html.Html Msg
locationHeaders =
  let divClass (class, child) = Html.div [HTMLATTR.class class] [Html.text child]
  in
    Html.div [HTMLATTR.class "loc-header-row"] (
      List.map divClass [
        ("loc-header-cell-img", "Image"),
        ("loc-header-cell-name", "Name"),
        ("loc-header-cell-pressure", "Pressure")
        ]
    )

view : Model -> Html.Html Msg
view model =
  case model of
    Loading ->
      Html.div [] [Html.text "Loading"]
    Failed msg ->
      Html.div [] [Html.text msg]
    Success locations ->
      Html.div [HTMLATTR.class "locations"] (List.append [locationHeaders] (List.map locationToHtml locations))

-- DECODERS

nameDecoder : JSD.Decoder String
nameDecoder = JSD.field "name" JSD.string

pressureDecoder : JSD.Decoder Float
pressureDecoder = JSD.field "pressure" JSD.float

imgurlDecoder : JSD.Decoder String
imgurlDecoder = JSD.field "image" JSD.string

locationDecoder : JSD.Decoder Location
locationDecoder = JSD.map3 Location nameDecoder pressureDecoder imgurlDecoder

locationListDecoder : JSD.Decoder (List Location)
locationListDecoder = JSD.field "locations" (JSD.list locationDecoder)


