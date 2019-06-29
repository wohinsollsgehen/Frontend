import Browser
import Html
import Html.Attributes as HTMLATTR
import Http
import Json.Decode as JSD
import Time
-- import Json


main = Browser.element {init = init, update = update, view = view, subscriptions = subscr}

-- MODEL
type Order = Alphabetic | Pressure
type alias Location = {name : String, pressure : Float, imgurl : String}

locationToString : Location -> String
locationToString loc = 
  loc.name ++ " " ++ String.fromFloat loc.pressure

type alias LocData = {locations : (List Location), order : Order}

jsonDataRequest = Http.get
      { url = "http://localhost:8000/sample.json"
      , expect = Http.expectJson ReceivedLocations locationListDecoder  
      }

type Model = Loading | Failed String | Success LocData 
init : () -> (Model, Cmd Msg)
init _ = (Loading, jsonDataRequest)


-- UPDATE
type Msg = ReceivedLocations (Result Http.Error (List Location)) | Tick Time.Posix 
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedLocations (Ok locations) ->
          (Success {locations = locations, order = Pressure}, Cmd.none)
    ReceivedLocations (Err _) ->
          (Failed "An error occurred while loading the json.", Cmd.none)
    Tick time ->
      (model, jsonDataRequest)


-- SUBSCRIPTIONS
subscr: Model -> Sub Msg
subscr mdl = 
  Time.every 15 Tick


-- VIEW
locationToHtml : Location -> Html.Html Msg
locationToHtml loc =
  Html.div [HTMLATTR.class "loc-row"] [
    Html.div [HTMLATTR.class "loc-cell-img"]
             [Html.img [HTMLATTR.attribute "src" loc.imgurl] []],
    Html.div [HTMLATTR.class "loc-cell-name"] [Html.text loc.name],
    Html.div [HTMLATTR.class "loc-cell-pressure"]
             [Html.text (String.fromFloat loc.pressure)]
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

sortLocations : LocData -> List Location
sortLocations locdata =
  case locdata.order of
    Alphabetic ->
      List.sortBy .name locdata.locations  
    Pressure ->
      List.sortBy .pressure locdata.locations  

   

view : Model -> Html.Html Msg
view model =
  case model of
    Loading ->
      Html.div [] [Html.text "Loading"]
    Failed msg ->
      Html.div [] [Html.text msg]
    Success data ->
      Html.div [HTMLATTR.class "locations"]
       (List.append [locationHeaders] (List.map locationToHtml (sortLocations data)))

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
