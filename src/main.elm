import Browser
import Html
import Html.Attributes as HTMLATTR
import Html.Events as HEv
import Http
import Json.Decode as JSD
import Time
-- import Json


main = Browser.element {init = init, update = update, view = view, subscriptions = subscr}

-- MODEL

type alias Flags = { endpoint : String }

type alias Location =
  { name : String
  , pressure : Float
  , imgurl : String
  }

type LocData = Loading | Failed String | Success (List Location)

jsonDataRequest endpoint = Http.get
      { url = endpoint
      , expect = Http.expectJson ReceivedLocations locationListDecoder
      }

type alias Model =
  { sort : List Location -> List Location
  , locations : LocData
  , endpoint : String
  }

init : Flags -> (Model, Cmd Msg)
init { endpoint } =
  ( { sort = List.sortBy .name, locations = Loading, endpoint = endpoint }
  , jsonDataRequest endpoint
  )

-- UPDATE
type Msg = ReceivedLocations (Result Http.Error (List Location))
         | Tick Time.Posix
         | SortByChanged String
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedLocations (Ok locations) ->
          ({model | locations = Success locations }, Cmd.none)
    ReceivedLocations (Err err) ->
          ({model | locations = Failed ("An error occurred while loading the json: " ++ Debug.toString err) }, Cmd.none)
    Tick time ->
      (model, jsonDataRequest model.endpoint)
    SortByChanged value ->
      ({model | sort = sortFromValue value}, Cmd.none)


-- SUBSCRIPTIONS
subscr: Model -> Sub Msg
subscr mdl =
  Time.every 5000 Tick


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

sortFromValue : String -> List Location -> List Location
sortFromValue str = case str of
    "pressure" -> List.sortBy .pressure
    _ ->  List.sortBy .name

locationHeaders : Html.Html Msg
locationHeaders =
  let divClass (class, child) = Html.div [HTMLATTR.class class] [Html.text child]
  in
    Html.div [HTMLATTR.class "loc-header-row"] (
      [ Html.select [HEv.onInput SortByChanged ]
        [ Html.option [HTMLATTR.value "alphabetically"] [Html.text "alphabetically"]
        , Html.option [HTMLATTR.value "pressure"] [Html.text "pressure"]
        ]
      ] ++
      List.map divClass [
        ("loc-header-cell-img", "Image"),
        ("loc-header-cell-name", "Name"),
        ("loc-header-cell-pressure", "Pressure")
      ]
    )

locationToString : Location -> String
locationToString loc =
  loc.name ++ " " ++ String.fromFloat loc.pressure

view : Model -> Html.Html Msg
view model =
  case model.locations of
    Loading ->
      Html.div [] [Html.text "Loading"]
    Failed msg ->
      Html.div [] [Html.text msg]
    Success data ->
      Html.div [HTMLATTR.class "locations"]
       (List.append [locationHeaders] (List.map locationToHtml (model.sort data)))

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
