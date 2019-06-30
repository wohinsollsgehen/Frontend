import Browser
import Browser.Navigation
import Html
import Html.Attributes as HTMLATTR
import Html.Events as HEv
import Http
import Json.Decode as JSD
import Time
import Dict
import Json.Decode.Pipeline as JSP
import Url
import Url.Parser.Query
import Url.Parser
-- import Json


-- main = Browser.application
--   { init = init
--   , update = update
--   , view = toDoc << view
--   , subscriptions = subscr
--   , onUrlRequest = \_ -> Noop
--   , onUrlChange = \_ -> Noop
--   }

main = Browser.element
  { init = init
  , update = update
  , view = view
  , subscriptions = subscr
  }

-- MODEL

type alias Flags = { endpoint : String }

type alias Point = { lat : Float, lng : Float }

type alias Location =
  { name : String
  , pressure : Float
  , imgurl : String
  , location : Point
  , capacity : Int
  , time : Time.Posix
  , visitors : Int
  , infourl : Maybe String
  , description : Maybe String
  }

type LocData = Loading | Failed String | Success (List Location)

type Order = ByName Bool | ByPressure Bool | ByDistance Point

sortBy : Order -> List Location -> List Location
sortBy order = case order of
  ByName False -> List.sortBy .name
  ByName True -> List.reverse << List.sortBy .name
  ByPressure False -> List.sortBy .pressure
  ByPressure True -> List.reverse << List.sortBy .pressure
  ByDistance p -> List.sortBy (distance p)

jsonDataRequest endpoint = Http.get
      { url = endpoint
      , expect = Http.expectJson ReceivedLocations locationListDecoder
      }

type alias Model =
  { sort : Order
  , locations : LocData
  , endpoint : String
  , position : Maybe Point
  }

init : Flags ->  (Model, Cmd Msg) -- Url.Url -> Browser.Navigation.Key ->
init { endpoint } =
  let
    float n = Url.Parser.Query.custom n (Maybe.andThen (\x -> x) << List.head << List.map String.toFloat)
    point = Url.Parser.Query.map2 (Maybe.map2 Point) (float "lat") (float "lng")
    parseLoc = Url.Parser.parse (Url.Parser.query point)
  in
    ( { sort = ByPressure False, locations = Loading, endpoint = endpoint, position = Just { lat = 47.997015, lng = 7.8441079 } }
    , jsonDataRequest endpoint
    )

-- UPDATE
type Msg = ReceivedLocations (Result Http.Error (List Location))
         | Tick Time.Posix
         | OrderBy Order
         | Noop
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedLocations (Ok locations) ->
          ({model | locations = Success locations }, Cmd.none)
    ReceivedLocations (Err err) ->
          ({model | locations = Failed ("An error occurred while loading the json: " ++ Debug.toString err) }, Cmd.none)
    Tick time ->
      (model, jsonDataRequest model.endpoint)
    OrderBy order ->
      ({model | sort = order}, Cmd.none)
    Noop -> (model, Cmd.none)


-- SUBSCRIPTIONS
subscr: Model -> Sub Msg
subscr mdl =
  Time.every 5000 Tick


-- VIEW

pressureToColor : Float -> String
pressureToColor pressure = 
  if pressure < 0.4 then
    "#3c9a2b"
  else if pressure < 0.7 then
    "#FFFF00"
  else
    "#d10913"

pressureToWidth : Float -> String
pressureToWidth pressure =
  String.fromFloat(pressure * 100) ++ "%"
toDoc : Html.Html msg -> Browser.Document msg
toDoc x = { title = "Wohin solls gehen"
          , body = [x]
          }

flip f a b = f b a

locationToHtml : Model -> Location -> Html.Html Msg
locationToHtml model loc =
  let class name = HTMLATTR.class name
      attribute name = HTMLATTR.attribute name
      style name = HTMLATTR.style name
      distString = Maybe.map ((\x -> x ++ " m") << String.fromInt << truncate << flip distance loc) model.position
  in

    Html.div [class "loc-row"] [
      -- the image
      Html.div [class "loc-img"]
               [Html.img [attribute "src" loc.imgurl] []],
      -- the rest
      Html.div [class "loc-right"]
        [
          -- upper
          Html.div [class "loc-upper"] [
                     -- Name
                     Html.div [class "loc-name"] [Html.text loc.name],
                     -- Progress bar
                     Html.div [class "loc-progress-container"] [
                         Html.div [class "loc-progress", style "width" (pressureToWidth loc.pressure), style "background-color" (pressureToColor loc.pressure)] []
                       ]
                   ],
          -- lower
          Html.div [class "loc-lower"] [
                     -- info icon
                     case loc.infourl of
                       Just url ->
                         Html.a [attribute "href" url] [
                           Html.img [attribute "src" "img/info-circle.svg", class "loc-info"] [] 
                         ]
                       Nothing ->
                         Html.div[class "loc-info"] []
                        , 
                     -- Text
                     Html.div [class "loc-description"] [Html.text (Maybe.withDefault "" loc.description)],
                     -- Right side
                     Html.div [class "loc-lower-right"] [
                        Html.div [class "loc-location"
                        ] [
                          Html.img [attribute "src" "img/map-marker.svg", class "loc-map-icon"] [],
                          Html.text (Maybe.withDefault "nicht verfügbar" (distString))
                        ],
                        Html.button [class "loc-route"] [
                          Html.text "route"
                        ]
                     ]
                   ]
        ]
    ]

locationHeaders : Model -> Html.Html Msg
locationHeaders model =
  let divClass (class, child) = Html.div [HTMLATTR.class class] [Html.text child]
      sortSelect = select (ByPressure False) OrderBy
      sortOptions = [ (ByName False, "A-Z")
        , (ByName True, "Z-A")
        , (ByPressure False, "Leer - Voll")
        , (ByName True, "Voll - Leer")
        ]
      sortByPositionOption
        = Maybe.withDefault
            []
            (Maybe.map ((\x -> [(x, "Entfernung")]) << ByDistance) model.position)
  in
    Html.div [HTMLATTR.class "loc-header-row"] (
      [ sortSelect (sortOptions ++ sortByPositionOption)
      ]
    )

select : Order -> (Order -> msg) -> List (Order, String) -> Html.Html msg
select def msg opts =
  let invert (a, b) = (b, a)
      lookup l x = List.head <| List.map Tuple.second <| List.filter (\(k, _) -> k == x) <| l
      toString = Maybe.withDefault "This won't happen" << lookup opts
      fromString = let lu = Dict.fromList <| List.map invert opts
                   in Maybe.withDefault def << flip Dict.get lu
      option (_, name)= Html.option [HTMLATTR.value name] [Html.text name]
  in Html.select [HEv.onInput (msg << fromString) ] (List.map option opts)


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
       (List.append [locationHeaders model] (List.map (locationToHtml model) (sortBy model.sort data)))

-- DECODERS

pointDecoder : JSD.Decoder Point
pointDecoder =
  let lat = JSD.field "latitude" JSD.float
      lng = JSD.field "longitude" JSD.float
  in JSD.map2 Point lat lng

posixDecoder : JSD.Decoder Time.Posix
posixDecoder = JSD.map ((*) 1000 >> truncate >> Time.millisToPosix) JSD.float

locationDecoder : JSD.Decoder Location
locationDecoder = JSD.succeed Location
                   |> JSP.required "name" JSD.string
                   |> JSP.required "pressure" JSD.float
                   |> JSP.required "image" JSD.string
                   |> JSP.custom pointDecoder
                   |> JSP.required "capacity" JSD.int
                   |> JSP.required "lastTimestamp" posixDecoder
                   |> JSP.required "visitors" JSD.int
                   |> JSP.optional "infourl" (JSD.nullable JSD.string) Nothing
                   |> JSP.optional "description" (JSD.nullable JSD.string) Nothing

locationListDecoder : JSD.Decoder (List Location)
locationListDecoder = JSD.field "locations" (JSD.list locationDecoder)

-- HELPERS

earthA : Float
earthA = 6372.8

-- Adapted from https://gist.github.com/Ahrengot/e91881252364ee81b38d2f3487cdfd73
distLatLon : Float -> Float -> Float -> Float -> Float
distLatLon lon1 lat1 lon2 lat2 =
    let
        earthRadiusInM =
            6372800
        dLat =
            degrees (degrees lat2 - degrees lat1)

        dLon =
            degrees (degrees lon2 - degrees lon1)

        haversine =
            (sin <| dLat / 2)
                * (sin <| dLat / 2)
                + (cos <| degrees lat1)
                * (cos <| degrees lat2)
                * (sin <| dLon / 2)
                * (sin <| dLon / 2)

        c =
            2 * (atan2 (sqrt haversine) (sqrt 1 - haversine))
    in
        earthRadiusInM * c

-- Given the clients location as Point an Location returns the distance in m
distance : Point -> Location -> Float
distance from { location } = distLatLon from.lat from.lng location.lat location.lng
