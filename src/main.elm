import Browser
import Html
import Html.Attributes as HTMLATTR
import Html.Events as HEv
import Http
import Json.Decode as JSD
import Time
import Dict
import Json.Decode.Pipeline as JSP
-- import Json


main = Browser.element {init = init, update = update, view = view, subscriptions = subscr}

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

init : Flags -> (Model, Cmd Msg)
init { endpoint } =
  ( { sort = ByPressure False, locations = Loading, endpoint = endpoint, position = Nothing }
  , jsonDataRequest endpoint
  )

-- UPDATE
type Msg = ReceivedLocations (Result Http.Error (List Location))
         | Tick Time.Posix
         | OrderBy Order
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


-- SUBSCRIPTIONS
subscr: Model -> Sub Msg
subscr mdl =
  Time.every 5000 Tick


-- VIEW

locationToHtml : Location -> Html.Html Msg
locationToHtml loc =
  let class name = HTMLATTR.class name
      attribute name = HTMLATTR.attribute name
  in

    Html.div [class "loc-row"] [
      -- the image
      Html.div [class "loc-img"]
               [Html.img [attribute "src" loc.imgurl] []],
      -- the rest
      Html.div []
        [
          -- upper
          Html.div [class "loc-upper"] [
                     -- Name
                     Html.div [class "loc-name"] [Html.text loc.name],
                     -- Progress bar
                     Html.div [class "loc-progress"] []
                   ],
          -- lower
          Html.div [class "loc-lower"] [
                     -- info icon
                     case loc.infourl of
                       Just url ->
                         Html.a [attribute "href" url] [
                           Html.img [attribute "src" "img/info.svg", class "loc-info"] []
                         ]
                       Nothing ->
                         Html.div[attribute "src" "img/info.svg", class "loc-info"] []
                        , 
                     -- Text
                     Html.div [class "loc-description"] [Html.text (Maybe.withDefault "" loc.description)],
                     -- Right side
                     Html.div [class "loc-right"] [
                        Html.div [class "loc-location"
                        ] [
                          Html.img [attribute "src" "img/map-marker.svg"] [],
                          Html.text "542m"
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
      ] ++
      List.map divClass [
        ("loc-header-cell-img", "Image"),
        ("loc-header-cell-name", "Name"),
        ("loc-header-cell-pressure", "Pressure")
      ]
    )

select : Order -> (Order -> msg) -> List (Order, String) -> Html.Html msg
select def msg opts =
  let invert (a, b) = (b, a)
      flip f a b = f b a
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
       (List.append [locationHeaders model] (List.map locationToHtml (sortBy model.sort data)))

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

-- Given the clients location as Point an Location returns the distance in m
distance : Point -> Location -> Float
distance _ _ = 542
