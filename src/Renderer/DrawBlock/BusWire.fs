(*
This module implements wires between symbol ports. Wires can be autorouted, or manually routed by dragging segments.
Moving symbols causes the corresponding wires to move.
Wires are read and written from Issie as lists of wire vertices, whatever teh internal representation is.
*)


module BusWire

open CommonTypes
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers

//Static Vars
let minSegLen = 5.

//------------------------------------------------------------------------//
//------------------------------BusWire Types-----------------------------//
//------------------------------------------------------------------------//

///
type Orientation =  Horizontal | Vertical
type Rotation = CW0 | CW90 | CW180 | CW270

///
type SnapPosition = High | Mid | Low

/// Absolute Segment
type ASeg = 
    {
        Id: SegmentId
        Index: int
        Start: XYPos
        End: XYPos
        Dir: Orientation
        HostId: ConnectionId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateListA: list<float * SegmentId>
        Draggable: bool
        ManualRoute: bool
    }

/// Rotation Invariant Segment
type RISeg =
    {
        Id: SegmentId
        Index: int
        Start: XYPos
        Dir: Orientation
        Length: float
        HostId: ConnectionId
        /// List of distances from start of segment jumps. Only used on Horizontal segments
        JumpDistanceListRI: list<float * SegmentId>
        Draggable: bool
        ManualRoute: bool
    }

let getRISegEnd (startPos: XYPos) (seg: RISeg) : XYPos =
    match seg.Dir with
    | Horizontal -> {startPos with X = seg.Start.X + seg.Length}
    | Vertical -> {startPos with Y = seg.Start.Y + seg.Length}

let jumpDistToJumpCoord (startPos: XYPos, (dist: float, segId: SegmentId)) : float * SegmentId =
    startPos.X + dist, segId

let jumpCoordToJumpDist (startPos: XYPos, (coord: float, segId: SegmentId)) : float * SegmentId =
    coord - startPos.X, segId

let riSegToASeg (seg: RISeg) : ASeg =
    {
        Id = seg.Id
        Index = seg.Index
        Start = seg.Start
        End = getRISegEnd seg.Start seg
        Dir = seg.Dir
        HostId = seg.HostId
        JumpCoordinateListA =
            seg.JumpDistanceListRI
            |> List.map (fun distList -> (seg.Start, distList))
            |> List.map jumpDistToJumpCoord
        Draggable = seg.Draggable
        ManualRoute = seg.ManualRoute
    }

let aSegToRISeg (seg: ASeg) : RISeg =
    {
        Id = seg.Id
        Index = seg.Index
        Start = seg.Start
        Dir = seg.Dir
        Length =
            match seg.Dir with
            | Horizontal -> seg.End.X - seg.Start.X
            | Vertical -> seg.End.Y - seg.Start.Y
        HostId = seg.HostId
        JumpDistanceListRI =
            seg.JumpCoordinateListA
            |> List.map (fun coordList -> (seg.Start, coordList))
            |> List.map jumpCoordToJumpDist
        Draggable = seg.Draggable
        ManualRoute = seg.ManualRoute
    }

///
type Wire =
    {
        Id: ConnectionId 
        InputPort: InputPortId
        OutputPort: OutputPortId
        Color: HighLightColor
        Width: int
        Rotation: Rotation // CW rotation angle
        YReflect: bool // Reflected in Y axis before rotation
        Segments: RISeg list
    }

    with static member stickLength = 16.0



///
type Model =
    {
        Symbol: Symbol.Model
        WX: Map<ConnectionId, Wire>
        FromVerticalToHorizontalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        FromHorizontalToVerticalSegmentIntersections: Map<SegmentId, list<ConnectionId*SegmentId>>
        CopiedWX: Map<ConnectionId, Wire> 
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: list<ConnectionId>
        Notifications: Option<string>
    }

//----------------------------Message Type-----------------------------------//

///
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (InputPortId * OutputPortId)
    | BusWidths
    | CopyWires of list<ConnectionId>
    | DeleteWires of list<ConnectionId>
    | SelectWires of list<ConnectionId>
    | UpdateWires of list<ComponentId> * XYPos
    | DragWire of ConnectionId * MouseT
    | ColorWires of list<ConnectionId> * HighLightColor
    | ErrorWires of list<ConnectionId>
    | ResetJumps of list<ConnectionId>
    | MakeJumps of list<ConnectionId>
    | ResetModel // For Issie Integration
    | LoadConnections of list<Connection> // For Issie Integration

//-------------------------Debugging functions---------------------------------//
//let ppSId (sId:SegmentId) =
//    sId
//    |> (fun (SegmentId x) -> x)
//    |> Seq.toList
//    |> (fun chars -> chars[0..2])
//    |> List.map string
//    |> String.concat ""
//
//let ppS (seg:ASeg) =
//    sprintf $"|{seg.Index}:{ppSId seg.Id}|"
//
//let ppWId (wId:ConnectionId) =
//        wId
//        |> (fun (ConnectionId x) -> x)
//        |> Seq.toList
//        |> (fun chars -> chars[0..2])
//        |> List.map string
//        |> String.concat ""
//
//let ppMaps (model:Model) =
//    let mhv = model.FromHorizontalToVerticalSegmentIntersections
//    let mvh = model.FromVerticalToHorizontalSegmentIntersections
//    let m1 =
//        mhv
//        |> Map.toList
//        |> List.map (fun (sid,lst) ->
//            List.map (snd >> ppSId) lst
//            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
//            |> String.concat ";\n"
//    let m2 =
//        mvh
//        |> Map.toList
//        |> List.map (fun (sid,lst) ->
//            List.map (snd >> ppSId) lst
//            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
//            |> String.concat ";\n"
//    let jumps =
//        model.WX
//        |> Map.toList
//        |> List.map (fun (_wId,w) ->
//            sprintf $"Wire: {w.Segments |> List.collect (fun seg -> seg.JumpCoordinateListA |> List.map (fun (_f, sid) -> ppSId sid))}")
//            
//    printfn $"\n------------------\nMapHV:\n {m1} \n MapVH\n{m2} \nJumps:\n {jumps}\n------------------\n"
//
//
//
//let ppSeg seg (model: Model) = 
//        let cid,sid = seg
//        let wire = model.WX[cid]
//        let sg = List.find (fun (s:ASeg) -> s.Id = sid ) wire.Segments
//        let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
//        sprintf $"""[{ppSId sg.Id}: {pxy sg.Start}->{pxy sg.End}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""
//
//let pp segs (model: Model)= 
//    segs
//    |> List.map  ( fun seg ->
//        let cid,sid = seg
//        let wire = model.WX[cid]
//        match List.tryFind (fun (s:ASeg) -> s.Id = sid ) wire.Segments with
//        | Some  sg ->
//            let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
//            sprintf $"""[{pxy sg.Start}->{pxy sg.End}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""
//        | None -> "XX")
//    |> String.concat ";"

//-------------------------------Implementation code----------------------------//

//---------------------------------------------------------------------------//
//                         ad3919 code section start                         //
//---------------------------------------------------------------------------//

let ASegsToVertices (segList:ASeg list) = 
    let firstCoord = (segList[0].Start.X, segList[0].Start.Y)
    let verticesExceptFirst = List.map (fun seg -> (seg.End.X,seg.End.Y)) segList
    [firstCoord] @ verticesExceptFirst

let RISegsToVertices (segList: RISeg list) =
    segList
    |> List.scan getRISegEnd segList[0].Start
    |> List.map (fun pos -> pos.X, pos.Y)

/// Get initial list of wire vertices given port locations corresponding to the enpoints of a wire
let initialWireVerticesFromPorts (portCoords : XYPos * XYPos)  = 
    let startX, startY, endX, endY = snd(portCoords).X, snd(portCoords).Y, fst(portCoords).X, fst(portCoords).Y

    // adjust length of segments 0 and 6 - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let stickLength =
        if (endX - startX > 0.0) then
            let d = List.max [ abs (startX - endX) ; abs (startY - endY) ; Wire.stickLength / 4.0 ]
            min d (Wire.stickLength / 2.0)
        else
            Wire.stickLength / 2.0

    if endX - startX >= stickLength * 2.0 then
        [ // Wire travelling left to right (positive X) from output port to input port
            {X = startX; Y = startY};
            {X = startX + stickLength; Y = startY};
            {X = startX + stickLength; Y = startY};
            {X = (startX + endX) / 2.0; Y = startY};
            {X = (startX + endX) / 2.0; Y = endY};
            {X = endX - stickLength; Y = endY}
            {X = endX - stickLength; Y = endY}
            {X = endX; Y = endY}
        ], true // left to right
    elif abs (startY - endY) < 4.0 then
        [ // Wire travelling right to left (negative X), but ports are (almost) aligned vertically
          // An offset is added to the main horizontal segment so it can be seen / dragged more easily
            {X = startX; Y = startY}
            {X = startX+Wire.stickLength; Y = startY}
            {X = startX+Wire.stickLength; Y = startY}
            {X = startX+Wire.stickLength; Y = startY + Wire.stickLength}
            {X = endX-Wire.stickLength; Y = startY + Wire.stickLength}
            {X = endX-Wire.stickLength; Y = endY}
            {X = endX-Wire.stickLength; Y = endY}
            {X = endX; Y = endY}
        ], false // not left to right
    else
        [ // Wire travelling right to left (negative X), bending back on itself
            {X = startX; Y = startY}
            {X = startX+Wire.stickLength; Y = startY}
            {X = startX+Wire.stickLength; Y = startY}
            {X = startX+Wire.stickLength; Y = (startY+endY)/2.0}
            {X = endX-Wire.stickLength; Y = (startY+endY)/2.0}
            {X = endX-Wire.stickLength; Y = endY}
            {X = endX-Wire.stickLength; Y = endY}
            {X = endX; Y = endY}
        ], false // not left to right

let inferOrientationFromVertices (xyVerticesList: XYPos list) =
    if xyVerticesList.Length <> 8 then 
        failwithf $"Can't perform connection type inference except with 8 vertices: here given {xyVerticesList.Length} vertices"
    let getDir (vs:XYPos) (ve:XYPos) =
        match sign ((abs vs.X - abs ve.X)*(abs vs.X - abs ve.X) - (abs vs.Y - abs ve.Y)*(abs vs.Y - abs ve.Y)) with
        | 1 -> Some Horizontal
        | -1 -> Some Vertical
        | _ -> None
    let midS, midE = xyVerticesList[3], xyVerticesList[4]
    let first,last = xyVerticesList[1], xyVerticesList[5]
    let xDelta = abs last.X - abs first.X
    match getDir midS midE, abs xDelta > 20.0, xDelta > 0.0 with
    | Some Horizontal, _, _ when midE.X < midS.X -> Some Horizontal
    | Some Vertical, _, _ -> Some Vertical 
    | _, true, true -> Some Vertical
    | _, true, false -> Some Horizontal
    | _, false, _ -> None

/// this turns a list of vertices into a list of absolute segments
let convertVerticesToASegs connId (isLeftToRight: bool) (xyVerticesList: XYPos list) =
    let dirs = 
        match isLeftToRight with
        | true -> // for 5 adjustable segments left-to-right
            [Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical;Horizontal]
        | false -> // for 3 adjustale segments right-to-left
            [Horizontal;Horizontal;Vertical;Horizontal;Vertical;Horizontal;Horizontal]

    List.pairwise xyVerticesList
    |> List.mapi (
        fun i ({X=startX; Y=startY},{X=endX; Y=endY}) ->    
            {
                Id = SegmentId(JSHelpers.uuid())
                Index = i
                Start = {X=startX;Y=startY};
                End = {X=endX;Y=endY};
                Dir = dirs[i]
                HostId  = connId;
                JumpCoordinateListA = [];
                Draggable =
                    match i with
                    | 1 | 5 ->  isLeftToRight
                    | 0  | 6  -> false
                    | _ -> true
                ManualRoute = false
            })

let convertVerticesToRISegs connId (isLeftToRight: bool) (verticesList: XYPos list) : RISeg list =
    convertVerticesToASegs connId isLeftToRight verticesList
    |> List.map aSegToRISeg
    // TODO: native RISeg implementation

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToASegs connId (verticesList: list<float*float>) : ASeg list =
    let XYPosList =
        verticesList |> List.map (fun (x,y) -> {X=x;Y=y})

    let makeNewSegmentsFromPorts (xyList: XYPos list) : ASeg list =
        initialWireVerticesFromPorts (xyList[0], xyList[xyList.Length - 1])
        |> (fun (vertices, isLeftToRight) -> convertVerticesToASegs connId isLeftToRight vertices)

    if XYPosList.Length <> 8 then // wire must have 7 segments and so 8 vertices, if not: reroute from endpoints
        makeNewSegmentsFromPorts XYPosList
    else 
        match inferOrientationFromVertices XYPosList with
        | Some Vertical ->
            printfn "Converting vertical"
            convertVerticesToASegs connId true XYPosList
        | Some Horizontal ->
            printfn "Converting horizontal"
            convertVerticesToASegs connId false XYPosList
        | _ -> // can't work out what vertices are, so default to auto-routing
            printfn "Converting unknown"
            makeNewSegmentsFromPorts XYPosList

let issieVerticesToRISegs connId (verticesList: (float * float) list) : RISeg list =
    issieVerticesToASegs connId verticesList
    |> List.map aSegToRISeg
    // TODO: native RISeg implementation

//----------------------interface to Issie-----------------------//
// Section of functions that offer an interface between our implementation and Issie

/// Converts a BusWire.Wire type in the Model, identified by ConnectionId, to a Connection type
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let wire = wModel.WX[cId]
    // Get contents of DUs
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = wire.Id, wire.InputPort, wire.OutputPort
    { // Return Connection record
        Id = strId
        Source = { Symbol.getPort wModel.Symbol strOutputPort with PortNumber = None } // None for connections 
        Target = { Symbol.getPort wModel.Symbol strInputPort with PortNumber = None } // None for connections 
        Vertices = RISegsToVertices wire.Segments // We don't use vertices
    }

/// Converts BusWire.Wire(s) in WX of supplied Model to list of Connections
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.WX
    |> Map.toList
    |> List.map (fun (connectionId, _) -> extractConnection wModel connectionId)

///Returns the abs of an XYPos object
let absXYPos (pos : XYPos) = 
    {X = abs pos.X; Y = abs pos.Y}

/// Returns truw if 2 segments intersect, given their start (p) and end (q) points
let segmentIntersectsSegment ((p1', q1') : (XYPos * XYPos)) ((p2', q2') : (XYPos * XYPos)) : bool =
    
    // in addition the way that coordinates can be positive or negative but are absed when used is appalling
    // the manual or auto route info per segment should be a separate field in Segmnet, not encoded in the sign of the coordinates
    // that is needed when writing out or reading from Issie, but the write/read process can easily translate to a sane internal data structure in the draw blokc model
    let p1, q1, p2, q2 = absXYPos p1', absXYPos q1', absXYPos p2', absXYPos q2' // TODO: Fix import function so this can be removed
    let commonCoord (segStart: XYPos) (segEnd: XYPos) : (Orientation * float) =
        match segStart with
        | {X = x} when segStart.X = segEnd.X -> Horizontal, x
        | {Y = y} when segStart.Y = segEnd.Y -> Vertical, y
        | _ -> failwithf "Segment must have a common coordinate"
    
    let seg1Ori, seg1Coord = commonCoord p1 q1
    let seg2Ori, seg2Coord = commonCoord p2 q2
    
    if seg1Ori = seg2Ori then
        false
    elif seg1Ori = Horizontal then
        (
            seg1Coord >= min p2.Y q2.Y &&
            seg1Coord <= max p2.Y q2.Y &&
            seg2Coord >= min p1.X q1.X &&
            seg2Coord <= max p1.X q1.X
        )
    else
        (
            seg1Coord >= min p2.X q2.X &&
            seg1Coord <= max p2.X q2.X &&
            seg2Coord >= min p1.Y q1.Y &&
            seg2Coord <= max p1.Y q1.Y
        )

///Returns the absolute segment with positive Start and End coordinates
let makeASegPos (seg : ASeg) =
    {seg with
        Start = absXYPos seg.Start
        End = absXYPos seg.End }

let makeRISegPos (seg: RISeg) =
    { seg with Start = absXYPos seg.Start }

/// Initial list of absolute segments based on positions of ports to be connected
let makeInitialASegList (hostId: ConnectionId) (portCoords: XYPos * XYPos) : list<ASeg> =
    let xyPairs, isLeftToRight = initialWireVerticesFromPorts portCoords
    xyPairs |> convertVerticesToASegs hostId isLeftToRight

/// Initial list of rotation invariant segments based on positions of ports to be connected
let makeInitialRISegList (hostId: ConnectionId) (portCoords: XYPos * XYPos) : RISeg list =
    makeInitialASegList hostId portCoords
    |> List.map aSegToRISeg
    // TODO: native RISeg implementation

/// Render given segment using colour and width properties given
let renderSegment (segment: RISeg) (colour: string) (width: string) : ReactElement = 
    let widthOption = EEExtensions.String.tryParseWith System.Int32.TryParse width // Convert bus width string to int
    let renderWidth, halfWidth = // Convert bus width to render widths
        match widthOption with
        | Some 1 -> 1.5, 0.25
        | Some n when n < int "8" -> 2.5, 0.5
        | _ -> 3.5, 1.0
    let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }

    if segment.Dir = Horizontal then // Horizontal segment, can contain jumps
        let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }
        let jumpSizeX = 9.0
        let jumpSizeY = 6.0
        
        let renderWireSubSegment (vertex: XYPos) (orientation: Orientation) (len: float) : list<ReactElement> =
            let Xa, Ya = vertex.X, vertex.Y
            let Xb, Yb =
                match orientation with
                | Horizontal -> vertex.X + len, vertex.Y
                | Vertical -> vertex.X, vertex.Y + len
            [
                makeCircle Xa Ya circleParameters
                makeLine Xa Ya Xb Yb lineParameters
                makeCircle Xb Yb circleParameters
            ]

        let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
            let x, y = intersectionCoordinate.X, intersectionCoordinate.Y
            let startingPoint = {X = x - jumpSizeX/2.0; Y = y}
            let startingControlPoint = {X = x - jumpSizeX/2.0; Y = y - jumpSizeY}
            let endingControlPoint = {X = x + jumpSizeX/2.0; Y = y - jumpSizeY}
            let endingPoint = {X = x + jumpSizeX/2.0; Y = y}
            [
                makeCircle startingPoint.X startingPoint.Y circleParameters
                makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
                makeCircle endingPoint.X endingPoint.Y circleParameters
            ]
        
        let rec renderMultipleSegmentJumps (segmentJumpCoordinateList : list<float>) (segmentJumpYCoordinate : float) : list<ReactElement> =
            match segmentJumpCoordinateList with
            | [] -> [] // No jumps on segment
            | [jumpOneX] -> // One jump on segment
                renderSingleSegmentJump {X = jumpOneX; Y = segmentJumpYCoordinate}
            | jumpOneX :: jumpTwoX :: jumpXsTail -> // More than one jump on segment
                if (segment.Length < 0) then // Right to left segment
                    renderSingleSegmentJump {X = jumpOneX; Y = segmentJumpYCoordinate} // First jump
                    @ // Segment between first and second jump
                    renderWireSubSegment {X = jumpOneX - jumpSizeX/2.0; Y = segmentJumpYCoordinate} Horizontal jumpSizeX
                    @ // Recursive call for second + any remaining jumps
                    renderMultipleSegmentJumps (jumpTwoX :: jumpXsTail) (segmentJumpYCoordinate)
                else  // Left to right segment
                    renderSingleSegmentJump {X = jumpOneX; Y = segmentJumpYCoordinate} // First jump
                    @ // Segment between first and second jump
                    renderWireSubSegment {X = jumpOneX + jumpSizeX/2.0; Y = segmentJumpYCoordinate} Horizontal (-jumpSizeX)
                    @ // Recursive call for second + any remaining jumps
                    renderMultipleSegmentJumps (jumpTwoX :: jumpXsTail) (segmentJumpYCoordinate)

        let renderWireSegment (seg': RISeg) : list<ReactElement> =
            let seg = riSegToASeg seg'
            let jumpXList =
                if (segment.Length < 0) then // Right to left segment
                    seg.JumpCoordinateListA
                    |> List.map fst
                    |> List.sortDescending
                else // Left to right segment
                    seg.JumpCoordinateListA
                    |> List.map fst
                    |> List.sort
            
            match jumpXList with
                | [] -> renderWireSubSegment seg.Start seg.Dir seg'.Length // No jumps, render segment directly
                | jumpXList ->
                     let jumpY = seg.Start.Y // SHOULD be equal to seg.End.Y since ONLY horizontal segments have jumps
                     let jumpOneX = jumpXList[0]
                     let jumpLastX = jumpXList[(List.length jumpXList) - 1]

                     if (segment.Length < 0) then // Right to left segment
                           // Segment from start to first jump
                         renderWireSubSegment seg.Start Horizontal (jumpOneX + jumpSizeX/2.0 - seg.Start.X)
                         @ // First to last jump with segments in between
                         renderMultipleSegmentJumps jumpXList jumpY
                         @ // Segment from last jump to end
                         renderWireSubSegment {X = jumpLastX - jumpSizeX/2.0; Y = jumpY} Horizontal seg.End.X
                     else  // Left to right segment
                           // Segment from start to first jump
                         renderWireSubSegment seg.Start Horizontal (jumpOneX - jumpSizeX/2.0 - seg.Start.X)
                         @ // First to last jump with segments in between
                         renderMultipleSegmentJumps jumpXList jumpY
                         @ // Segment from last jump to end
                         renderWireSubSegment {X = jumpLastX + jumpSizeX/2.0; Y = jumpY} Horizontal seg.End.X

        let wireSegmentReactElementList =
            segment |> renderWireSegment

        g [] wireSegmentReactElementList
    
    else // Vertical Segment, cannot contain jumps
        let segEnd = getRISegEnd segment.Start segment
        let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, segEnd.X, segEnd.Y
        let segmentElements =
            [
                makeCircle Xa Ya circleParameters
                makeLine Xa Ya Xb Yb lineParameters
                makeCircle Xb Yb circleParameters
            ]
        g [] segmentElements

///
type WireRenderProps =
    {
        key: string
        Segments: RISeg list
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortLocation: XYPos
    }

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let renderWireSegmentList : list<ReactElement> =
                props.Segments
                |> List.map ( fun (segment: RISeg) -> renderSegment segment (props.ColorP.Text()) (string props.StrokeWidthP) )
                // Call render helper functions on each segment, including jump rendering

            let renderWireWidthText : ReactElement =
                let textParameters =
                    {
                        TextAnchor = "left";
                        FontSize = "12px";
                        FontWeight = "Bold";
                        FontFamily = "Verdana, Arial, Helvetica, sans-serif";
                        Fill = props.ColorP.Text();
                        UserSelect = UserSelectOptions.None;
                        DominantBaseline = "middle";
                    }
                let textString = if props.StrokeWidthP = 1 then "" else string props.StrokeWidthP // Only print if width > 1
                makeText (props.OutputPortLocation.X+1.0) (props.OutputPortLocation.Y-7.0) (textString) textParameters
            g [] ([ renderWireWidthText ] @ renderWireSegmentList)
        
    , "Wire"
    , equalsButFunctions
    )
   
let view (model: Model) (dispatch: Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    let wiresArray =
        model.WX
        |> Map.toArray
        |> Array.map snd
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    let wires =
        wiresArray
        |> Array.map
            (
                fun wire ->
                    let stringOutId =
                        match wire.OutputPort with
                        | OutputPortId stringId -> stringId

                    let outputPortLocation = Symbol.getOnePortLocationNew model.Symbol stringOutId PortType.Output
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            Segments = List.map makeRISegPos wire.Segments
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                        }
                    singleWireView props
            )
    TimeHelpers.instrumentTime "WirePrepareProps" rStart // Time creation of wires
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start // Time total view funtion and return original output

//---------------------------------------------------------------------------//
//                          ad3919 code section end                          //
//---------------------------------------------------------------------------//

/// This function is given two couples of
/// points that define two line segments and it returns:
/// - Some (x, y) if the two segments intersect;
/// - None if the do not.
let segmentIntersectsSegmentCoordinates ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : Option<XYPos> =
    
    if (segmentIntersectsSegment (p1, q1) (p2, q2)) then
        let x1, y1, x2, y2 = abs p1.X, abs p1.Y, abs q1.X, abs q1.Y
        let x3, y3, x4, y4 = abs p2.X, abs p2.Y, abs q2.X, abs q2.Y
        let uA = ((x4-x3)*(y1-y3) - (y4-y3)*(x1-x3)) / ((y4-y3)*(x2-x1) - (x4-x3)*(y2-y1))

        let intersectionX = x1 + (uA * (x2-x1)) // if coordinates are wanted, maybe useful later
        let intersectionY = y1 + (uA * (y2-y1))
        Some {X = intersectionX; Y = intersectionY}
    
    else None

/// This funtion is given a bounding box and it returns the coordinates
/// of the top-left and the bottom-right corners of this bounding box.
let getTopLeftAndBottomRightCorner (box : BoundingBox) : XYPos * XYPos = 
    let {BoundingBox.X = x; BoundingBox.Y = y} = box
    let {BoundingBox.H = h; BoundingBox.W = w} = box
    let coords = [(x, y); (x, y+h); (x+w, y); (x+w, y+h)]
    let topLeft = List.min coords
    let bottomRight = List.max coords

    {X = fst(topLeft) ; Y = snd(topLeft)} , {X = fst(bottomRight) ; Y = snd(bottomRight)}

/// This function is given a Segment and a BoundingBox
/// and it returns:
/// - (false, None) if the segment does not intersect the bounding box
/// - (true, None) if the segment is fully included inside the bounding box
/// - (true, Some coordinate)  if the segment intersects the bounding box
let segmentIntersectsBoundingBoxCoordinates (segIn : ASeg) (bb : BoundingBox) : bool * Option<XYPos> =
    let seg = makeASegPos segIn
    let ({X = x; Y = y} : XYPos), ({X = a; Y = b} : XYPos) = getTopLeftAndBottomRightCorner bb
    let w , h = (a-x), (b-y) // a = x+w;  b = y+h
    let x1, y1, x2, y2 = seg.Start.X, seg.Start.Y, seg.End.X, seg.End.Y 

    let segPointInBox =
        (
            ( (x1 > x) && (x1 < (x+w)) ) && ( (y1 > y) && (y1 < (y+h)) )
        )
        ||
        (
            ( (x2 > x) && (x2 < (x+w)) ) && ( (y2 > y) && (y2 < (y+h)) )
        )

    let left = segmentIntersectsSegmentCoordinates (seg.Start, seg.End) ({X=x; Y=y}, {X=x; Y=y+h})
    let right = segmentIntersectsSegmentCoordinates (seg.Start, seg.End) ({X=x+w; Y=y}, {X=x+w; Y=y+h})
    let top = segmentIntersectsSegmentCoordinates (seg.Start, seg.End) ({X=x; Y=y}, {X=x+w; Y=y})
    let bottom = segmentIntersectsSegmentCoordinates (seg.Start, seg.End) ({X=x; Y=y+h}, {X=x+w; Y=y+h})
    
    let (intersectionList : list<XYPos>) = 
        [top; bottom; left; right]
        |> List.choose id

    if intersectionList.Length = 0 then
        if segPointInBox then
            true, None
        else
            false, None
    else
        let intersection = 
            intersectionList
            |> List.head
        true, Some intersection

/// This distance is given a point and a segment
/// and it returns the distance between them.
let distanceFromPointToSegment (point : XYPos) (segment : ASeg) : float = 
    let x0, y0 = point.X, abs point.Y
    let x1, y1, x2, y2 = abs segment.Start.X, abs segment.Start.Y, abs segment.End.X, abs segment.End.Y

    if (x1 = x2) then abs (x1 - x0)
    elif (y1 = y2) then abs (y1 - y0)
    else
        let numerator = abs (  (x2-x1)*(y1-y0) - (x1-x0)*(y2-y1)  )
        let denominator = sqrt (  (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)  )
        numerator/denominator

/// This function takes the current state of the model and the
/// IDs of the wires to be rerouted (i.e. updated) as inputs,
/// it REROUTES ALL THE GIVEN WIRES using the default wire
/// shapes defined and it returns the model updated.
let routeGivenWiresBasedOnPortPositions (wiresToBeRouted : list<ConnectionId>) (model : Model) : Model = 
    let updatedWireMap = 
        wiresToBeRouted
        |> List.map (fun id -> model.WX[id])
        |> List.map
            (
                fun wire -> 
                    let posTuple = Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
                    (wire.Id, {wire with Segments = makeInitialRISegList wire.Id posTuple})
            )
        |> Map.ofList
    
    let newWX = 
        model.WX
        |> Map.map (fun id wire -> if Map.containsKey id updatedWireMap then updatedWireMap[id] else wire)

    {model with WX = newWX}

/// Given the current state of the BusWire model,
/// a ConnectionId and an BoundingBox,
/// this function returns a list of Segments of the
/// wire corresponding to the given id that intersect the bounding box.
let getIntersectingSegments (model:Model) (wireId:ConnectionId) (selectBox:BoundingBox) : list<ASeg> =     
    model.WX[wireId].Segments
    |> List.map riSegToASeg
    |> List.filter (fun seg -> fst(segmentIntersectsBoundingBoxCoordinates seg selectBox))


//Finds the closest segment in a wire to a point using euclidean distance
let getClosestSegment (model : Model) (wireId : ConnectionId) (pos : XYPos) : ASeg =
    model.WX[wireId].Segments
    |> List.map riSegToASeg
    |> List.minBy (
        fun seg -> 
            distanceFromPointToSegment pos seg)

/// Function called when a wire has been clicked, so no need to be an option
let getClickedSegment (model:Model) (wireId: ConnectionId) (pos: XYPos) : SegmentId =
    let boundingBox = {X = pos.X - 5.0; Y = pos.Y - 5.0; H = 10.0; W = 10.0}
    let intersectingSegments = getIntersectingSegments model wireId boundingBox

    //getIntersecting segments may not return anything at low resolutions as the mouse was not on any segment, but in range of the wire bbox
    //In this case just return the segment closest to mouse position
    //TODO - should it just do this anyway?
    if List.isEmpty intersectingSegments 
    then (getClosestSegment model wireId pos).Id
    else (List.head intersectingSegments).Id

let checkSegmentAngle (seg:ASeg) (name:string) =
    match seg.Dir with
    | Vertical -> abs (abs seg.Start.X - abs seg.End.X) < 0.000001
    | Horizontal -> abs (abs seg.Start.Y - abs seg.End.Y) < 0.000001
    |> (fun ok ->
        if not ok then  
            printfn $"Weird segment '{name}':\n{seg}\n\n fails angle checking")

let segPointsLeft (seg:ASeg) =
    abs seg.Start.X > abs seg.End.X && seg.Dir = Horizontal

let segXDelta seg = abs seg.End.X - abs seg.Start.X

/// change the middle X coordinate of the joined ends of two segments (seg0 is LH, seg1 is RH).
/// compensate for negative signs in coordinates using as value but preserving sign
/// xPos is asumed positive
let moveXJoinPos xPos seg0 seg1 =
    let changeXKeepingSign (coord:XYPos) =
        if coord.X < 0.0 then {coord with X = -xPos}
        else {coord with X = xPos}
    [ {seg0 with End = changeXKeepingSign seg0.End}; {seg1 with Start = changeXKeepingSign seg1.Start} ]

let changeLengths isAtEnd seg0 seg1 =
    let outerSeg, innerSeg =
        if isAtEnd then seg1, seg0 else seg0, seg1
    let innerX = segXDelta innerSeg
    let outerX = segXDelta outerSeg

    // should never happen, can't do anything
    if seg0.Dir <> Horizontal || seg1.Dir <> Horizontal || outerX < 0.0 then [seg0 ; seg1]
    elif innerX < 0.0 then  
        // the case where we need to shorten the first or last segment (seg0 here)
        moveXJoinPos (if isAtEnd then seg1.End.X - Wire.stickLength else seg0.Start.X + Wire.stickLength) seg0 seg1
    else [ seg0; seg1]
       

/// Called for segments 1, 2, 3, 4, 5 - if they are vertical and move horizontally.
/// The function returns distance reduced if need be to prevent wires moving into components
/// approx equality test is safer tehn exact equality - but probably not needed.
let getSafeDistanceForMove (seg: ASeg) (seg0:ASeg) (seg6:ASeg) (distance:float) =
    let shrink = match seg.Index with | 1 | 2 | 4 | 5 -> 0.5 | _ -> 1.0
    match seg.Index with
    | _ when seg.Dir = Horizontal ->
        distance
    | 3 when distance < 0.0 && abs (abs seg0.Start.Y - abs seg.Start.Y) > 0.0001 ->
        distance
    | 3 when distance > 0.0 && abs (abs seg6.Start.Y - abs seg.End.Y) > 0.0001 ->
        distance
    | 1 | 2 -> 
        let minDistance = seg0.Start.X + Wire.stickLength * shrink - abs seg.End.X
        max minDistance distance
    | 4 | 5 ->
        let maxDistance = seg6.End.X -  Wire.stickLength * shrink - abs seg.Start.X
        min maxDistance distance
    | 3 ->
        let minDistance = abs seg0.Start.X + Wire.stickLength * shrink - abs seg.Start.X
        let maxDistance = abs seg6.End.X -  Wire.stickLength * shrink - abs seg.Start.X
        distance
        |> max minDistance
        |> min maxDistance        
        
    | _ -> 
        distance

        
/// Adjust wire so that two adjacent horizontal segments that are in opposite directions
/// get eliminated
let removeRedundantSegments  (segs: ASeg list) =
    let setAbsX x (pos: XYPos) =
        let x = if pos.X < 0.0 then - abs x else abs x
        {pos with X = x}
    let xDelta seg = abs seg.End.X - abs seg.Start.X
    let setStartX x (seg:ASeg) = {seg with Start = setAbsX x seg.Start}
    let setEndX x (seg:ASeg) = {seg with End = setAbsX x seg.End}
    let adjust seg1 seg2 =
        let xd1, xd2 = xDelta seg1, xDelta seg2
        if seg1.Dir = Horizontal && 
           seg2.Dir = Horizontal && 
           sign xd1 <> sign xd2 
        then
            if abs xd1 > abs xd2 then
                [setEndX seg2.End.X seg1; setStartX seg2.End.X seg2]
            else
                [setEndX seg1.Start.X seg1; setStartX seg1.End.X seg2]
        else
            [seg1;seg2]
    adjust segs[0] segs[1] @  segs[2..4] @ adjust segs[5] segs[6]
       

/// This function allows a wire segment to be moved a given amount in a direction perpedicular to
/// its orientation (Horizontal or Vertical). Used to manually adjust routing by mouse drag.
/// The moved segment is tagged by negating one of its coordinates so that it cannot be auto-routed
/// after the move, thus keeping the moved position.
let moveSegment (seg:ASeg) (distance:float) (model:Model) = 
    let wire = model.WX[seg.HostId]
    let wireSegs = wire.Segments |> List.map riSegToASeg
    let index = seg.Index
    if index <= 0 || index >= wireSegs.Length - 1 then
        failwithf $"Buswire segment index {index} out of range in moveSegment in wire length {wireSegs.Length}"
    let prevSeg = wireSegs[index-1]
    let nextSeg = wireSegs[index+1]
    if seg.Dir = prevSeg.Dir || seg.Dir = nextSeg.Dir then
        wire
    else
        //runTestFable()
        distance      
        |> getSafeDistanceForMove seg wireSegs[0] wireSegs[6]   
        |> (fun distance' ->
            let newPrevEnd, newSegStart, newSegEnd, newNextStart = 
                match seg.Dir with

                | Vertical -> 
                    {prevSeg.End with X = - (abs seg.Start.X + distance')}, 
                    {seg.Start with X = - (abs seg.Start.X + distance')}, 
                    {seg.End with X = - (abs seg.End.X + distance')}, 
                    {nextSeg.Start with X = - (abs seg.End.X + distance')}

                | Horizontal -> 
                    {prevSeg.End with Y = - (abs seg.Start.Y + distance')}, 
                    {seg.Start with Y = - (abs seg.Start.Y + distance')}, 
                    {seg.End with Y = - (abs seg.End.Y + distance')}, 
                    {nextSeg.Start with Y = - (abs seg.End.Y + distance')}

            let newPrevSeg = {prevSeg with End = newPrevEnd}
            let newSeg = {seg with Start = newSegStart;End = newSegEnd}
            let newNextSeg = {nextSeg with Start = newNextStart}
        
            let newSegments =
                wireSegs[.. index-2] @ [newPrevSeg; newSeg; newNextSeg] @ wireSegs[index+2 ..]
                |> removeRedundantSegments

            {wire with Segments = List.map aSegToRISeg newSegments})

/// Initialisatiton with no wires
let init () =
    let symbols,_ = Symbol.init()
    {   
        WX = Map.empty;
        FromVerticalToHorizontalSegmentIntersections = Map.empty;
        FromHorizontalToVerticalSegmentIntersections = Map.empty;
        Symbol = symbols; 
        CopiedWX = Map.empty; 
        SelectedSegment = SegmentId(""); 
        LastMousePos = {X = 0.0; Y = 0.0};
        ErrorWires = []
        Notifications = None
    } , Cmd.none

///Returns the wires connected to a list of components given by componentIds
let getConnectedWires (wModel : Model) (compIds : list<ComponentId>) =
    let inputPorts, outputPorts = Symbol.getPortLocations wModel.Symbol compIds

    wModel.WX
    |> Map.toList
    |> List.map snd
    |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts || Map.containsKey wire.OutputPort outputPorts)
    |> List.map (fun wire -> wire.Id)
    |> List.distinct

///Returns a tuple of: wires connected to inputs ONLY, wires connected to outputs ONLY, wires connected to both inputs and outputs
let filterWiresByCompMoved (wModel : Model) (compIds : list<ComponentId>) =
        let inputPorts, outputPorts = Symbol.getPortLocations wModel.Symbol compIds
        let lst = 
            wModel.WX
            |> Map.toList
            |> List.map snd

        let inputWires =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        let outputWires =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.OutputPort outputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        let fullyConnected =
            lst
            |> List.filter (fun wire -> Map.containsKey wire.InputPort inputPorts && Map.containsKey wire.OutputPort outputPorts)
            |> List.map (fun wire -> wire.Id)
            |> List.distinct

        (inputWires, outputWires, fullyConnected)

//Returns a newly autorouted wire given a model and wire
let autorouteWire (model : Model) (wire : Wire) : Wire =
    let posTuple = Symbol.getTwoPortLocations (model.Symbol) (wire.InputPort) (wire.OutputPort)
    {wire with Segments = List.map aSegToRISeg (makeInitialASegList wire.Id posTuple)}

/// reverse segment order, and Start, End coordinates, so list can be processed from input to output
/// this function is self-inverse
let revSegments (segs:ASeg list) =
    List.rev segs
    |> List.map (fun seg -> {seg with Start = seg.End; End = seg.Start})

//
//  ====================================================================================================================
//
//                                        WIRE SEGMENTS FOR ROUTING
//
//
// Segments, going from Start (output port) to End (input port) coords, are summarised as:
// H => Horizontal (incr X)
// V => Vertical (incr Y)
// 0 => zero length segment (never used)
//
// segment qualifiers:
// F => min length (next to output or input, cannot be shortened)
//
// "Simple" case where output.X < input.X and 3 segment autoroute is possible
//  S0.FH  S1.0V  S2.H  S3.V  S4.H  S5.0V S6.FH
//
// "Complex" case where output.X > input.X and wire ends back for 5 segment autoroute
//  S0.FH  S1.V  S2.H  S3.V  S4.H  S5.0V S6.FH (not sure if H and V are correct here)
//
// To determine adjustment on End change we just reverse the segment and apply the Start change algorithm
// Adjustment => reverse list of segments, swap Start and End, and alter the sign of all coordinates
// For simplicity, due to the encoding of manual changes into coordinates by negating them (yuk!)
// we do not alter coordinate sign. Instead we invert all numeric comparisons.
// There are no constants used in the algorithm (if there were, they would need to be negated)
//
// ======================================================================================================================


let inline addPosPos (pos1: XYPos) (pos:XYPos) =
    {X = pos1.X + pos.X; Y = pos1.Y + pos.Y}


let inline moveEnd (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:ASeg) -> if i = n then {seg with End = mover seg.End} else seg)


let inline moveStart (mover: XYPos -> XYPos) (n:int) =
    List.mapi (fun i (seg:ASeg) -> if i = n then {seg with Start = mover seg.Start} else seg)

let inline moveAll (mover: XYPos -> XYPos) (n : int) =
    List.mapi (fun i (seg:ASeg) -> if i = n then {seg with Start = mover seg.Start; End = mover seg.End} else seg)

let  transformXY tX tY (pos: XYPos) =
    {pos with X = tX pos.X; Y = tY pos.Y}

let transformSeg tX tY (seg: ASeg) =
    let trans = transformXY tX tY
    {seg with Start = trans seg.Start; End = trans seg.End }

let topology (pos1: XYPos) (pos2:XYPos) =
    sign (abs pos1.X - abs pos2.X), sign (abs pos1.Y - abs pos2.Y)

/// Returns None if full autoroute is required or Some segments with initial part of the segment list autorouted
/// up till the first dragged (manually routed) segment.
/// ReverseFun must equal not or id. not => the segments go from input to output (reverse of normal).
/// This allows the same code to work on both ends of the wire, with segment reversal done outside this
/// function to implement input -> output direction.
let partialAutoRoute (segs: ASeg list) (newPortPos: XYPos) =
    let wirePos = segs[0].End
    let portPos = segs[0].Start
    let newWirePos = {newPortPos with X = newPortPos.X + (abs wirePos.X - portPos.X) }
    let (diff:XYPos) = {X=newPortPos.X-portPos.X; Y= newPortPos.Y - portPos.Y}
    let lastAutoIndex =
        let isNegative (pos:XYPos) = pos.X < 0.0 || pos.Y < 0.0
        let isAutoSeg (seg: ASeg) = 
            not (isNegative seg.Start || isNegative seg.End)
        segs
        |> List.takeWhile isAutoSeg
        |> List.length
        |> (fun n -> if n > 5 then None else Some (n + 1))
    let scaleBeforeSegmentEnd segIndex =
        let seg = segs[segIndex]
        let fixedPt = absXYPos seg.End
        let scale x fx nx wx =
            if nx = fx then x else ((abs x - fx)*(nx-fx)/(abs wx - fx) + fx) * float (sign x)
        let startPos = if segIndex = 1 then portPos else wirePos
        let newStartPos = if segIndex = 1 then newPortPos else newWirePos
        let scaleX x = scale x fixedPt.X newStartPos.X startPos.X
        let scaleY y = scale y fixedPt.Y newStartPos.Y startPos.Y
        match List.splitAt (segIndex+1) segs, segIndex with
        | ((scaledSegs), otherSegs), 1 ->
            Some ((List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        | ((firstSeg :: scaledSegs), otherSegs), _ ->
            Some ((moveAll (addPosPos diff) 0 [firstSeg] @ List.map (transformSeg scaleX scaleY) scaledSegs) @ otherSegs)
        | _ -> None

    let checkTopology index =
        let finalPt = segs[6].Start
        let oldTop x = topology (if index = 1 then portPos else wirePos) x
        let newTop x = topology (if index = 1 then newPortPos else newWirePos) x
        if oldTop finalPt <> newTop finalPt then
            // always aandon manual routing
            None 
        else
            let manSegEndPt = segs[index].End
            let oldT = oldTop manSegEndPt
            let newT = newTop manSegEndPt
            if oldT = newT then
                Some index
            else
                None
    lastAutoIndex
    |> Option.bind checkTopology
    |> Option.bind scaleBeforeSegmentEnd


///Returns the new positions keeping manual coordinates negative, and auto coordinates positive
let negXYPos (pos : XYPos) (diff : XYPos) : XYPos =
    let newPos = Symbol.posAdd (absXYPos pos) diff
    if pos.X < 0. || pos.Y < 0. then {X = - newPos.X; Y = - newPos.Y}
    else newPos

///Moves a wire by a specified amount by adding a XYPos to each start and end point of each segment
let moveWire (wire : Wire) (diff : XYPos) =    
    {wire with 
        Segments = 
            wire.Segments
            |> List.map (fun seg -> 
                {seg with
                    Start = negXYPos seg.Start diff
                })
    }

/// Re-routes a single wire in the model when its ports move.
/// Tries to preserve manual routing when this makes sense, otherwise re-routes with autoroute.
/// Partial routing from input end is done by reversing segments and and swapping Start/End
/// inout = true => reroute input (target) side of wire.
let updateWire (model : Model) (wire : Wire) (inOut : bool) =
    let newPort = 
        match inOut with
        | true -> Symbol.getInputPortLocation model.Symbol wire.InputPort
        | false -> Symbol.getOutputPortLocation model.Symbol wire.OutputPort
    if inOut then
        partialAutoRoute (revSegments (List.map riSegToASeg wire.Segments)) newPort
        |> Option.map revSegments
    else 
        partialAutoRoute (List.map riSegToASeg wire.Segments) newPort
    |> Option.map (fun segs -> {wire with Segments = (List.map aSegToRISeg segs)})
    |> Option.defaultValue (autorouteWire model wire)

let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =
    let mutable newWX = model.WX
    // Arrays are faster to check than lists
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps
    let changeJumps wid index jumps =
        let jumps = List.sortDescending jumps
        let changeSegment segs =
            List.mapi (fun i x -> if i <> index then x else { x with JumpCoordinateListA = jumps }) segs

        newWX <- Map.add wid { newWX[wid] with Segments = List.map aSegToRISeg (changeSegment (List.map riSegToASeg newWX[wid].Segments)) } newWX

    let segs =
        model.WX
        |> Map.toArray
        |> Array.mapi (fun _i (_wid, w) -> List.toArray w.Segments)

    for w1 in 0 .. segs.Length - 1 do
        for h' in segs[w1] do
            let h = riSegToASeg h'
            if h.Dir = Horizontal then
                // work out what jumps this segment should have
                let mutable jumps: (float * SegmentId) list = []
                
                if not (Array.contains h.HostId wiresWithNoJumpsA) then
                    for w2 in 0 .. segs.Length - 1 do
                        // everything inside the inner loop should be very highly optimised
                        // it is executed n^2 time where n is the number of segments (maybe 5000)
                        // the abs here are because segment coordinates my be negated to indicate manual routing
                        for v' in segs[w2] do
                            let v = riSegToASeg v'
                            if not (Array.contains v.HostId wiresWithNoJumpsA) then
                                match v.Dir with
                                | Vertical ->
                                    let x, x1, x2 = abs v.Start.X, abs h.Start.X, abs h.End.X
                                    let y, y1, y2 = abs h.Start.Y, abs v.Start.Y, abs v.End.Y
                                    let xhi, xlo = max x1 x2, min x1 x2
                                    let yhi, ylo = max y1 y2, min y1 y2
                                    //printfn $"{[xlo;x;xhi]}, {[ylo;y;yhi]}"
                                    if x < xhi - 5.0 && x > xlo + 5.0 && y < yhi - 5.0 && y > ylo + 5.0 then
                                        //printfn "found a jump!"
                                        jumps <- (x, v.Id) :: jumps
                                | _ -> ()
                    // compare jumps with what segment now has, and change newWX if need be
                // note that if no change is needed we do not update WX
                // simple cases are done without sort for speed, proably not necessary!
                // The jump list is sorted in model to enable easier rendering of segments
                match jumps, h.JumpCoordinateListA with
                | [], [] -> ()
                | [ a ], [ b ] when a <> b -> changeJumps h.HostId h.Index jumps
                | [], _ -> changeJumps h.HostId h.Index jumps
                | _, [] -> // in this case we need to sort the jump list
                    changeJumps h.HostId h.Index (List.sort jumps)
                | newJumps, oldJ ->
                    let newJ = List.sort newJumps
                    // oldJ is already sorted (we only ever write newJ back to model)
                    if newJ <> oldJ then changeJumps h.HostId h.Index newJumps else ()

    { model with WX = newWX }


let updateWireSegmentJumps (_wireList: list<ConnectionId>) (wModel: Model) : Model =
    let startT = TimeHelpers.getTimeMs()
    let model = makeAllJumps [] wModel
    TimeHelpers.instrumentTime "UpdateJumps" startT
    model



/// This function updates the wire model by removing from the stored lists of intersections
/// all those generated by wireList wires.
/// intersetcions are stored in maps on the model and on the horizontal segments containing the jumps
let resetWireSegmentJumps (wireList : list<ConnectionId>) (wModel : Model) : Model =
    makeAllJumps wireList wModel



   
        



/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model : Model) (compIdList : ComponentId list) (diff : XYPos) =

    let (inputWires, outputWires, fullyConnected) = filterWiresByCompMoved model compIdList

    let newWires = 
        model.WX
        |> Map.toList
        |> List.map (fun (cId, wire) -> 
            if List.contains cId fullyConnected //Translate wires that are connected to moving components on both sides
            then (cId, moveWire wire diff)
            elif List.contains cId inputWires //Only route wires connected to ports that moved for efficiency
            then (cId, updateWire model wire true)
            elif List.contains cId outputWires
            then (cId, updateWire model wire false)
            else (cId, wire))
        |> Map.ofList
        
    {model with WX = newWires}

///
let update (msg : Msg) (model : Model) : Model*Cmd<Msg> =
    
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd


    | UpdateWires (componentIdList, diff) -> 
        updateWires model componentIdList diff, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let portOnePos, portTwoPos = Symbol.getTwoPortLocations model.Symbol inputId outputId
        let _wireWidthFromSymbol = WireWidth.Configured 1
        let wireId = ConnectionId(JSHelpers.uuid())
        let segmentList = makeInitialASegList wireId (portOnePos, portTwoPos)
        
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = List.map aSegToRISeg segmentList
                Rotation = CW0
                YReflect = false
            }
            
        let wireAddedMap = Map.add newWire.Id newWire model.WX
        let newModel = updateWireSegmentJumps [wireId] {model with WX = wireAddedMap}

        newModel, Cmd.ofMsg BusWidths

    | BusWidths ->

        let processConWidths (connWidths: ConnectionsWidth) =
            let addWireWidthFolder (wireMap: Map<ConnectionId, Wire>) _ wire  =
                let width =
                    match connWidths[wire.Id] with
                    | Some a -> a
                    | None -> wire.Width
                let newColor = if wire.Color = Purple || wire.Color = Brown then Purple else DarkSlateGrey
                wireMap.Add ( wire.Id, { wire with Width = width ; Color = newColor} )

            let addSymbolWidthFolder (m: Map<ComponentId,Symbol.Symbol>) (_: ConnectionId) (wire: Wire) =
                    let inPort = model.Symbol.Ports[match wire.InputPort with InputPortId ip -> ip]
                    let symId = ComponentId inPort.HostId
                    let symbol = m[symId]

                    match symbol.Compo.Type with
                    | SplitWire _n ->
                        match inPort.PortNumber with 
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"What? wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym m)
                    | MergeWires ->
                        match inPort.PortNumber with 
                        | Some 0 -> 
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 -> 
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"What? wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> m

            let newWX = ((Map.empty, model.WX) ||> Map.fold addWireWidthFolder)

            let symbolsWithWidths =
                (model.Symbol.Symbols, newWX) ||> Map.fold addSymbolWidthFolder

            { model with 
                WX = newWX; Notifications = None ; 
                ErrorWires=[]; 
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}}, Cmd.none    
        


        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model )
        
        
        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with 
                    Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)
    
    | CopyWires (connIds : list<ConnectionId>) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.WX
        { model with CopiedWX = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : list<ConnectionId>) -> 
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Red}
                    else if List.contains id model.ErrorWires then 
                        {wire with Color = HighLightColor.DarkSlateGrey}
                    else wire
                ) 
        
        {model with WX = newWX ; ErrorWires = connectionIds}, Cmd.none

    | SelectWires (connectionIds : list<ConnectionId>) -> //selects all wires in connectionIds, and also deselects all other wires
        let newWX =
            model.WX
            |> Map.map
                (fun id wire -> 
                    if List.contains id model.ErrorWires then
                        if List.contains id connectionIds then 
                            {wire with Color = HighLightColor.Brown} 
                        else 
                            {wire with Color = HighLightColor.Red}
                    else if List.contains id connectionIds then
                        {wire with Color = HighLightColor.Purple} 
                    else
                        {wire with Color = HighLightColor.DarkSlateGrey} 
                ) 
        
        {model with WX = newWX}, Cmd.none

    | DeleteWires (connectionIds : list<ConnectionId>) -> 
        let newModel = resetWireSegmentJumps (connectionIds) (model)
        let newWX =
             newModel.WX
             |> Map.filter (fun id _wire -> not (List.contains id connectionIds))
        {newModel with WX = newWX}, Cmd.ofMsg BusWidths

    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with
        | Down ->
            let segId = getClickedSegment model connId mMsg.Pos
            {model with SelectedSegment = segId }, Cmd.none
        | Drag ->
            let segId = model.SelectedSegment
            let rec getSeg (segList: list<ASeg>) = 
                match segList with
                | h::t -> if h.Id = segId then h else getSeg t
                | _ -> failwithf "segment Id not found in segment list"
            let seg = getSeg (List.map riSegToASeg model.WX[connId].Segments)
            if seg.Draggable then
                let distanceToMove = 
                    match seg.Dir with
                    | Horizontal -> mMsg.Pos.Y - abs seg.Start.Y
                    | Vertical -> mMsg.Pos.X - abs seg.Start.X

                let newWire = moveSegment seg distanceToMove model
                let newWX = Map.add seg.HostId newWire model.WX
 
                {model with WX = newWX}, Cmd.none
            else
                model, Cmd.none
        
        | _ -> model, Cmd.none


    | ColorWires (connIds, color) -> // Just Changes the colour of the wires, Sheet calls pasteWires before this
        let newWires =
            (List.fold (fun prevWires cId -> 
                let oldWireOpt = Map.tryFind cId model.WX
                match oldWireOpt with
                | None -> 
                    printfn "BusWire error: expected wire in ColorWires does not exist"
                    prevWires
                | Some oldWire ->
                    Map.add cId { oldWire with Color = color } prevWires) model.WX connIds)
        { model with WX = newWires }, Cmd.none
    
    | ResetJumps connIds ->
        printfn $"resetting jumps on {connIds.Length} wires"
        
        let newModel =
            model
            |> resetWireSegmentJumps connIds
        
        newModel, Cmd.none
    
    | MakeJumps connIds ->
        printfn $"making jumps on {connIds.Length} wires"

        let newModel =
            model
            |> updateWireSegmentJumps connIds
            
        newModel, Cmd.none
    
    | ResetModel -> { model with WX = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none
    
    | LoadConnections conns -> // we assume components (and hence ports) are loaded before connections
        let posMatchesVertex (pos:XYPos) (vertex: float*float) =
            let epsilon = 0.00001
            abs (abs pos.X - abs (fst vertex)) < epsilon &&
            abs (abs pos.Y - abs (snd vertex)) < epsilon
            |> (fun b -> if not b then printf $"Bad wire endpoint match on {pos} {vertex}"; b else b)
        let newWX =
            conns 
            |> List.map ( fun conn ->
                            let inputId = InputPortId conn.Target.Id
                            let outputId = OutputPortId conn.Source.Id
                            let connId = ConnectionId conn.Id
                            let segments = issieVerticesToASegs connId conn.Vertices
                            let makeWirePosMatchSymbol inOut (wire:Wire) =
                                match inOut with
                                | true -> posMatchesVertex 
                                            (Symbol.getInputPortLocation model.Symbol inputId)
                                            (List.head conn.Vertices)
                                | false ->
                                          posMatchesVertex 
                                            (Symbol.getOutputPortLocation model.Symbol outputId) 
                                            (List.last conn.Vertices)
                                |> (fun b -> 
                                    if b then 
                                        wire 
                                    else
                                        let getS (connId:string) = 
                                            Map.tryFind connId model.Symbol.Ports
                                            |> Option.map (fun port -> port.HostId)
                                            |> Option.bind (fun symId -> Map.tryFind (ComponentId symId) model.Symbol.Symbols)
                                            |> Option.map (fun sym -> sym.Compo.Label)
                                        printfn $"Updating loaded wire from {getS conn.Source.Id}->{getS conn.Target.Id} of wire "
                                        updateWire model wire inOut)
                                
                                
                            connId,
                            {
                                Id = ConnectionId conn.Id
                                InputPort = inputId
                                OutputPort = outputId
                                Color = HighLightColor.DarkSlateGrey
                                Width = 1
                                Segments = List.map aSegToRISeg segments
                                Rotation = CW0
                                YReflect = false
                            }
                            |> makeWirePosMatchSymbol false
                            |> makeWirePosMatchSymbol true
                        )
            |> Map.ofList
        
        let connIds =
            conns
            |> List.map (fun conn -> ConnectionId conn.Id)
            
        { model with WX = newWX }, Cmd.ofMsg (MakeJumps connIds)

//---------------Other interface functions--------------------//

///
let wireIntersectsBoundingBox (w : Wire) (bb : BoundingBox) =
    let boolList = List.map (fun seg -> fst(segmentIntersectsBoundingBoxCoordinates seg bb)) (List.map riSegToASeg w.Segments)
    List.contains true boolList

///
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> = 
    wModel.WX
    |> Map.map (fun _id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun _id boolVal -> boolVal)
    |> Map.toList
    |> List.map (fun (id,_bool) -> id)

///searches if the position of the cursor is on a wire in a model
///Where n is 5 pixels adjusted for top level zoom
let getWireIfClicked (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.X = pos.X - n; Y = pos.Y - n; H = n*2.; W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires

///
let pasteWires (wModel : Model) (newCompIds : list<ComponentId>) : (Model * list<ConnectionId>) =
    let oldCompIds = Symbol.getCopiedSymbols wModel.Symbol
    
    let pastedWires =
        let createNewWire (oldWire : Wire) : list<Wire> =
            let newId = ConnectionId(JSHelpers.uuid())
    
            match Symbol.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->

                let portOnePos, portTwoPos = Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort)
                let segmentList = makeInitialASegList newId (portOnePos, portTwoPos)
                [
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = List.map aSegToRISeg segmentList;
                    }
                ]
            | None -> []
        
        wModel.CopiedWX
        |> Map.toList
        |> List.map snd
        |> List.collect createNewWire
        |> List.map (fun wire -> wire.Id, wire)
        |> Map.ofList
    
    let newWireMap = Map.fold ( fun acc newKey newVal -> Map.add newKey newVal acc ) pastedWires wModel.WX
    let pastedConnIds =
        pastedWires
        |> Map.toList
        |> List.map fst
        
    { wModel with WX = newWireMap }, pastedConnIds

///
let getPortIdsOfWires (model: Model) (connIds: ConnectionId list) : (InputPortId list * OutputPortId list) =
    (([], []), connIds)
    ||> List.fold (fun (inputPorts, outputPorts) connId ->
            (model.WX[connId].InputPort :: inputPorts, model.WX[connId].OutputPort :: outputPorts))
