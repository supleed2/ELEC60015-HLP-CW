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

///
type SnapPosition = High | Mid | Low

///
type Segment = 
    {
        Id : SegmentId
        Index: int
        Start: XYPos
        End: XYPos
        Dir: Orientation
        HostId: ConnectionId
        /// List of x-coordinate values of segment jumps. Only used on horizontal segments.
        JumpCoordinateList: list<float * SegmentId>
        Draggable : bool
    }



///
type Wire =
    {
        Id: ConnectionId 
        InputPort: InputPortId
        OutputPort: OutputPortId
        Color: HighLightColor
        Width: int
        Segments: list<Segment>
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
let ppSId (sId:SegmentId) =
    sId
    |> (fun (SegmentId x) -> x)
    |> Seq.toList
    |> (fun chars -> chars[0..2])
    |> List.map string
    |> String.concat ""

let ppS (seg:Segment) =
    sprintf $"|{seg.Index}:{ppSId seg.Id}|"

let ppWId (wId:ConnectionId) =
        wId
        |> (fun (ConnectionId x) -> x)
        |> Seq.toList
        |> (fun chars -> chars[0..2])
        |> List.map string
        |> String.concat ""

let ppMaps (model:Model) =
    let mhv = model.FromHorizontalToVerticalSegmentIntersections
    let mvh = model.FromVerticalToHorizontalSegmentIntersections
    let m1 =
        mhv
        |> Map.toList
        |> List.map (fun (sid,lst) ->
            List.map (snd >> ppSId) lst
            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
            |> String.concat ";\n"
    let m2 =
        mvh
        |> Map.toList
        |> List.map (fun (sid,lst) ->
            List.map (snd >> ppSId) lst
            |> (fun segs -> sprintf $"""<{ppSId sid}->[{String.concat ";" segs}]>"""))
            |> String.concat ";\n"
    let jumps =
        model.WX
        |> Map.toList
        |> List.map (fun (wId,w) ->
            sprintf $"Wire: {w.Segments |> List.collect (fun seg -> seg.JumpCoordinateList |> List.map (fun (f, sid) -> ppSId sid))}")
            
    printfn $"\n------------------\nMapHV:\n {m1} \n MapVH\n{m2} \nJumps:\n {jumps}\n------------------\n"



let ppSeg seg (model: Model) = 
        let cid,sid = seg
        let wire = model.WX[cid]
        let sg = List.find (fun (s:Segment) -> s.Id = sid ) wire.Segments
        let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
        sprintf $"""[{ppSId sg.Id}: {pxy sg.Start}->{pxy sg.End}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""

let pp segs (model: Model)= 
    segs
    |> List.map  ( fun seg ->
        let cid,sid = seg
        let wire = model.WX[cid]
        match List.tryFind (fun (s:Segment) -> s.Id = sid ) wire.Segments with
        | Some  sg ->
            let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
            sprintf $"""[{pxy sg.Start}->{pxy sg.End}]-{match sg.Dir with | Vertical -> "V" | _ -> "H"}-{sg.Index}"""
        | None -> "XX")
    |> String.concat ";"

//-------------------------------Implementation code----------------------------//

/// Wire to Connection
let segmentsToVertices (segList:Segment list) = 
    let firstCoord = (segList[0].Start.X, segList[0].Start.Y)
    let verticesExceptFirst = List.mapi (fun i seg -> (seg.End.X,seg.End.Y)) segList
    [firstCoord] @ verticesExceptFirst


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// wire vertices
let makeInitialWireVerticesList (portCoords : XYPos * XYPos)  = 
    let xs, ys, Xt, Yt = snd(portCoords).X, snd(portCoords).Y, fst(portCoords).X, fst(portCoords).Y

    // adjust length of segments 0 and 6 - the sticks - so that when two ports are aligned and close you still get left-to-right routing.
    let adjStick = 
        let d = List.max [ abs (xs - Xt) ; abs (ys - Yt) ; Wire.stickLength / 4.0 ]
        if (Xt - xs > 0.0) then
            min d (Wire.stickLength / 2.0)
        else
            Wire.stickLength / 2.0

    // the simple case of a wire travelling from output to input in a left-to-right (positive X) direction
    let leftToRight = 
        [
            {X = xs; Y = ys};
            {X = xs+adjStick; Y = ys};
            {X = xs+adjStick; Y = ys};
            {X = (xs+Xt)/2.0; Y = ys};
            {X = (xs+Xt)/2.0; Y = Yt};
            {X = Xt-adjStick; Y = Yt}
            {X = Xt-adjStick; Y = Yt}
            {X = Xt; Y = Yt}
        ]
    // the case of a wire travelling from output to input in a right-to-left (negative X) direction. Thus must bend back on itself.
    let rightToLeft =
        [
            {X = xs; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = (ys+Yt)/2.0}
            {X = Xt-Wire.stickLength; Y = (ys+Yt)/2.0}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt; Y = Yt}
        ]

    // the special case of a wire travelling right-to-left where the two ends are vertically almost identical. 
    // In this case we ad an offset to the main horizontal segment so it is more visible and can be easily re-routed manually.
    let rightToLeftHorizontal =
        [
            {X = xs; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys}
            {X = xs+Wire.stickLength; Y = ys + Wire.stickLength}
            {X = Xt-Wire.stickLength; Y = ys + Wire.stickLength}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt-Wire.stickLength; Y = Yt}
            {X = Xt; Y = Yt}
        ]

    if Xt - xs >= adjStick * 2.0 then 
        leftToRight, true
    elif abs (ys - Yt) < 4.0 then 
        rightToLeftHorizontal, false
    else 
        rightToLeft, false 

let inferDirectionfromVertices (xyVerticesList: XYPos list) =
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

/// this turns a list of vertices into a list of segments
let xyVerticesToSegments connId (isLeftToRight: bool) (xyVerticesList: XYPos list) =

    let dirs = 
        match isLeftToRight with
        | true -> 
            // for 5 adjustable segments left-to-right
            [Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical;Horizontal]
        | false ->
            // for 3 adjustale segments right-to-left
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
                JumpCoordinateList = [];
                Draggable =
                    match i with
                    | 1 | 5 ->  isLeftToRight
                    | 0  | 6  -> false
                    | _ -> true
            })

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Wire
let issieVerticesToSegments 
        (connId) 
        (verticesList: list<float*float>) =
    let xyVerticesList =
        verticesList
        |> List.map (fun (x,y) -> {X=x;Y=y})

    let makeSegmentsFromVertices (xyList: XYPos list) =
        makeInitialWireVerticesList (xyList[0], xyList[xyList.Length - 1])
        |> (fun (vl, isLeftToRight) -> xyVerticesToSegments connId isLeftToRight vl)
        

    // segments lists must must be length 7, in case legacy vertex list does not conform check this
    // if there are problems reroute
        //vertex lists are one element longer than segment lists
    if xyVerticesList.Length <> 8 then  
        makeSegmentsFromVertices xyVerticesList
    else 
        match inferDirectionfromVertices xyVerticesList with
        | Some Vertical -> 
            printfn "Converting vertical"
            xyVerticesToSegments connId true xyVerticesList
        | Some Horizontal -> 
            printfn "Converting horizontal"
            xyVerticesToSegments connId false xyVerticesList
        | _ ->
            // can't work out what vertices are, so default to auto-routing
            printfn "Converting unknown"
            makeSegmentsFromVertices xyVerticesList
            

    
//----------------------interface to Issie-----------------------//
/// This function is given a ConnectionId and it
/// converts the corresponding BusWire.Wire type to a
/// Connection type, offering an interface
/// between our implementation and Issie.
let extractConnection (wModel : Model) (cId : ConnectionId) : Connection =
    let conn = wModel.WX[cId]
    let ConnectionId strId, InputPortId strInputPort, OutputPortId strOutputPort = conn.Id, conn.InputPort, conn.OutputPort
    {
        Id = strId
        Source = { Symbol.getPort wModel.Symbol strOutputPort with PortNumber = None } // None for connections 
        Target = { Symbol.getPort wModel.Symbol strInputPort with PortNumber = None } // None for connections 
        Vertices = segmentsToVertices conn.Segments
    } // We don't use vertices

/// This function is given a list of ConnectionId and it
/// converts the corresponding BusWire.Wire(s) to a
/// list of Connectio, offering an interface
/// between our implementation and Issie.
let extractConnections (wModel : Model) : list<Connection> = 
    wModel.WX
    |> Map.toList
    |> List.map (fun (key, _) -> extractConnection wModel key)

/// Given three points p, q, r, the function returns true if 
/// point q lies on line segment 'pr'. Otherwise it returns false.
let onSegment (p : XYPos) (q : XYPos) (r : XYPos) : bool = 
    (
        (q.X <= max (p.X) (r.X)) &&
        (q.X >= min (p.X) (r.X)) &&
        (q.Y <= max (p.Y) (r.Y)) &&
        (q.Y >= min (p.Y) (r.Y))
    )
  
/// Given three points p, q, r, the function returns:
/// - 0 if p, q and r are colinear;
/// - 1 if the path that you must follow when you start at p, you visit q and you end at r, is a CLOCKWISE path;
/// - 2 if the path that you must follow when you start at p, you visit q and you end at r, is a COUNTERCLOCKWISE path.
let orientation (p : XYPos) (q : XYPos) (r : XYPos) : int =
    let result = (q.Y - p.Y) * (r.X - q.X) - (q.X - p.X) * (r.Y - q.Y)
  
    if (result = 0.0) then 0 // colinear
    elif (result > 0.0) then 1 // clockwise
    else 2 //counterclockwise

///Returns the abs of an XYPos object
let getAbsXY (pos : XYPos) = 
    {X = abs pos.X; Y = abs pos.Y}
  
/// Given two sets of two points: (p1, q1) and (p2, q2)
/// that define two segments, the function returns true
/// if these two segments intersect and false otherwise.
let segmentIntersectsSegment ((p1, q1) : (XYPos * XYPos)) ((p2, q2) : (XYPos * XYPos)) : bool =
    // this is a terrible implementation
    // determining intersection should be done by finding intersection point and comparing with coords
    // since segments are always horizontal or vertical that is pretty easy.
    // in addition the way that coordinates can be positive or negative but are absed when used is appalling
    // the manual or auto route info per segment should be a separate field in Segmnet, not encoded in the sign of the coordinates
    // that is needed when writing out or reading from Issie, but the write/read process can easily translate to a sane internal data structure in the draw blokc model
    let p1,q1,p2,q2= getAbsXY p1, getAbsXY q1, getAbsXY p2, getAbsXY q2
    // Find the four orientations needed for general and 
    // special cases 
    let o1 = orientation (p1) (q1) (p2)
    let o2 = orientation (p1) (q1) (q2)
    let o3 = orientation (p2) (q2) (p1)
    let o4 = orientation (p2) (q2) (q1)
  
    // General case 
    if (o1 <> o2 && o3 <> o4)
        then true

    // Special Cases 
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1 
    elif (o1 = 0 && onSegment (p1) (p2) (q1))
        then true
  
    // p1, q1 and q2 are colinear and q2 lies on segment p1q1 
    elif (o2 = 0 && onSegment (p1) (q2) (q1))
        then true
  
    // p2, q2 and p1 are colinear and p1 lies on segment p2q2 
    elif (o3 = 0 && onSegment (p2) (p1) (q2))
        then true
  
     // p2, q2 and q1 are colinear and q1 lies on segment p2q2 
    elif (o4 = 0 && onSegment (p2) (q1) (q2))
        then true
    else false



///Returns a segment with positive Start and End coordinates
let makeSegPos (seg : Segment) =
    {seg with
        Start = getAbsXY seg.Start
        End = getAbsXY seg.End }

/// Given two coordinates, this function returns the euclidean
/// distance between them.
let distanceBetweenTwoPoints (pos1 : XYPos) (pos2 : XYPos) : float =
    sqrt ( (pos1.X - pos2.X)*(pos1.X - pos2.X) + (pos1.Y - pos2.Y)*(pos1.Y - pos2.Y) )


/// Given the coordinates of two port locations that correspond
/// to the endpoints of a wire, this function returns a list of
/// Segment(s).
let makeInitialSegmentsList (hostId : ConnectionId) (portCoords : XYPos * XYPos) : list<Segment> =
    let xyPairs, isLeftToRight = makeInitialWireVerticesList portCoords
    xyPairs
    |> xyVerticesToSegments hostId isLeftToRight


/// This function renders the given
/// segment (i.e. creates a ReactElement
/// using the data stored inside it),
/// using the colour and width properties given.
let renderSegment (segment : Segment) (colour : string) (width : string) : ReactElement = 
    let wOpt = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth = 
        match wOpt with
        | Some 1 -> 1.5
        | Some n when n < int "8" -> 2.5
        | _ -> 3.5
    let halfWidth = (renderWidth/2.0) - (0.75)
    let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }

    if segment.Dir = Horizontal then
        let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

        let renderWireSubSegment (vertex1 : XYPos) (vertex2 : XYPos) : list<ReactElement> =
            let Xa, Ya, Xb, Yb = vertex1.X, vertex1.Y, vertex2.X, vertex2.Y
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]
        
        let segmentJumpHorizontalSize = 9.0
        let segmentJumpVerticalSize = 6.0
        
        let renderSingleSegmentJump (intersectionCoordinate : XYPos) : list<ReactElement> =
            let x, y = intersectionCoordinate.X, intersectionCoordinate.Y

            let startingPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y}
            let startingControlPoint = {X = x - segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
            let endingControlPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y - segmentJumpVerticalSize}
            let endingPoint = {X = x + segmentJumpHorizontalSize/2.0; Y = y}

            makePath startingPoint startingControlPoint endingControlPoint endingPoint pathParameters
            ::
            makeCircle startingPoint.X startingPoint.Y circleParameters
            ::
            [
                makeCircle endingPoint.X endingPoint.Y circleParameters
            ]
        
        let rec renderMultipleSegmentJumps (segmentJumpCoordinateList : list<float>) (segmentJumpYCoordinate : float) : list<ReactElement> =
            
            match segmentJumpCoordinateList with

            | [] -> []


            | [singleElement] ->
                renderSingleSegmentJump {X = singleElement; Y = segmentJumpYCoordinate}


            | firstElement :: secondElement :: tailList ->

                if (segment.Start.X > segment.End.X) then
                    renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                    @
                    renderWireSubSegment {X = firstElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                    @
                    renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
                
                else
                    renderSingleSegmentJump {X = firstElement; Y = segmentJumpYCoordinate}
                    @
                    renderWireSubSegment {X = firstElement + segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate} {X = secondElement - segmentJumpHorizontalSize/2.0; Y = segmentJumpYCoordinate}
                    @
                    renderMultipleSegmentJumps (secondElement :: tailList) (segmentJumpYCoordinate)
            

        let completeWireSegmentRenderFunction (seg : Segment) : list<ReactElement> =
            
            let jumpCoordinateList =
                if (segment.Start.X > segment.End.X) then
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sortDescending
                    
                else
                    seg.JumpCoordinateList
                    |> List.map fst
                    |> List.sort
            
            match jumpCoordinateList with
                | [] -> renderWireSubSegment seg.Start seg.End

                | lst ->
                     let y = seg.Start.Y // SHOULD be equal to seg.End.Y since ONLY horizontal segments have jumps
                     let firstSegmentJumpCoordinate = lst[0]
                     let lastSegmentJumpCoordinate = lst[(List.length lst) - 1]

                     if (segment.Start.X > segment.End.X) then
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y} seg.End

                     else
                         renderWireSubSegment seg.Start {X = firstSegmentJumpCoordinate - segmentJumpHorizontalSize/2.0; Y = y}
                         @
                         renderMultipleSegmentJumps lst y
                         @
                         renderWireSubSegment {X = lastSegmentJumpCoordinate + segmentJumpHorizontalSize/2.0; Y = y} seg.End
        

        let wireSegmentReactElementList = segment
                                          |> completeWireSegmentRenderFunction

        g [] wireSegmentReactElementList
    
    else
        let Xa, Ya, Xb, Yb = segment.Start.X, segment.Start.Y, segment.End.X, segment.End.Y
        let segmentElements = 
            makeLine Xa Ya Xb Yb lineParameters
            ::
            makeCircle Xa Ya circleParameters
            ::
            [
                makeCircle Xb Yb circleParameters
            ]
        g [] segmentElements

///
type WireRenderProps =
    {
        key: string
        Segments: list<Segment>
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortLocation: XYPos
    }


// ------------------------------redundant wire memoisation code------------------------------
// this code is not used because React (via Function.Of) does this caching anyway - better tha it can be
// done here
let mutable cache:Map<string,WireRenderProps*ReactElement> = Map.empty

/// not used
let memoOf (f: WireRenderProps -> ReactElement, _, _) =
    (fun props ->
        match Map.tryFind props.key cache with
        | None -> 
            let re = f props
            cache <- Map.add props.key (props,re) cache 
            re
        | Some (props',re) ->  
            if props' = props then re else
                let re = f props
                cache <- Map.add props.key (props,re) cache
                re)
//-------------------------------------------------------------------------------------------

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let renderWireSegmentList : list<ReactElement> =
                props.Segments
                |> List.map
                    (
                        fun (segment : Segment) -> renderSegment segment (props.ColorP.Text()) (string props.StrokeWidthP)
                            //call a bunch of render helper functions to render the segment (*** DO NOT FORGET SEGMENT JUMPS ***)
                    )
            
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
                let textString = if props.StrokeWidthP = 1 then "" else string props.StrokeWidthP //Only print width > 1
                makeText (props.OutputPortLocation.X+1.0) (props.OutputPortLocation.Y-7.0) (textString) textParameters
            g [] ([ renderWireWidthText ] @ renderWireSegmentList)
        
    , "Wire"
    , equalsButFunctions
    )

///
let MapToSortedList map : Wire list = 
    let listSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Purple) map
        |> Map.toList
        |> List.map snd
    let listErrorSelected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Brown) map
        |> Map.toList
        |> List.map snd
    let listErrorUnselected =
        Map.filter (fun id wire -> wire.Color = HighLightColor.Red) map
        |> Map.toList
        |> List.map snd
    let listUnSelected = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.DarkSlateGrey) map
        |> Map.toList
        |> List.map snd
    let listCopied = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Thistle) map
        |> Map.toList
        |> List.map snd
    let listWaves = 
        Map.filter (fun id wire -> wire.Color = HighLightColor.Blue) map
        |> Map.toList
        |> List.map snd

    listUnSelected @ listErrorUnselected @ listErrorSelected @ listSelected @ listWaves @ listCopied
   
let view (model : Model) (dispatch : Dispatch<Msg>) =
    let start = TimeHelpers.getTimeMs()
    let wires1 =
        model.WX
        |> Map.toArray
        |> Array.map snd
    TimeHelpers.instrumentTime "WirePropsSort" start
    let rStart = TimeHelpers.getTimeMs()
    let wires =
        wires1
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
                            Segments = List.map makeSegPos wire.Segments
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = outputPortLocation
                        }
                    singleWireView props)
    TimeHelpers.instrumentInterval "WirePrepareProps" rStart ()
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start


// -------------------------------------------------------------- Inigo Selwood


/// Gets the intersection point between two segments, or None if the segments
/// don't intersect
let segmentIntersectsSegmentCoordinates
        ((start1, end1): XYPos * XYPos)
        ((start2, end2): XYPos * XYPos): Option<XYPos> =

    if segmentIntersectsSegment (start1, end1) (start2, end2) then
        let start1X, start1Y = abs start1.X, abs start1.Y
        let start2X, start2Y = abs start2.X, abs start2.Y

        let end1X, end1Y = abs end1.X, abs end1.Y
        let end2X, end2Y = abs end2.X, abs end2.Y

        let alpha =
            ((end2X - start2X) * (start1Y - start2Y) -
            (end2Y - start2Y) * (start1X - start2X)) /
            ((end2Y - start2Y) * (end1X - start1X) -
            (end2X - start2X) * (end1Y - start1Y))

        // Evaluate the intersection point
        let xIntersection = start1X + (alpha * (end1X - start1X))
        let yIntersection = start1Y + (alpha * (end1Y - start1Y))

        Some {
            X = xIntersection
            Y = yIntersection
        }

    else
        None


/// Given a bounding box, find the top-left and bottom-right corners.
/// Note: finding the (min, max) vertex values seems a little redundant, but
/// maybe the (width, height) values can be negative? Either way, I haven't
/// changed it
let getTopLeftAndBottomRightCorner (box : BoundingBox) : XYPos * XYPos =

    // The vertices -- each of the box's coordinates
    let vertices =
        [
            (box.X, box.Y);
            (box.X, box.Y + box.H);
            (box.X + box.W, box.Y);
            (box.X + box.W, box.Y + box.H)
        ]

    // Convert the top left and bottom right corners to points
    let tupleToXYPoint (tuple: float * float): XYPos =
        {
            X = fst tuple
            Y = snd tuple
        }
    let topLeft = tupleToXYPoint <| List.min vertices
    let bottomRight = tupleToXYPoint <| List.max vertices

    (topLeft, bottomRight)


/// Given a segment and a bounding box, evaluates:
/// - (false, None) if there's no intersection
/// - (true, None) if it's fully inside the box
/// - (true, Some point) if there's an intersection
let segmentIntersectsBoundingBoxCoordinates
        (testSegment: Segment)
        (boundingBox: BoundingBox): bool * Option<XYPos> =

    // Get top-left, bottom-right corners, and evaluate bottom-left and
    // top-right ones
    let topLeft, bottomRight = getTopLeftAndBottomRightCorner boundingBox
    let bottomLeft = {X = topLeft.X; Y = bottomRight.Y}
    let topRight = {X = bottomRight.X; Y = topLeft.Y}

    // Checks whether either of the segment's vertices (start or end) are
    // within the bounding box
    let segment = makeSegPos testSegment
    let segmentVertexInBox =
        let pointInBox (point: XYPos): bool =
            point.X > topLeft.X
                && point.X < bottomRight.X
                && point.Y > topLeft.Y
                && point.Y < bottomRight.Y

        (pointInBox segment.Start) || (pointInBox segment.End)

    // Gets a list of points of intersection between a segment and the four
    // sides of the bounding box
    let intersectionList : list<XYPos> =
        let sideVertices =
            [
                (topLeft, bottomLeft)     // Left
                (topRight, bottomRight)   // Right
                (topLeft, topRight)       // Top
                (bottomLeft, bottomRight) // Bottom
            ]

        // True if the segment intersects the line defined by the side's two
        // points
        let intersectsSegment (side: XYPos * XYPos) =
            segmentIntersectsSegmentCoordinates
            <| (segment.Start, segment.End)
            <| side

        // For each side, check if there's an intersection
        sideVertices
        |> List.map intersectsSegment
        |> List.choose id

    if intersectionList.Length = 0 then
        segmentVertexInBox, None
    else
        true, Some (List.head intersectionList)


/// Given a point and a segment, calculate the distance between the two
let distanceFromPointToSegment (point: XYPos) (segment: Segment): float =

    // Get the (X, Y) coordinates of the segment's start and end points
    let pointToTuple (point': XYPos): float * float =
        point'.X, point'.Y
    let start1X, start1Y = pointToTuple segment.Start
    let end1X, end1Y = pointToTuple segment.End

    //  If the line is vertical, get the horizontal distance to the point
    let pointX, pointY = point.X, abs point.Y
    if start1X = end1X then
        abs (start1X - pointX)

    // Similarly, if the line is horizontal, get the vertical distance
    elif start1Y = end1Y then
        abs (start1Y - pointY)

    // Otherwise, do some funky arithmetic.
    else
        let alpha =
            (end1X - start1X) * (start1Y - pointY) -
            (start1X - pointX) * (end1Y - start1Y)
        let beta =
            (end1X - start1X) * (end1X - start1X) +
            (end1Y - start1Y) * (end1Y - start1Y)

        (abs alpha) / (sqrt beta)


/// Given the model's state and a list of wire IDs to remap,
/// reroute those wires with default shapes between
let routeGivenWiresBasedOnPortPositions
        (wiresToBeRouted: list<ConnectionId>)
        (model: Model): Model =

    // Evaluate a new WX mapping, rerouting the given wires
    let newWX =
        let updatedWireMap =

            // Evaluates a new list of segments for a wire
            let mapWireSegments (wire: Wire) =
                let segments =
                    let positions =
                        Symbol.getTwoPortLocations
                        <| model.Symbol
                        <| wire.InputPort
                        <| wire.OutputPort
                    makeInitialSegmentsList wire.Id positions

                let map = {wire with Segments = segments}
                (wire.Id, map)

            // Apply the segment remapping to each of the wires specified
            wiresToBeRouted
            |> List.map (fun id -> model.WX[id])
            |> List.map mapWireSegments
            |> Map.ofList

        // Create a new WX map, using values from the updated wire map (if
        // available)
        let selectNewWires id wire =
            if not (Map.containsKey id updatedWireMap) then wire
            else updatedWireMap[id]

        Map.map selectNewWires model.WX

    {model with WX = newWX}


/// For a given connection ID and bounding box: find the segments of the wire
/// that intersect the boundary (in a given model)
let getIntersectingSegments
        (model: Model)
        (wireId: ConnectionId)
        (selectBox: BoundingBox): list<Segment> =

    // Filter, returning true if the segment and bounding box intersect
    let segmentFilter (segment: Segment) =
        fst (segmentIntersectsBoundingBoxCoordinates segment selectBox)

    // Filter for only the segments which intersect
    List.filter segmentFilter model.WX[wireId].Segments


/// Finds the closest segment in a wire to a point (using euclidean distance)
let getClosestSegment
        (model: Model)
        (wireId: ConnectionId)
        (pos: XYPos): Segment =

    let distanceToPoint (segment: Segment) =
        distanceFromPointToSegment pos segment
    List.minBy distanceToPoint (model.WX[wireId].Segments)


/// Gets the ID of the wire clicked
/// Note: presumes we already know a wire has been clicked on, and just need
/// its ID
let getClickedSegment
        (model: Model)
        (wireID: ConnectionId)
        (position: XYPos) : SegmentId =

    // Find which segments intersect with an arbitrary bounding box around the
    // mouse
    let boundingBox =
        {
            X = position.X - 5.0
            Y = position.Y - 5.0
            H = 10.0
            W = 10.0
        }
    let intersectingSegments = getIntersectingSegments model wireID boundingBox

    // At low resolutions there might be no hits; in which case, we just use
    // the next closest segment in the range of the wire bounding box.
    match List.isEmpty intersectingSegments with
        | true ->  (getClosestSegment model wireID position).Id
        | false -> (List.head intersectingSegments).Id


/// Verifies that the segment is aligned with the axis it's meant to lie on
let checkSegmentAngle (segment: Segment) (name: string): unit =

    let isAligned =
        let distance (valueOne: float) (valueTwo: float): float =
            abs (abs valueOne - abs valueTwo)

        // To do: Consider replacing the magic number here with
        // System.Double.Epsilon?
        match segment.Dir with
            | Vertical   -> (distance segment.Start.X segment.End.X) < 0.000001
            | Horizontal -> (distance segment.Start.Y segment.End.Y) < 0.000001

    // Say something if the check fails
    if not isAligned then
        printfn $"Weird segment '{name}':\n{segment}\n\n fails angle checking"


/// Checks whether a segment points to the left
let segPointsLeft (segment: Segment): bool =
    (abs segment.Start.X > abs segment.End.X) && (segment.Dir = Horizontal)


/// Checks the segment's length along the X axis
let segXDelta (segment: Segment): float =
    (abs segment.End.X) - (abs segment.Start.X)


/// Given two segments which are joined at a given position, change the X
/// coordinate of that coordinate (compensating for negative values)
let moveXJoinPos
    (newXValue: float)
    (segment1: Segment)
    (segment2: Segment): list<Segment> =

    let changeXKeepingSign (coord: XYPos) =
        if coord.X < 0.0 then
            {coord with X = -newXValue}
        else
            {coord with X = newXValue}

    [
        {segment1 with End = changeXKeepingSign segment1.End}
        {segment2 with Start = changeXKeepingSign segment2.Start}
    ]


/// Picks the outermost segment (usually segment 1, unless isAtEnd [of wire])
/// and ensures it's at least a Wire.stickLength long by moving the point where
/// it meets the other (innermost) segment
let changeLengths
        (isAtEnd: bool)
        (segment1: Segment)
        (segment2: Segment): list<Segment> =

    // Evaluate which segment is outermost, presuming the first segment unless
    // at the end of a wire
    let outerSegment, innerSegment =
        match isAtEnd with
            | true -> segment2, segment1
            | false -> segment1, segment2

    // Get the lengths of both the inner and outer segments along the X axis
    let innerX = segXDelta innerSegment
    let outerX = segXDelta outerSegment

    // This case shouldn't occur
    if segment1.Dir <> Horizontal
            || segment2.Dir <> Horizontal
            || outerX < 0.0 then
        [segment1; segment2]

    // If the inner segment is alreay of length zero, we need to shorten the
    // first or last segment. We do that by moving the join point rather
    elif innerX < 0.0 then

        // Evaluate a new join point which is moved by a single stick length,
        // and apply it to the segments
        let newJoinPoint =
            if isAtEnd then
                segment2.End.X - Wire.stickLength
            else
                segment1.Start.X + Wire.stickLength
        moveXJoinPos newJoinPoint segment1 segment2

    // Otherwise, don't do anything
    else
        [segment1; segment2]


/// Called for segments [1:5] -- if they're vertical and can move horizontally.
/// Returns the distance value once it's been moderated to prevent wires
/// moving into components.
let getSafeDistanceForMove
        (testSegment: Segment)
        (firstSegment: Segment)
        (lastSegment: Segment)
        (distance: float) =

    // Stick length can be shrunk for segments which aren't at the end of their
    // wires -- so we find a value for that shrink factor here
    let shrink =
        let atEnd = testSegment.Index < 1 || testSegment.Index > 5
        if atEnd then 1.0
        else 0.5

    // With that shrink factor, we find the minimum and maximum distance from
    // the segment to the wire's start/end
    let minimumDistance =
        firstSegment.Start.X +
        Wire.stickLength * shrink -
        abs testSegment.End.X
    let maximumDistance =
        lastSegment.End.X -
        Wire.stickLength * shrink -
        abs testSegment.Start.X

    // These helpers make the match case a little less verbose
    let positive = distance > 0.0
    let negative = distance < 0.0

    // Check whether a given end of the test segment is vertically close to the
    // first/last segment in the wire
    let yJoined (segment: Segment) (point: XYPos): bool =
        abs (abs segment.Start.Y - abs point.Y) < 0.0001

    // I haven't spent the time to understand this match case, but it works.
    // I assume finds how much a wire section _can_ move given its position in
    // that wire.
    match testSegment.Index with
        | _ when testSegment.Dir = Horizontal -> distance
        | 3 when negative && yJoined firstSegment testSegment.Start -> distance
        | 3 when positive && yJoined lastSegment testSegment.End -> distance
        | 1
        | 2 -> max minimumDistance distance
        | 4
        | 5 -> min maximumDistance distance
        | 3 -> distance |> max minimumDistance |> min maximumDistance
        | _ -> distance


/// Remove pairs of adjacent segments which are aligned but not of the same
/// sign
let removeRedundantSegments (segments: Segment list) =

    // Reduces a sequential pair of segments
    let reduce (segment1: Segment) (segment2: Segment): list<Segment> =

        let direction (segment: Segment): int =
            sign (abs segment.End.X - abs segment.Start.X)

        let length (segment: Segment): float =
            abs segment.End.X - abs segment.Start.X

        // If the segments are aligned but not facing in the same direction,
        // they need to be reduced
        if segment1.Dir = Horizontal
            && segment2.Dir = Horizontal
            && direction segment1 <> direction segment2 then

            // Sets the absolute X value of a position
            let setAbsoluteX (position: XYPos) (x: float) =
                position.X
                |> (fun xPosition -> if xPosition > 0.0 then 1.0 else -1.0)
                |> (fun sign -> {position with X = sign * (abs x)})

            // Set the start, end values of a segment
            let setEnd (segment: Segment) (value: float): Segment =
                {segment with End = setAbsoluteX segment.Start value}
            let setStart (segment: Segment) (value: float): Segment =
                {segment with Start = setAbsoluteX segment.Start value}

            // Depending on which direction the segments are misaligned, move
            // the (start, end) points
            if length segment1 > length segment2 then
                [
                    setEnd segment1 segment2.End.X
                    setStart segment2 segment2.End.X
                ]
            else
                [
                    setEnd segment1 segment1.Start.X
                    setStart segment2 segment1.End.X
                ]

        // Otherwise, the segments are fine as they are
        else
            [segment1; segment2]

    // Reduce the first and last pair of segments in a wire
    reduce segments[0] segments[1]
        @ segments[2..4]
        @ reduce segments[5] segments[6]


/// Moves a wire segment a given amount perpendicular to its orientation.
/// Used to manually adjust routing by dragging with the mouse.
let moveSegment (segment:Segment) (distance:float) (model:Model) =
    let wire = model.WX[segment.HostId]
    let index = segment.Index

    // Check the segment's index is in-range
    if index <= 0 || index >= wire.Segments.Length - 1 then
        failwithf $"Buswire segment index {index} out of range in moveSegment "
                "in wire length {wire.Segments.Length}"

    let lastSegment = wire.Segments[index - 1]
    let nextSegment = wire.Segments[index + 1]

    // Don't do anything if the segment being dragged is aligned with both the
    // last and the next one
    if segment.Dir = lastSegment.Dir || segment.Dir = nextSegment.Dir then
        wire

    // Otherwise, we need to work out how far the segment can be safely dragged
    // without colliding with components
    else

        // Work out a move distance, that's the maximum allowed
        let moveDistance =
            let startSegment, endSegment = wire.Segments[0], wire.Segments[6]
            getSafeDistanceForMove segment startSegment endSegment distance

        // Get new values for the current segment, the end of the last one, and
        // the start of the next one
        let newLastEnd, newCurrentStart, newCurrentEnd, newNextStart =

            // Sets the X/Y value of a point along a given axis
            let setValue (point: XYPos) (value: float) (axis: Orientation) =
                let newValue = - (abs value + moveDistance)
                match axis with
                    | Horizontal -> {point with X = newValue}
                    | Vertical -> {point with Y = newValue}

            match segment.Dir with
                | Vertical ->
                    setValue lastSegment.End segment.Start.X Horizontal,
                    setValue segment.Start segment.Start.X Horizontal,
                    setValue segment.End segment.End.X Horizontal,
                    setValue nextSegment.Start segment.End.X Horizontal

                | Horizontal ->
                    setValue lastSegment.End segment.Start.Y Vertical,
                    setValue segment.Start segment.Start.Y Vertical,
                    setValue segment.End segment.End.Y Vertical,
                    setValue nextSegment.Start segment.End.Y Vertical

        // Create new segments with those changed start/end values
        let newLastSegment = {lastSegment with End = newLastEnd}
        let newNextSeg = {nextSegment with Start = newNextStart}
        let newCurrentSegment =
            {
                segment with
                    Start = newCurrentStart
                    End = newCurrentEnd
            }

        // Insert the moved segments into the wire, and remove redundant
        // overlaps
        let newSegments =
            wire.Segments[..(index - 2)]
                @ [newLastSegment; newCurrentSegment; newNextSeg]
                @ wire.Segments[(index + 2)..]
            |> removeRedundantSegments

        // Create a new wire with the moved and reduced segments
        {wire with Segments = newSegments}


/// Initialisatiton with no wires
let init () =
    let symbols, _ = Symbol.init()

    let model =
        {
            WX = Map.empty
            FromVerticalToHorizontalSegmentIntersections = Map.empty
            FromHorizontalToVerticalSegmentIntersections = Map.empty
            Symbol = symbols
            CopiedWX = Map.empty
            SelectedSegment = SegmentId("")
            LastMousePos = {X = 0.0; Y = 0.0}
            ErrorWires = []
            Notifications = None
        }

    (model, Cmd.none)


/// Returns the wires connected to a list of components
let getConnectedWires (model : Model) (componentIDs : list<ComponentId>) =

    let isConnected (wire: Wire) =
        let inputs, outputs = Symbol.getPortLocations model.Symbol componentIDs
        Map.containsKey wire.InputPort inputs
                || Map.containsKey wire.OutputPort outputs

    model.WX
    |> Map.toList
    |> List.map snd
    |> List.filter isConnected
    |> List.map (fun wire -> wire.Id)
    |> List.distinct


/// Returns 3 tuples:
/// - wires connected only to inputs
/// - those connected only to outputs
/// - wires with both inputs and outputs connected
let filterWiresByCompMoved (model: Model) (componentIDs: list<ComponentId>) =
    let inputs, outputs = Symbol.getPortLocations model.Symbol componentIDs

    // List of all wires
    let wires =
        model.WX
        |> Map.toList
        |> List.map snd

    // Filter those with input connections
    let inputWires =
        wires
        |> List.filter (fun wire -> Map.containsKey wire.InputPort inputs)
        |> List.map (fun wire -> wire.Id)
        |> List.distinct

    // Filter for output connections
    let outputWires =
        wires
        |> List.filter (fun wire -> Map.containsKey wire.OutputPort outputs)
        |> List.map (fun wire -> wire.Id)
        |> List.distinct

    // And filter for wires with both inputs and outputs connected
    let fullyConnected =
        let filter (wire: Wire): bool =
            Map.containsKey wire.InputPort inputs
                    && Map.containsKey wire.OutputPort outputs

        wires
        |> List.filter filter
        |> List.map (fun wire -> wire.Id)
        |> List.distinct

    (inputWires, outputWires, fullyConnected)


/// Returns a newly autorouted wire given a model and wire
let autorouteWire (model: Model) (wire: Wire): Wire =

    // Get the wire's port locations
    let locations =
        let inputs, outputs = wire.InputPort, wire.OutputPort
        Symbol.getTwoPortLocations model.Symbol inputs outputs

    // Autoroute a segment between the wire's ports, and assign it to the wire
    {
        wire with
            Segments = makeInitialSegmentsList wire.Id locations
    }


/// Reverse the segment order, as well as (start, end) coordinates
let revSegments (segments: Segment list) =
    let invert (segment: Segment): Segment =
        {
            segment with
                Start = segment.End
                End = segment.Start
        }

    List.rev segments
    |> List.map invert


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


/// Create a position, the sum of two other positions
let inline addPosPos (position1: XYPos) (position2: XYPos): XYPos =
    {
        X = position1.X + position2.X
        Y = position1.Y + position2.Y
    }


/// Applies a mover function to the end of a segment at a given index
let inline moveEnd (mover: XYPos -> XYPos) (setIndex: int) =

    let setEndAtIndex (segmentIndex: int) (segment: Segment) =
        if segmentIndex = setIndex then
            {
                segment with
                    End = mover segment.End
            }
        else
            segment

    List.mapi setEndAtIndex


/// Applies a mover function to the start of a segment at a given index
let inline moveStart (mover: XYPos -> XYPos) (setIndex: int) =

    let setStartAtIndex (segmentIndex: int) (segment: Segment) =
        if segmentIndex = setIndex then
            {
                segment with
                    Start = mover segment.Start
            }
        else
            segment

    List.mapi setStartAtIndex


/// Applies a mover function to the (start, end) of a segment at a given index
let inline moveAll (mover: XYPos -> XYPos) (setIndex: int) =

    let setAllAtIndex (segmentIndex: int) (segment: Segment) =
        if segmentIndex = setIndex then
            {
                segment with
                    Start = mover segment.Start
                    End = mover segment.End
            }
        else
            segment

    List.mapi setAllAtIndex


/// Applies (X, Y) transformations to a point
let transformXY
        (xTransform: float -> float)
        (yTransform: float -> float)
        (point: XYPos): XYPos =

    {
        point with
            X = xTransform point.X
            Y = yTransform point.Y
    }


/// Applies (X, Y) transformations to the start and end of a segment
let transformSeg
        (xTransform: float -> float)
        (yTransform: float -> float)
        (segment: Segment): Segment =

    let transform = transformXY xTransform yTransform

    {
        segment with
            Start = transform segment.Start
            End = transform segment.End
    }


/// Gets a tuple, the pair of directions in each axis
let topology (position1: XYPos) (position2: XYPos): int * int =
    let delta (point0: float) (point1: float): int =
        sign (abs point0 - abs point1)
    (delta position1.X position2.X), (delta position1.Y position2.Y)


/// Performs a partial autoroute. Will fail (returning None) if a full
/// autoroute is needed -- or if there are manually dragged segments in the
/// way.
let partialAutoRoute
        (segments: Segment list)
        (newPortPosition: XYPos): option<list<Segment>> =

    let wirePosition = segments[0].End
    let portPosition = segments[0].Start

    let newWirePosition =
        {
            newPortPosition with
                X = newPortPosition.X + (abs wirePosition.X - portPosition.X)
        }
    let delta =
        {
            X = newPortPosition.X - portPosition.X
            Y = newPortPosition.Y - portPosition.Y
        }

    // Get the index of the last autorouted segment in the list
    let lastAutoroutedIndex =

        let isNegative (position: XYPos): bool =
            position.X < 0.0 || position.Y < 0.0

        let segmentAutorouted (segment: Segment): bool =
            not (isNegative segment.Start || isNegative segment.End)

        segments
        |> List.takeWhile segmentAutorouted
        |> List.length
        |> (fun index -> if index > 5 then None else Some (index + 1))

    let preEndScale (segmentIndex: int): option<list<Segment>> =

        let segment = segments[segmentIndex]
        let fixedPoint = getAbsXY segment.End

        let startPosition =
            if segmentIndex = 1 then portPosition
            else wirePosition
        let newStartPosition =
            if segmentIndex = 1 then newPortPosition
            else newWirePosition

        // I do not understand what this scale function does. Something vital,
        // but incomprehensible.
        let scale
                (value: float)
                (fixedPoint: float)
                (startPoint: float)
                (endPoint: float): float =

            if startPoint = fixedPoint then
                value
            else
                ((abs value - fixedPoint) * (startPoint - fixedPoint) /
                (abs endPoint - fixedPoint) + fixedPoint) * float (sign value)

        let scaleX (value: float): float =
            scale value fixedPoint.X newStartPosition.X startPosition.X
        let scaleY (y: float): float =
            scale y fixedPoint.Y newStartPosition.Y startPosition.Y

        // Partition the list into two portions about the split index. If the
        // split includes unscaled segments -- or discard scaled ones --
        // the route won't work and we return None.
        let splitList = List.splitAt (segmentIndex + 1) segments
        match splitList, segmentIndex with
            | (scaledSegments, otherSegments), 1 ->
                Some (
                    (List.map (transformSeg scaleX scaleY) scaledSegments)
                    @ otherSegments
                )
            | (firstSegment :: scaledSegments, otherSegments), _ ->
                Some (
                    (moveAll (addPosPos delta) 0 [firstSegment]
                    @ List.map (transformSeg scaleX scaleY) scaledSegments)
                    @ otherSegments
                )
            | _ -> None

    // Gets the topology of a segment with a given index -- if possible.
    let checkTopology (index: int): option<int> =
        let finalPoint = segments[6].Start

        let oldTopology (position: XYPos): int * int =
            topology
            <| if index = 1 then portPosition else wirePosition
            <| position
        let newTopology (position: XYPos): int * int =
            topology
            <| if index = 1 then newPortPosition else newWirePosition
            <| position

        if oldTopology finalPoint <> newTopology finalPoint then
            None
        else
            let routedSegmentEnd = segments[index].End
            if oldTopology routedSegmentEnd = newTopology routedSegmentEnd then
                Some index
            else
                None

    lastAutoroutedIndex
    |> Option.bind checkTopology
    |> Option.bind preEndScale


/// Returns the new positions keeping manual coordinates negative, and auto
/// coordinates positive
let negXYPos (position: XYPos) (difference: XYPos): XYPos =

    let newPosition = Symbol.posAdd (getAbsXY position) difference
    if position.X < 0.0 || position.Y < 0.0 then
        {
            X = - newPosition.X
            Y = - newPosition.Y
        }
    else
        newPosition


/// Moves a wire by a specified amount by adding a XYPos to each start and end
/// point of each segment
let moveWire (wire : Wire) (difference : XYPos): Wire =

    let transformer (segment: Segment): Segment =
        {
            segment with
                Start = negXYPos segment.Start difference
                End = negXYPos segment.End difference
        }

    {
        wire with
            Segments = List.map transformer wire.Segments
    }


/// Re-routes a wire in the model when its ports have moved. Tries to preserve
/// manual routing when it makes sense to do so -- otherwise, use auto-routing.
let updateWire (model: Model) (wire: Wire) (isInput: bool) =

    // Get the connection port, either the input or the output
    let newPort =
        let symbol = model.Symbol
        match isInput with
            | true -> Symbol.getInputPortLocation symbol wire.InputPort
            | false -> Symbol.getOutputPortLocation symbol wire.OutputPort

    // Partially route from input to end by reversing segments, and swapping
    // the start/end values.
    let newSegments =
        if isInput then
            partialAutoRoute (revSegments wire.Segments) newPort
            |> Option.map revSegments
        else
            partialAutoRoute wire.Segments newPort

    /// Take the new segments and create a wire from them
    newSegments
    |> Option.map (fun segs -> {wire with Segments = segs})
    |> Option.defaultValue (autorouteWire model wire)


// ------------------------------------------------------------ / Inigo Selwood

let makeAllJumps (wiresWithNoJumps: ConnectionId list) (model: Model) =
    let mutable newWX = model.WX
    // Arrays are faster to check than lists
    let wiresWithNoJumpsA = List.toArray wiresWithNoJumps
    let changeJumps wid index jumps =
        let jumps = List.sortDescending jumps
        let changeSegment segs =
            List.mapi (fun i x -> if i <> index then x else { x with JumpCoordinateList = jumps }) segs

        newWX <- Map.add wid { newWX[wid] with Segments = changeSegment newWX[wid].Segments } newWX

    let segs =
        model.WX
        |> Map.toArray
        |> Array.mapi (fun i (wid, w) -> List.toArray w.Segments)

    for w1 in 0 .. segs.Length - 1 do
        for h in segs[w1] do
            if h.Dir = Horizontal then
                // work out what jumps this segment should have
                let mutable jumps: (float * SegmentId) list = []
                
                if not (Array.contains h.HostId wiresWithNoJumpsA) then
                    for w2 in 0 .. segs.Length - 1 do
                        // everything inside the inner loop should be very highly optimised
                        // it is executed n^2 time where n is the number of segments (maybe 5000)
                        // the abs here are because segment coordinates my be negated to indicate manual routing
                        for v in segs[w2] do
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
                match jumps, h.JumpCoordinateList with
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


let updateWireSegmentJumps (wireList: list<ConnectionId>) (wModel: Model) : Model =
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
        let wireWidthFromSymbol = WireWidth.Configured 1
        let wireId = ConnectionId(JSHelpers.uuid())
        let segmentList = makeInitialSegmentsList wireId (portOnePos, portTwoPos)
        
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Segments = segmentList
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
                    | SplitWire n ->
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
             |> Map.filter (fun id wire -> not (List.contains id connectionIds))
        {newModel with WX = newWX}, Cmd.ofMsg BusWidths

    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with
        | Down ->
            let segId = getClickedSegment model connId mMsg.Pos
            {model with SelectedSegment = segId }, Cmd.none
        | Drag ->
            let segId = model.SelectedSegment
            let rec getSeg (segList: list<Segment>) = 
                match segList with
                | h::t -> if h.Id = segId then h else getSeg t
                | _ -> failwithf "segment Id not found in segment list"
            let seg = getSeg model.WX[connId].Segments
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
                            let segments = issieVerticesToSegments connId conn.Vertices
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
                            { Id = ConnectionId conn.Id
                              InputPort = inputId
                              OutputPort = outputId
                              Color = HighLightColor.DarkSlateGrey
                              Width = 1
                              Segments = segments}
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
    let boolList = List.map (fun seg -> fst(segmentIntersectsBoundingBoxCoordinates seg bb)) w.Segments
    List.contains true boolList

///
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : list<ConnectionId> = 
    wModel.WX
    |> Map.map (fun id wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun id boolVal -> boolVal)
    |> Map.toList
    |> List.map (fun (id,bool) -> id)

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
                let segmentList = makeInitialSegmentsList newId (portOnePos, portTwoPos)
                [
                    {
                        oldWire with
                            Id = newId;
                            InputPort = InputPortId newInputPort;
                            OutputPort = OutputPortId newOutputPort;
                            Segments = segmentList;
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
