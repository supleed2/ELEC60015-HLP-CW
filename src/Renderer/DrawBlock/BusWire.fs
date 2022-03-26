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

type Orientation =  Horizontal | Vertical

/// In Wire.Rotation: PosY: 0, PosX: 90, NegY: 180, NegX: 270 degree clockwise rotation
type Direction = PosX | PosY | NegX | NegY

type SnapPosition = High | Mid | Low

type routeType = Sameside | Oppositeside | Rightangle

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
        JumpCoordinateListA: (float * SegmentId) list
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
        JumpDistanceListRI: (float * SegmentId) list
        Draggable: bool
        ManualRoute: bool
    }

/// Returns RISeg endpoint given the segment and it's start point
let getRISegEnd (startPos: XYPos) (seg: RISeg) : XYPos =
    match seg.Dir with
    | Horizontal -> {startPos with X = seg.Start.X + seg.Length}
    | Vertical -> {startPos with Y = seg.Start.Y + seg.Length}

let riSegEnd (seg: RISeg) : XYPos =
    getRISegEnd seg.Start seg

/// Converts a jump distance from segment start (RISeg) to an absolute jump coordinate (ASeg)
let jumpDistToJumpCoord (startPos: XYPos, (dist: float, segId: SegmentId)) : float * SegmentId =
    startPos.X + dist, segId

/// Converts an absolute jump coordinate (ASeg) to a jump distance from segment start (RISeg)
let jumpCoordToJumpDist (startPos: XYPos, (coord: float, segId: SegmentId)) : float * SegmentId =
    coord - startPos.X, segId

/// Converts a Rotation Invariant Segment to an Absolute Segment
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

/// Converts an Absolute Segment to a Rotation Invariant Segment 
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
        Rotation: Direction
        /// True = Reflected along y axis , False = Not Reflected
        YReflect: bool
        Segments: ASeg list
    }

    with
        static member stickLength = 16.0
        static member arcSize = 8.0

///
type Model =
    {
        Symbol: Symbol.Model
        WX: Map<ConnectionId, Wire>
        FromVerticalToHorizontalSegmentIntersections: Map<SegmentId, (ConnectionId * SegmentId) list>
        FromHorizontalToVerticalSegmentIntersections: Map<SegmentId, (ConnectionId * SegmentId) list>
        CopiedWX: Map<ConnectionId, Wire>
        SelectedSegment: SegmentId
        LastMousePos: XYPos
        ErrorWires: ConnectionId list
        Notifications: string option
    }

//----------------------------Message Type-----------------------------------//

///
type Msg =
    | Symbol of Symbol.Msg
    | AddWire of (InputPortId * OutputPortId)
    | BusWidths
    | CopyWires of ConnectionId list
    | DeleteWires of ConnectionId list
    | SelectWires of ConnectionId list
    | UpdateWires of ComponentId list * XYPos
    | DragWire of ConnectionId * MouseT
    | ColorWires of ConnectionId list * HighLightColor
    | ErrorWires of ConnectionId list
    | ResetModel // For Issie Integration
    | LoadConnections of Connection list // For Issie Integration

//----------------------------Helper Functions-----------------------------------//

let rotate_rel rotation (pos:XYPos) = 
        match rotation with
        | PosX -> {pos with X = -pos.Y ; Y = pos.X}
        | NegX -> {pos with X = pos.Y ; Y = -pos.X}
        | PosY -> pos
        | NegY -> {pos with X = -pos.X; Y = -pos.Y}
    
let yReflect yreflect (pos:XYPos) =
    if yreflect then
        {pos with X = -pos.X}
    else
        pos

/// Transform a wire with its RISegs list to a list of ASegs
let riSegWireToASegs (wire:Wire) =

    let getStartEnd (seg:RISeg)=
        match seg.Dir with
        | Horizontal -> (seg.Start,{seg.Start with X = seg.Start.X+seg.Length})
        | Vertical -> (seg.Start,{seg.Start with Y = seg.Start.Y+seg.Length}) 

    let rotate (pos:XYPos) = 
        match wire.Rotation with
        | PosX -> {pos with X = pos.Y ; Y = -pos.X}
        | NegX -> {pos with X = -pos.Y ; Y = pos.X}
        | PosY -> pos
        | NegY -> {pos with X = -pos.X; Y = -pos.Y}
    
    let yReflect (pos:XYPos) =
        if wire.YReflect then
            {pos with X = -pos.X}
        else
            pos

    let check_Orientation (pos: XYPos * XYPos)=
        let pos1 = fst pos
        let pos2 = snd pos
        if  abs (pos1.X - pos2.X) <= XYPos.epsilon 
        then Vertical
        else Horizontal
    
    let lstOfPos =
        List.map getStartEnd (List.map aSegToRISeg wire.Segments)
        |> List.map (fun (s,e) -> (rotate s, rotate e))
        |> List.map (fun (s,e) -> (yReflect s, yReflect e))

    let posToASegs (pos: XYPos * XYPos) (riSeg: RISeg) : ASeg =
        {
            Id = riSeg.Id;
            Index = riSeg.Index;
            Start = fst pos;
            End = snd pos;
            Dir = check_Orientation pos;
            HostId = riSeg.HostId;
            JumpCoordinateListA = List.map(fun (len, id) -> (len + (fst pos).X) , id) riSeg.JumpDistanceListRI;
            Draggable = riSeg.Draggable;
            ManualRoute = riSeg.ManualRoute
        }
    
    (lstOfPos,(List.map aSegToRISeg wire.Segments)) ||> List.map2 posToASegs

/// Transform a list of ASegs to RISegs list  (default Rotation: PosY, YReflect: False)
let aSegListToRISegList (aSegList: ASeg list) =
    let toRISeg (seg: ASeg) =
        let direction, length =
            let xChange = seg.End.X - seg.Start.X
            let yChange = seg.End.Y - seg.Start.Y
            match xChange, yChange with
            | x, y when x = 0 -> Vertical, y
            | x, y when y = 0 -> Horizontal, x
            // default orientation for unused segments?
            | x, _ -> Horizontal, x

        {
            Id = seg.Id
            Index = seg.Index
            Length = length
            Start = seg.Start
            Dir = direction
            HostId = seg.HostId
            JumpDistanceListRI = List.map(fun (x, id) -> (x - seg.Start.X) , id) seg.JumpCoordinateListA
            Draggable = seg.Draggable
            ManualRoute = seg.ManualRoute
        }

    aSegList
    |> List.map toRISeg

//-------------------------Debugging functions---------------------------------//

let ppSId (sId:SegmentId) =
    sId
    |> (fun (SegmentId x) -> x)
    |> Seq.toList
    |> (fun chars -> chars[0..2])
    |> List.map string
    |> String.concat ""

let ppS (seg: RISeg) =
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
        |> List.map (fun (_wId,w) ->
            sprintf $"Wire: {w.Segments |> List.collect (fun seg -> seg.JumpCoordinateListA |> List.map (fun (_f, sid) -> ppSId sid))}")
            
    printfn $"\n------------------\nMapHV:\n {m1} \n MapVH\n{m2} \nJumps:\n {jumps}\n------------------\n"

let ppSeg seg (model: Model) = 
        let cid,sid = seg
        let wire = model.WX[cid]
        let sg = List.find (fun (s: ASeg) -> s.Id = sid ) wire.Segments
        let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
        sprintf $"""[{ppSId sg.Id}: {pxy sg.Start}]-{match sg.Dir with | Horizontal -> "Horizontal" | Vertical -> "Vertical" }-{sg.Index}"""

let pp segs (model: Model)= 
    segs
    |> List.map  ( fun seg ->
        let cid,sid = seg
        let wire = model.WX[cid]
        match List.tryFind (fun (s: ASeg) -> s.Id = sid ) wire.Segments with
        | Some  sg ->
            let pxy (xy: XYPos) = sprintf $"{(xy.X,xy.Y)}"
            sprintf $"""[{pxy sg.Start}]-{match sg.Dir with | Horizontal -> "Horizontal" | Vertical -> "Vertical" }-{sg.Index}"""
        | None -> "XX")
    |> String.concat ";"

let ppRISeg riseg=
    printfn $"Index:{riseg.Index},Dir:{riseg.Dir},Length:{riseg.Length},Draggable:{riseg.Draggable}"
    riseg

//-------------------------------Implementation code----------------------------//

/// Convert list of Absolute Segments to a list of vertices
let ASegsToVertices (segList:ASeg list) = 
    let firstCoord = (segList[0].Start.X, segList[0].Start.Y)
    let verticesExceptFirst = List.map (fun seg -> (seg.End.X,seg.End.Y)) segList
    [firstCoord] @ verticesExceptFirst

/// Convert list of Rotation Invariant Segments to a list of vertices // Currently unused
let RISegsToVertices (segList: RISeg list) =
    segList
    |> List.scan getRISegEnd segList[0].Start
    |> List.map (fun pos -> pos.X, pos.Y)

/// Get initial list of wire vertices given port locations corresponding to the
/// enpoints of a wire
let initialWireVerticesFromPorts 
        (startPort: XYPos) 
        (endPort: XYPos) 
        (routetype: routeType): (list<XYPos> * bool)  = 

    let startX, startY, endX, endY = 
        startPort.X, startPort.Y, endPort.X, endPort.Y
    
    printfn $"startX:{startX},endX:{endX},startY:{startY},endY:{endY}"
    // Oppositeside -> the two ports face one another
    if routetype = Oppositeside then
        
        // Adjust length of segments 0 and 6 - the sticks - so that when two 
        // ports are aligned and close, you still get left-to-right routing
        let stickLength =
            if (endX - startX > 0.0) then
                [ 
                    abs (startX - endX)
                    abs (startY - endY)
                    Wire.stickLength / 4.0 
                ]
                |> List.max
                |> min (Wire.stickLength / 2.0)
            else
                Wire.stickLength / 2.0
        
        // Wire travelling left to right (positive X) from output port to input 
        // port
        // S - + - + - + - E
        if endX - startX >= stickLength * 2.0 then  
            [ 
                {X = startX; Y = startY}
                {X = startX + stickLength; Y = startY};
                {X = startX + stickLength; Y = startY} ;
                {X = (startX + endX) / 2.0; Y = startY};
                {X = (startX + endX) / 2.0; Y = endY};
                {X = endX - stickLength; Y = endY}
                {X = endX - stickLength; Y = endY}
                {X = endX; Y = endY}
            ], 
            true // left to right

        // Wire travelling right to left (negative X), but ports are (almost) 
        // aligned vertically An offset is added to the main horizontal segment 
        // so it can be seen / dragged more easily
        // 
        // E - + - +   + - + - S
        //         |   |
        //         + - +
        elif abs (startY - endY) < 4.0 then
            [ 
                {X = startX; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY + Wire.stickLength}
                {X = endX - Wire.stickLength; Y = startY + Wire.stickLength}
                {X = endX - Wire.stickLength; Y = endY}
                {X = endX - Wire.stickLength; Y = endY}
                {X = endX; Y = endY}
            ], 
            false // not left to right
        
        // Wire travelling right to left (negative X), bending back on itself
        // 
        //         + - + - S
        //         |
        // E - + - +
        else
            [ 
                {X = startX; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = (startY + endY) / 2.0}
                {X = endX - Wire.stickLength; Y = (startY + endY) / 2.0}
                {X = endX - Wire.stickLength; Y = endY}
                {X = endX - Wire.stickLength; Y = endY}
                {X = endX; Y = endY}
            ], 
            false // not left to right
    
    // Rightangle -> there's a 90 degree angle between the two ports
    elif routetype = Rightangle then 

        // If the angle is a longer than the stick length, add a kink to the 
        // wire which will space it out more evenly
        // 
        // S - + - +
        //         |
        //         +
        //         |
        //         E
        if (endX - startX >= Wire.stickLength) 
                && (endY - startY >= Wire.stickLength) then
            [ 
                {X = startX; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = endX; Y = startY}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY}
            ], 
            true // Left-to-right
        //fix issue by wrapping wire input port and not through for 1st quadrant  
        else if (((startX + endX) / 2.0)>(startX+Wire.stickLength)) then
            [ 
                {X = startX; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = (startX + endX) / 2.0; Y = startY}
                {X = (startX + endX) / 2.0; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY}
            ], false
        else if (((startY + endY) / 2.0)>(startY+Wire.stickLength)) then
            [ 
                {X = startX; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = ((startY + endY) - Wire.stickLength) / 2.0}
                {X = endX; Y = ((startY + endY) - Wire.stickLength) / 2.0}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY}
            ], 
            false
        else
            [ 
                {X = startX; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = startY}
                {X = startX + Wire.stickLength; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY}
            ], 
            false
    
    // Sameside -> the two ports are facing in the same direction
    elif routetype = Sameside then

        // If the X distance is more than twice the sticklength, and 
        // the output port is *above* the input port
        //
        //     E      E
        //     |      |
        // S   +  or  +   S
        // |   |      |   |
        // + - +      + - +
        if abs (endX - startX) >= Wire.stickLength * 2.0 
                && endY >= startY then
            [ 
                {X = startX; Y = startY} 
                {X = startX ; Y = startY - Wire.stickLength}
                {X = startX ; Y = startY - Wire.stickLength}
                {X = startX ; Y = startY - Wire.stickLength}
                {X = endX; Y = startY - Wire.stickLength}
                {X = endX ; Y = endY - Wire.stickLength}
                {X = endX ; Y = endY - Wire.stickLength}
                {X = endX; Y = endY}
            ], 
            true // Left-to-right
        
        // Otherwise if the start port is above the end port:
        //
        //     S      S
        //     |      |
        // E   +  or  +   E
        // |   |      |   |
        // + - +      + - +
        elif abs (endX - startX) >= Wire.stickLength * 2.0 then
            [ 
                {X = startX; Y = startY}
                {X = startX ; Y = startY - Wire.stickLength}
                {X = startX ; Y = startY - Wire.stickLength}
                {X = startX ; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY}
            ], 
            true // Left-to-right
        
        // And last but not least, if the X distance is less than two stick
        // lengths' worth:
        // 
        // S                      E
        // |                      |
        // + - +   E  or  S   + - +
        //     |   |      |   |  
        //     + - +      + - +
        elif endY < startY then
            [ 
                {X = startX; Y = startY}
                {X = startX; Y = startY - Wire.stickLength}
                {X = startX; Y = startY - Wire.stickLength}
                {X = startX; Y = ((startY + endY) / 2.0) - Wire.stickLength}
                {X = endX + Wire.stickLength * 4.0; Y = ((startY + endY) / 2.0) - Wire.stickLength}
                {X = endX + Wire.stickLength * 4.0; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY}
            ], 
            false // Not left-to-right
        else
            [ 
                {X = startX; Y = startY}
                {X = startX ; Y = startY - Wire.stickLength}
                {X = startX + Wire.stickLength * 4.0; Y = startY - Wire.stickLength}
                {X = startX + Wire.stickLength * 4.0; Y = ((startY + endY) / 2.0) - Wire.stickLength}
                {X = endX; Y = ((startY + endY) / 2.0) - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY - Wire.stickLength}
                {X = endX; Y = endY}
            ], 
            false // Not left-to-right
    
    else
        failwith "route type not recognized"


/// Infer whether wire is LeftToRight from vertices
let inferOrientationFromVertices (xyVerticesList: XYPos list) : bool option =
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
    | Some Horizontal, _, _ when midE.X < midS.X -> Some false
    | Some Vertical, _, _ -> Some true 
    | _, true, true -> Some true
    | _, true, false -> Some false
    | _, false, _ -> None

/// this turns a list of vertices into a list of absolute segments
let convertVerticesToASegs connId (isLeftToRight: bool) routetype rotation (yreflect:bool) (startPos:XYPos) (xyVerticesList: XYPos list) =
    let dirs = 
        match routetype with 
        | Oppositeside->
            match isLeftToRight with
            | true -> // for 3 adjustable segments left-to-right
                [Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical;Horizontal]
            | false -> // for 6 adjustale segments right-to-left
                [Horizontal;Horizontal;Vertical;Horizontal;Vertical;Horizontal;Horizontal]
        | Sameside->
            match isLeftToRight with
            | true -> 
                [Vertical;Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical]
            | false -> 
                [Vertical;Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical]
        | Rightangle->
            match isLeftToRight with
            | true -> 
                [Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical;Vertical]
            | false -> 
                [Horizontal;Vertical;Horizontal;Vertical;Horizontal;Vertical;Vertical]

    let draggable index =
        match routetype with 
            | Oppositeside->
                match index with
                    | 0 | 6 -> false
                    | 1 | 5 -> isLeftToRight 
                    | _ -> true
            | Sameside-> 
                match index with
                    | 0 | 6 -> false
                    | _ -> true
            | Rightangle->
                match index with
                    | 0 | 6 -> false
                    | _ -> true

    let flipdir=
        function | Horizontal -> Vertical
                 | Vertical -> Horizontal

    let dummySeg = 
        {
            Id = SegmentId "dummy";
            Index = -1;
            Start = {X=0; Y=0};
            Dir = Horizontal ;
            Length = 0
            HostId = ConnectionId "dummy";
            JumpDistanceListRI = [];
            Draggable = false;
            ManualRoute = false
        }
    
    let getlength (segOri:Orientation) segLength yreflect =
        let factor = if yreflect then -1.0 else 1.0
        match segOri with
            | Horizontal-> if rotation = NegX || rotation = NegY  then -segLength*factor else segLength*factor;
            | Vertical -> if rotation = PosX || rotation = NegY then -segLength else segLength; 

    let risegs' =  
        List.pairwise xyVerticesList
        |> List.mapi (
            fun i ({X=startX; Y=startY},{X=endX; Y=endY}) ->    
                {
                    Id = SegmentId(JSHelpers.uuid())
                    Index = i
                    Start = {X=startX; Y=startY};
                    End = {X=endX; Y=endY};
                    Dir = dirs[i]
                    HostId  = connId;
                    JumpCoordinateListA = [];
                    Draggable = draggable i
                    ManualRoute = false
                })
        |> List.map aSegToRISeg
        |> List.map (
            fun risegs->
            {
                Id = risegs.Id;
                Index = risegs.Index;
                Start = risegs.Start;
                Dir = if rotation = PosX || rotation = NegX then flipdir risegs.Dir else risegs.Dir ;
                Length = getlength risegs.Dir risegs.Length yreflect
                HostId = risegs.HostId;
                JumpDistanceListRI = risegs.JumpDistanceListRI;
                Draggable = risegs.Draggable;
                ManualRoute = risegs.ManualRoute
            })
    
    ((dummySeg,startPos),risegs')
    ||> List.scan (fun start_resSeg currSeg->
                    let start = snd start_resSeg
                    let next_start = 
                        if currSeg.Dir = Horizontal
                        then {start with X=start.X+currSeg.Length}
                        else {start with Y=start.Y+currSeg.Length}
                    {
                        currSeg with Start = start
                    },next_start)
    |> List.skip 1
    |> List.map (fun resSeg_start-> riSegToASeg (fst resSeg_start))

/// Convert a (possibly legacy) issie Connection stored as a list of vertices to Absolute Segments
let issieVerticesToASegs connId (verticesList: list<float*float>) : ASeg list =
    let XYPosList =
        verticesList |> List.map (fun (x,y) -> {X=x;Y=y})

    let makeNewSegmentsFromPorts (xyList: XYPos list) : ASeg list =
        initialWireVerticesFromPorts xyList[0] xyList[xyList.Length - 1] Oppositeside
        |> (fun (vertices, isLeftToRight) -> convertVerticesToASegs connId isLeftToRight Oppositeside PosY false xyList[0] vertices)

    if XYPosList.Length <> 8 then // wire must have 7 segments and so 8 vertices, if not: reroute from endpoints
        makeNewSegmentsFromPorts XYPosList
    else 
        match inferOrientationFromVertices XYPosList with
        | Some true ->
            printfn "Converting leftToRight"
            convertVerticesToASegs connId true Oppositeside PosY false {X=0;Y=0} XYPosList
        | Some false ->
            printfn "Converting rightToLeft"
            convertVerticesToASegs connId false Oppositeside PosY false {X=0;Y=0}  XYPosList
        | _ -> // can't work out what vertices are, so default to auto-routing
            printfn "Converting unknown"
            makeNewSegmentsFromPorts XYPosList

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
        Vertices = ASegsToVertices wire.Segments // We don't use vertices
    }

/// Converts BusWire.Wire(s) in WX of supplied Model to list of Connections
let extractConnections (wModel : Model) : Connection list = 
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
    let segmentOrientation (segStart: XYPos) (segEnd: XYPos) : Orientation =
        match segStart with
        | _ when segStart.X = segEnd.X -> Vertical
        | _ when segStart.Y = segEnd.Y -> Horizontal
        | _ -> failwithf "Segment must have a common coordinate"
    
    let seg1Ori = segmentOrientation p1 q1
    let seg2Ori = segmentOrientation p2 q2
    
    if seg1Ori = seg2Ori then
        false
    elif seg1Ori = Horizontal then
        ( // seg 1 horizontal and seg 2 vertical
            p1.Y >= min p2.Y q2.Y &&
            p1.Y <= max p2.Y q2.Y &&
            p2.X >= min p1.X q1.X &&
            p2.X <= max p1.X q1.X
        )
    else
        ( // seg 1 vertical and seg 2 horizontal
            p1.X >= min p2.X q2.X &&
            p1.X <= max p2.X q2.X &&
            p2.Y >= min p1.Y q1.Y &&
            p2.Y <= max p1.Y q1.Y
        )

///Returns the absolute segment with positive Start and End coordinates
let makeASegPos (seg : ASeg) =
    {seg with
        Start = absXYPos seg.Start
        End = absXYPos seg.End }

let makeRISegPos (seg: RISeg) =
    { seg with Start = absXYPos seg.Start }

/// Initial list of absolute segments based on positions of ports to be 
/// connected.
/// Does some cool stuff to allow for routing between ports of rotated 
/// components using reflection/rotation of 3 basic patterns
let makeInitialASegList (hostId: ConnectionId) 
        (inputPort: Symbol.PortOrientation * XYPos) 
        (outputPort: Symbol.PortOrientation * XYPos)
        : list<ASeg> =
    
    // Get (input, output) port positions and orientations
    let inputPortPos, outputPortPos = snd inputPort, snd outputPort
    let (inputPortOri: Symbol.PortOrientation), 
            (outputPortOri: Symbol.PortOrientation) = 
        fst inputPort, fst outputPort

    // Lil' cheeky functor to flip positions about the x axis
    let reverse_rotate =
        function
        | PosY -> PosY
        | PosX -> NegX
        | NegY -> NegY
        | NegX -> PosX

    // There's 3 types of route conditions:
    // - SameSide
    // - RightAngle
    // - OppositeSide
    // 
    // Based on the direction of the port coming out of the input (and into the
    // output), we pick a shape of route, a rotation direction, and whether or 
    // not the wire requires reflection.
    let routetype, rotation, yreflect =
        match outputPortOri, inputPortOri with 
            | Symbol.Top, Symbol.Top -> Sameside, PosY, false
            | Symbol.Top, Symbol.Right -> Rightangle, PosX, true
            | Symbol.Top, Symbol.Bottom -> Oppositeside, NegX, false
            | Symbol.Top, Symbol.Left -> Rightangle, NegX, false
            
            | Symbol.Right, Symbol.Top -> Rightangle, PosY, false 
            | Symbol.Right, Symbol.Right -> Sameside, PosX, false
            | Symbol.Right, Symbol.Bottom -> Rightangle,NegY, true
            | Symbol.Right, Symbol.Left -> Oppositeside, PosY, false

            | Symbol.Bottom, Symbol.Top ->  Oppositeside, PosX, false
            | Symbol.Bottom, Symbol.Right -> Rightangle, PosX, false
            | Symbol.Bottom, Symbol.Bottom -> Sameside, NegY, false
            | Symbol.Bottom, Symbol.Left -> Rightangle, NegX, true

            | Symbol.Left, Symbol.Top -> Rightangle, PosY, true
            | Symbol.Left, Symbol.Right -> Oppositeside, NegY, false
            | Symbol.Left, Symbol.Bottom -> Rightangle, NegY, false 
            | Symbol.Left, Symbol.Left -> Sameside, NegX, false

    // Get the adjusted input port position, applying any rotations and
    // reflections
    let inputPortPos': XYPos = 
        let relativePos =
            inputPortPos - outputPortPos
            |> rotate_rel (reverse_rotate rotation)
            |> yReflect yreflect
        
        relativePos + outputPortPos

    let (xyPairs: list<XYPos>), (isLeftToRight: bool) = 
        initialWireVerticesFromPorts outputPortPos inputPortPos' routetype
    
    // Does a thing
    convertVerticesToASegs 
        hostId 
        isLeftToRight 
        routetype 
        rotation 
        yreflect 
        outputPortPos 
        xyPairs

/// Render given Rotation Invariant Segment using colour and width properties given
/// Takes in 2 connecting segments to add rounded corners if appropriate
let renderRISegAndCorner (colour: string) (width: string) (segments: RISeg * RISeg * RISeg) : ReactElement =
    let prevSeg, segment', nextSeg = segments
    let segment = {segment' with Start = absXYPos segment'.Start}
    let widthOption = EEExtensions.String.tryParseWith System.Int32.TryParse width
    let renderWidth, halfWidth = 
        match widthOption with
        | Some 1 -> 1.5, 0.25
        | Some n when n < int "8" -> 2.5, 0.5
        | _ -> 3.5, 1.0
    let lineParameters = { defaultLine with Stroke = colour; StrokeWidth = string renderWidth }
    let circleParameters = { defaultCircle with R = halfWidth; Stroke = colour; Fill = colour }
    let pathParameters = { defaultPath with Stroke = colour; StrokeWidth = string renderWidth }

    // If first segment / previous or current segment too short /  collinear with previous segment -> no corner
    let startCorner = not (prevSeg.Index = -1 || (min (abs prevSeg.Length) (abs segment.Length)) < (Wire.arcSize * 2.0) || prevSeg.Dir = segment.Dir)

    let endCorner =
        // If last segment / current or next segment too short / collinear with next segment -> no corner
        if nextSeg.Index = 0 || (min (abs segment.Length) (abs nextSeg.Length)) < (Wire.arcSize * 2.0) || segment.Dir = nextSeg.Dir then
            let endPoint = getRISegEnd segment.Start segment
            [
                makeCircle endPoint.X endPoint.Y circleParameters
            ], false
        else // Corner
            let arcStart =
                match segment.Length with
                | l when l < 0 -> getRISegEnd segment.Start {segment with Length = segment.Length + Wire.arcSize}
                | _ -> getRISegEnd segment.Start {segment with Length = segment.Length - Wire.arcSize}
            let arcMid = nextSeg.Start
            let arcEnd =
                match nextSeg.Length with
                | l when l < 0 -> getRISegEnd nextSeg.Start {nextSeg with Length = - Wire.arcSize}
                | _ -> getRISegEnd nextSeg.Start {nextSeg with Length = Wire.arcSize}
            [
                makeCircle arcStart.X arcStart.Y circleParameters
                makePath arcStart arcMid arcEnd arcEnd pathParameters
                makeCircle arcEnd.X arcEnd.Y circleParameters
            ], true

    let segmentElements =
        let segStart =
            match startCorner, segment.Length with
            | true, l when l < 0 -> getRISegEnd segment.Start {segment with Length = - Wire.arcSize}
            | true, _ -> getRISegEnd segment.Start {segment with Length = Wire.arcSize}
            | false, _ -> segment.Start
        let segEnd =
            match (snd endCorner), segment.Length with
            | true, l when l < 0 -> getRISegEnd segment.Start {segment with Length = segment.Length + Wire.arcSize}
            | true, _ -> getRISegEnd segment.Start {segment with Length = segment.Length - Wire.arcSize}
            | false, _ -> getRISegEnd segment.Start segment
        [
            makeLine segStart.X segStart.Y segEnd.X segEnd.Y lineParameters
        ]
        @
        fst endCorner

    g [] segmentElements

/// Takes in a Rotation Invariant Segment List and renders the resulting React Element List, with rounded corners if appropriate
let renderRISegList (colour: string) (width: string) (segs: RISeg list) : ReactElement list =
    let riSegs =
        segs
        |> List.filter ( fun seg -> seg.Length <> 0.0 )
        |> List.mapi (fun i seg -> {seg with Index = i})
        |> List.toSeq
    let dummyStartRISeg = {Seq.head riSegs with Index = -1; Length = 0.0}
    let dummyEndRISeg = {Seq.head riSegs with Index = 0; Length = 0.0}
    let riSegTriplets =
        Seq.zip3 (Seq.append [dummyStartRISeg] riSegs) riSegs (Seq.append (Seq.tail riSegs) [dummyEndRISeg])
        |> List.ofSeq
    riSegTriplets
    |> List.map ( fun segTrip -> renderRISegAndCorner colour width segTrip )

///
type WireRenderProps =
    {
        key: string
        Segments: ASeg list
        ColorP: HighLightColor
        StrokeWidthP: int
        OutputPortLocation: XYPos
    }

let singleWireView = 
    FunctionComponent.Of(
        fun (props: WireRenderProps) ->
            let renderWireSegmentList : ReactElement list =
                props.Segments
                |> List.map aSegToRISeg
                |> renderRISegList (props.ColorP.Text()) (string props.StrokeWidthP)

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

                    let outputPortLocation = Symbol.getOnePortLocationNew model.Symbol stringOutId PortType.Output // TODO: Symbol fn in BusWire
                    let props =
                        {
                            key = match wire.Id with | ConnectionId s -> s
                            Segments = List.map makeASegPos wire.Segments
                            ColorP = wire.Color
                            StrokeWidthP = wire.Width
                            OutputPortLocation = snd outputPortLocation
                        }
                    singleWireView props
            )
    TimeHelpers.instrumentTime "WirePrepareProps" rStart // Time creation of wires
    let symbols = Symbol.view model.Symbol (Symbol >> dispatch)
 
    g [] [(g [] wires); symbols]
    |> TimeHelpers.instrumentInterval "WireView" start

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
        (testSegment: ASeg)
        (boundingBox: BoundingBox): bool * Option<XYPos> =

    // Get top-left, bottom-right corners, and evaluate bottom-left and
    // top-right ones
    let topLeft, bottomRight = getTopLeftAndBottomRightCorner boundingBox
    let bottomLeft = {X = topLeft.X; Y = bottomRight.Y}
    let topRight = {X = bottomRight.X; Y = topLeft.Y}

    // Checks whether either of the segment's vertices (start or end) are
    // within the bounding box
    let segment = makeASegPos testSegment
    let segmentVertexInBox =
        let pointInBox (point: XYPos): bool =
            point.X > topLeft.X
                && point.X < bottomRight.X
                && point.Y > topLeft.Y
                && point.Y < bottomRight.Y

        (pointInBox segment.Start) || (pointInBox segment.End)

    // Gets a list of points of intersection between a segment and the four
    // sides of the bounding box
    let intersectionList : XYPos list =
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
let distanceFromPointToSegment (point: XYPos) (segment: ASeg): float =

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
        (wiresToBeRouted: ConnectionId list)
        (model: Model): Model =

    // Evaluate a new WX mapping, rerouting the given wires
    let newWX =
        let updatedWireMap =

            // Evaluates a new list of segments for a wire
            let mapWireSegments (wire: Wire) =
                let segments =
                    let positions =
                        Symbol.getTwoPortLocations // TODO: Symbol fn in BusWire
                        <| model.Symbol
                        <| wire.InputPort
                        <| wire.OutputPort
                    makeInitialASegList wire.Id (fst positions) (snd positions)

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
        (selectBox: BoundingBox): ASeg list =

    // Filter, returning true if the segment and bounding box intersect
    let segmentFilter (segment: ASeg) =
        fst (segmentIntersectsBoundingBoxCoordinates segment selectBox)

    // Filter for only the segments which intersect
    List.filter segmentFilter model.WX[wireId].Segments

/// Finds the closest segment in a wire to a point (using euclidean distance)
let getClosestSegment
        (model: Model)
        (wireId: ConnectionId)
        (pos: XYPos): ASeg =

    let distanceToPoint (segment: ASeg) =
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
let checkSegmentAngle (segment: ASeg) (name: string): unit =

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
let segPointsLeft (segment: ASeg): bool =
    (abs segment.Start.X > abs segment.End.X) && (segment.Dir = Horizontal)

/// Checks the segment's length along the X axis
let segXDelta (segment: ASeg): float =
    (abs segment.End.X) - (abs segment.Start.X)

/// Given two segments which are joined at a given position, change the X
/// coordinate of that coordinate (compensating for negative values)
let moveXJoinPos
    (newXValue: float)
    (segment1: ASeg)
    (segment2: ASeg): ASeg list =

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
        (segment1: ASeg)
        (segment2: ASeg): ASeg list =

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
        (testSegment: ASeg)
        (firstSegment: ASeg)
        (lastSegment: ASeg)
        (distance: float) =

    let controlSegOri = 
        match firstSegment.Dir,lastSegment.Dir,testSegment.Dir with
        | Horizontal, Horizontal, _ ->Vertical
        | Vertical, Vertical, _ -> Horizontal
        | _,_,Horizontal -> Horizontal
        | _,_,Vertical -> Vertical

    // Stick length can be shrunk for segments which aren't at the end of their
    // wires -- so we find a value for that shrink factor here
    let shrink =
        let atEnd = testSegment.Index < 1 || testSegment.Index > 5
        if atEnd then 1.0
        else 0.5

    // With that shrink factor, we find the minimum and maximum distance from
    // the segment to the wire's start/end
    let minimumDistanceX =
        firstSegment.Start.X +
        Wire.stickLength * shrink -
        testSegment.End.X
    let maximumDistanceX =
        lastSegment.End.X -
        Wire.stickLength * shrink -
        testSegment.Start.X

    let minimumDistanceY =
        firstSegment.Start.Y +
        Wire.stickLength * shrink -
        testSegment.End.Y
    let maximumDistanceY =
        lastSegment.End.Y -
        Wire.stickLength * shrink -
        testSegment.Start.Y

    // These helpers make the match case a little less verbose
    let positive = distance > 0.0
    let negative = distance < 0.0

    // Check whether a given end of the test segment is vertically close to the
    // first/last segment in the wire
    let yJoined (segment: ASeg) (point: XYPos): bool =
        abs (segment.Start.Y - point.Y) > 0.0001

    let xJoined (segment: ASeg) (point: XYPos): bool =
        abs (segment.Start.X -  point.X) > 0.0001

    // I haven't spent the time to understand this match case, but it works.
    // I assume finds how much a wire section _can_ move given its position in
    // that wire.
    match controlSegOri with 
    | Vertical ->
        match testSegment.Index with
            | _ when testSegment.Dir = Horizontal -> distance
            | 3 when negative && yJoined firstSegment testSegment.Start -> distance
            | 3 when positive && yJoined lastSegment testSegment.End -> distance
            | 1 | 2 -> max minimumDistanceX distance
            | 4 | 5 -> min maximumDistanceX distance
            | 3 -> distance |> max minimumDistanceX |> min maximumDistanceX
            | _ -> distance
    | Horizontal ->
        match testSegment.Index with
            | _ when testSegment.Dir = Horizontal -> distance
            | 3 when negative && xJoined firstSegment testSegment.Start -> distance
            | 3 when positive && xJoined lastSegment testSegment.End -> distance
            | 1 | 2 -> max minimumDistanceY distance
            | 4 | 5 -> min maximumDistanceY distance
            | 3 -> distance |> max minimumDistanceY |> min maximumDistanceY
            | _ -> distance

/// Remove pairs of adjacent segments which are aligned but not of the same
/// sign
let removeRedundantSegments (segments: ASeg list) =

    // Reduces a sequential pair of segments
    let reduce (segment1: ASeg) (segment2: ASeg): ASeg list =

        let direction (segment: ASeg): int =
            sign (abs segment.End.X - abs segment.Start.X)

        let length (segment: ASeg): float =
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
            let setEnd (segment: ASeg) (value: float): ASeg =
                {segment with End = setAbsoluteX segment.Start value}
            let setStart (segment: ASeg) (value: float): ASeg =
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
let moveSegment (segment: ASeg) (distance:float) (model:Model) =
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
                let newValue = value + moveDistance
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
                    ManualRoute = true
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
let getConnectedWires (model : Model) (componentIDs : ComponentId list) =

    let isConnected (wire: Wire) =
        let inputs, outputs = Symbol.getPortLocations model.Symbol componentIDs // TODO: Symbol fn in BusWire
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
let filterWiresByCompMoved (model: Model) (componentIDs: ComponentId list) =
    let inputs, outputs = Symbol.getPortLocations model.Symbol componentIDs // TODO: Symbol fn in BusWire

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
        Symbol.getTwoPortLocations model.Symbol inputs outputs // TODO: Symbol fn in BusWire

    // Autoroute a segment between the wire's ports, and assign it to the wire
    {
        wire with
            Segments = makeInitialASegList wire.Id (fst locations) (snd locations)
    }

/// Reverse the segment order, as well as (start, end) coordinates
let revSegments (segments: ASeg list) =
    let invert (segment: ASeg) : ASeg =
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

    let setEndAtIndex (segmentIndex: int) (segment: ASeg) =
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

    let setStartAtIndex (segmentIndex: int) (segment: ASeg) =
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

    let setAllAtIndex (segmentIndex: int) (segment: ASeg) =
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
        (segment: ASeg): ASeg =

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
        (segments: ASeg list)
        (newPortPosition: XYPos): ASeg list option =

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
        let segmentAutorouted (segment: ASeg): bool =
           segment.ManualRoute

        segments
        |> List.takeWhile segmentAutorouted
        |> List.length
        |> (fun index -> if index > 5 then None else Some (index + 1))

    let preEndScale (segmentIndex: int): ASeg list option =

        let segment = segments[segmentIndex]
        let fixedPoint = absXYPos segment.End

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

/// Moves a wire by a specified amount by adding a XYPos to each start and end
/// point of each segment
let moveWire (wire : Wire) (difference : XYPos): Wire =

    let transformer (segment: ASeg) : ASeg =
        {
            segment with
                ManualRoute = segment.ManualRoute
                Start = Symbol.posAdd segment.Start difference
                End = Symbol.posAdd segment.End difference
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
            | true -> Symbol.getInputPortLocation symbol wire.InputPort // TODO: Symbol fn in BusWire
            | false -> Symbol.getOutputPortLocation symbol wire.OutputPort // TODO: Symbol fn in BusWire

    // Partially route from input to end by reversing segments, and swapping
    // the start/end values.
    let newSegments =
        if isInput then
            partialAutoRoute (revSegments wire.Segments) (snd newPort)
            |> Option.map revSegments
        else
            partialAutoRoute wire.Segments (snd newPort)

    // Take the new segments and create a wire from them
    newSegments
    |> Option.map (fun segs -> {wire with Segments = segs})
    |> Option.defaultValue (autorouteWire model wire)

/// Re-routes the wires in the model based on a list of components that have been altered.
/// If the wire input and output ports are both in the list of moved components, does not re-route wire but instead translates it.
/// Keeps manual wires manual (up to a point).
/// Otherwise it will auto-route wires connected to components that have moved
let updateWires (model: Model) (compIdList: ComponentId list) (diff: XYPos) =
    let (inputWires, outputWires, fullyConnectedWires) = filterWiresByCompMoved model compIdList

    let newWires = 
        model.WX
        |> Map.toList
        |> List.map (fun (cId, wire) -> 
            if List.contains cId fullyConnectedWires then // Translate wires that are connected to moving components on both sides
                (cId, moveWire wire diff)
            elif List.contains cId inputWires then // Only route wires connected to ports that moved for efficiency
                (cId, updateWire model wire true)
            elif List.contains cId outputWires then
                (cId, updateWire model wire false)
            else
                (cId, wire))
        |> Map.ofList

    {model with WX = newWires}

/// Update the Model according to the message received, return a tuple of new Model and Msg 
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    
    match msg with
    | Symbol sMsg ->
        let sm,sCmd = Symbol.update sMsg model.Symbol
        {model with Symbol=sm}, Cmd.map Symbol sCmd

    | UpdateWires (componentIdList, diff) -> updateWires model componentIdList diff, Cmd.none

    | AddWire ( (inputId, outputId) : (InputPortId * OutputPortId) ) ->
        let portOnePos, portTwoPos = Symbol.getTwoPortLocations model.Symbol inputId outputId // TODO: Symbol fn in BusWire
        let wireId = ConnectionId(JSHelpers.uuid())
        let segmentList = makeInitialASegList wireId portOnePos portTwoPos
        
        let newWire = 
            {
                Id = wireId
                InputPort = inputId
                OutputPort = outputId
                Color = HighLightColor.DarkSlateGrey
                Width = 1
                Rotation = PosY
                YReflect = false
                Segments = segmentList
            }
            
        let wireAddedMap = Map.add newWire.Id newWire model.WX
        let newModel = {model with WX = wireAddedMap}

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
                    | SplitWire _ ->
                        match inPort.PortNumber with 
                        | Some 0 -> {symbol with InWidth0 = Some wire.Width}
                        | x -> failwithf $"Error: wire found with input port {x} other than 0 connecting to SplitWire"
                        |> (fun sym -> Map.add symId sym m)
                    | MergeWires ->
                        match inPort.PortNumber with 
                        | Some 0 -> 
                            Map.add symId  {symbol with InWidth0 = Some wire.Width} m
                        | Some 1 -> 
                            Map.add symId {symbol with InWidth1 = Some wire.Width} m
                        | x -> failwithf $"Error: wire found with input port {x} other than 0 or 1 connecting to MergeWires"
                    | _ -> m

            let newWX = ((Map.empty, model.WX) ||> Map.fold addWireWidthFolder)
            let symbolsWithWidths = (model.Symbol.Symbols, newWX) ||> Map.fold addSymbolWidthFolder

            { model with 
                WX = newWX; Notifications = None ; 
                ErrorWires=[]; 
                Symbol = {model.Symbol with Symbols = symbolsWithWidths}
            }, Cmd.none

        let canvasState = (Symbol.extractComponents model.Symbol, extractConnections model )

        match BusWidthInferer.inferConnectionsWidth canvasState with
        | Ok connWidths ->
            processConWidths connWidths
        | Error e ->
                { model with 
                    Notifications = Some e.Msg }, Cmd.ofMsg (ErrorWires e.ConnectionsAffected)
    
    | CopyWires (connIds : ConnectionId list) ->
        let copiedWires = Map.filter (fun connId _ -> List.contains connId connIds) model.WX
        { model with CopiedWX = copiedWires }, Cmd.none

    | ErrorWires (connectionIds : ConnectionId list) -> 
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

    | SelectWires (connectionIds : ConnectionId list) -> //selects all wires in connectionIds, and also deselects all other wires
        let newWX =
            model.WX
            |> Map.map
                ( fun id wire -> 
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

    | DeleteWires (connectionIds : ConnectionId list) -> 
        let newWX =
             model.WX
             |> Map.filter (fun id _ -> not (List.contains id connectionIds))
        {model with WX = newWX}, Cmd.ofMsg BusWidths

    // Handles wire dragging request
    | DragWire (connId : ConnectionId, mMsg: MouseT) ->
        match mMsg.Op with

        // On a mouse press, select that given segment
        | Down ->
            let segId = getClickedSegment model connId mMsg.Pos
            {model with SelectedSegment = segId }, Cmd.none
        
        // On a mouse drag, move the segment the mouse is hovering over
        | Drag ->

            // Get the segment
            let aSeg: ASeg =

                // Picks the segment being selected from a list of all the 
                // wire's segments
                let aSegOption: option<ASeg> =
            
                    let choiceFunctor (segment: ASeg): option<ASeg> =
                        if segment.Id = model.SelectedSegment then Some segment
                        else None 
                    
                    model.WX[connId].Segments
                    |> List.choose choiceFunctor
                    |> List.tryExactlyOne
                
                // Make sure a segment was found
                match aSegOption with
                | Some aSeg -> aSeg
                | None -> failwithf "Error: Segment Id not found in list"

            // Check the segment is draggable; otherwise, do nothing
            if aSeg.Draggable then

                // Evaluate the drag distance (perpendicular to the segment's
                // direction) to the mouse position
                let distanceToMove = 
                    match aSeg.Dir with
                    | Horizontal -> mMsg.Pos.Y - abs aSeg.Start.Y
                    | Vertical -> mMsg.Pos.X - abs aSeg.Start.X

                // Create a new wire segment by dragging the current one, and 
                // insert it into the model
                let newWire = moveSegment aSeg distanceToMove model
                let newWX = Map.add aSeg.HostId newWire model.WX
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
    
    | ResetModel -> { model with WX = Map.empty; ErrorWires = []; Notifications = None }, Cmd.none
    
    | LoadConnections conns -> // we assume components (and hence ports) are loaded before connections
        let posMatchesVertex (pos:XYPos) (vertex: float*float) =
            let epsilon = 0.00001
            abs (abs pos.X - abs (fst vertex)) < epsilon && abs (abs pos.Y - abs (snd vertex)) < epsilon
            |> (fun b -> if not b then printf $"Bad wire endpoint match on {pos} {vertex}"; b else b)
        let newWX =
            let connsToWX conn = 
                let inputId = InputPortId conn.Target.Id
                let outputId = OutputPortId conn.Source.Id
                let connId = ConnectionId conn.Id
                let aSegs = issieVerticesToASegs connId conn.Vertices
                let riSegs = aSegListToRISegList aSegs
                let makeWirePosMatchSymbol inOut (wire:Wire) =
                    match inOut with
                    | true  -> posMatchesVertex 
                                (snd (Symbol.getInputPortLocation model.Symbol inputId)) 
                                (List.head conn.Vertices)
                    | false -> posMatchesVertex 
                                (snd (Symbol.getOutputPortLocation model.Symbol outputId)) 
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
                            updateWire model wire inOut
                        )
                                
                connId,
                {   Id = ConnectionId conn.Id
                    InputPort = inputId
                    OutputPort = outputId
                    Color = HighLightColor.DarkSlateGrey
                    Width = 1
                    Rotation = PosY
                    YReflect = false
                    Segments = List.map riSegToASeg riSegs
                }
                |> makeWirePosMatchSymbol false
                |> makeWirePosMatchSymbol true
            
            conns 
            |> List.map connsToWX
            |> Map.ofList
        
        { model with WX = newWX }, Cmd.none

//---------------Other interface functions--------------------//

let wireIntersectsBoundingBox (w : Wire) (bb : BoundingBox) =
    w.Segments
    |> List.map ( fun seg -> fst (segmentIntersectsBoundingBoxCoordinates seg bb) )
    |> List.contains true

///
let getIntersectingWires (wModel : Model) (selectBox : BoundingBox) : ConnectionId list = 
    wModel.WX
    |> Map.map (fun _connId wire -> wireIntersectsBoundingBox wire selectBox)
    |> Map.filter (fun _connId intersects -> intersects)
    |> Map.toList
    |> List.map fst

///searches if the position of the cursor is on a wire in a model
///Where n is 5 pixels adjusted for top level zoom
let getWireIfClicked (wModel : Model) (pos : XYPos) (n : float) : ConnectionId Option =
    let boundingBox = {BoundingBox.X = pos.X - n; Y = pos.Y - n; H = n*2.; W = n*2.}
    let intersectingWires = getIntersectingWires (wModel : Model) boundingBox
    List.tryHead intersectingWires

///
let pasteWires (wModel : Model) (newCompIds : ComponentId list) : (Model * ConnectionId list) =
    let oldCompIds = Symbol.getCopiedSymbols wModel.Symbol
    
    let pastedWires =
        let createNewWire (oldWire : Wire) : Wire list =
            let newId = ConnectionId(JSHelpers.uuid())
    
            match Symbol.getEquivalentCopiedPorts wModel.Symbol oldCompIds newCompIds (oldWire.InputPort, oldWire.OutputPort) with
            | Some (newInputPort, newOutputPort) ->

                let portOnePos, portTwoPos = Symbol.getTwoPortLocations wModel.Symbol (InputPortId newInputPort) (OutputPortId newOutputPort) // TODO: Symbol fn in BusWire
                let segmentList = makeInitialASegList newId portOnePos portTwoPos
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

///Given the Model and the connectionID list, return the tuple of InputPortId list and OutputPortId list
let getPortIdsOfWires (model: Model) (connIds: ConnectionId list) : (InputPortId list * OutputPortId list) =
    (([], []), connIds)
    ||> List.fold (fun (inputPorts, outputPorts) connId ->
            (model.WX[connId].InputPort :: inputPorts, model.WX[connId].OutputPort :: outputPorts))
