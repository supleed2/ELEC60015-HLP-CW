﻿//keep old drawing style but with new functions  -> DONE
//genAportoffsetmap -> different for mux adder (ports not on LHS/RHS), -> easy to change symbols in the future
//rotation of these components is different as well -> TO ADD
//add new addClock -> work with floats instead of ints
//complete rotation for all symbols -> pending 6 + 7 points symbols
//Output same rotation as input with rotation (o+2)%4  
//function to create map for custom components port names -> DONE
//add extensions as Maps in PortNamesMap 
// change in points the w,h to floats in the begining??
//change names and if found better types
//why adder ports needs different rotation function??? 

//---------------------------------------------------------------------------------//
//--------------------AP1919 CODE SECTION STARTS-------------------------------------//
//---------------------------------------------------------------------------------//


(*
This module draws schematics component symbols. Each symbol is associated with a unique Issie component.
*)

module Symbol
open Electron
open ElectronAPI
open Fable.React
open Fable.React.Props
open Elmish
open DrawHelpers
open CommonTypes
open System.Text.RegularExpressions



/// --------- STATIC VARIABLES --------- ///

let GridSize = 30 

/// ---------- SYMBOL TYPES ---------- ///

type PortOrientation = Right | Bottom | Left | Top

type PortOrientationOffset = {
    Side: PortOrientation // Designated which side of symbol port is on (0 -> right, 1 -> top, 2 -> left, 3 -> bottom). to have coherency with STransform.
    Offset: XYPos
    SideIndex: int
}

type Symbol =
    {
        Pos: XYPos
        STransform: Rotation // Describes how symbol is rotated/flipped (0 -> 0 deg, 1 -> 90 deg, 2 -> 180 deg, -> 3 -> 270 deg).
        InWidth0: int option
        InWidth1: int option
        Id : ComponentId       
        Compo : Component                 
        Colour: string
        ShowInputPorts: bool
        ShowOutputPorts: bool
        Opacity: float
        Moving: bool
        APortOffsetsMap: Map<string,PortOrientationOffset>
    }



type Model = {
    Symbols: Map<ComponentId, Symbol>
    CopiedSymbols: Map<ComponentId, Symbol>
    Ports: Map<string, Port>                            // string since it's for both input and output ports

    InputPortsConnected:  Set<InputPortId>              // we can use a set since we only care if an input port 
                                                        // is connected or not (if so it is included) in the set 

    OutputPortsConnected: Map<OutputPortId, int>        // map of output port id to number of wires connected to that port
    }

//----------------------------Message Type-----------------------------------//


type Msg =
    | MouseMsg of MouseT
    | AddSymbol of pos:XYPos * compType:ComponentType * lbl: string
    | CopySymbols of ComponentId list
    | DeleteSymbols of sIds:ComponentId list
    | ShowAllInputPorts | ShowAllOutputPorts | DeleteAllPorts 
    | MoveSymbols of compList: ComponentId list * move: XYPos
    | ShowPorts of ComponentId list
    | SelectSymbols of ComponentId list// Issie interface
    | RotateSymbols of ComponentId list //First Attempt at implementing a way to rotate symbol.
    | FlipHSymbols of ComponentId list //First Attempt at implementing a way to flip symbol horizontally.
    | FlipVSymbols of ComponentId list //First Attempt at implementing a way to flip symbol vertically.
    | SymbolsHaveError of sIds: ComponentId list
    | ChangeLabel of sId : ComponentId * newLabel : string
    | ChangePort of sId : ComponentId * portName: string * portSide:string
    | PasteSymbols of sIds: ComponentId list
    | ColorSymbols of compList : ComponentId list * colour : HighLightColor
    | ErrorSymbols of errorIds: ComponentId list * selectIds: ComponentId list * isDragAndDrop: bool
    | ChangeNumberOfBits of compId:ComponentId * NewBits:int 
    | ChangeLsb of compId: ComponentId * NewBits:int64 
    | ChangeConstant of compId: ComponentId * NewBits:int64 * NewText:string
    | ResetModel // For Issie Integration
    | LoadComponents of  Component list // For Issie Integration
    | WriteMemoryLine of ComponentId * int64 * int64 // For Issie Integration 
    | WriteMemoryType of ComponentId * ComponentType

//---------------------------------helper types and functions----------------//

let posDiff (a:XYPos) (b:XYPos) =
    {X=a.X-b.X; Y=a.Y-b.Y}

let posAdd (a:XYPos) (b:XYPos) =
    {X=a.X+b.X; Y=a.Y+b.Y}

let posOf x y = {X=x;Y=y}

//STransform Finite State Machine


let stransform_fsm (prev_state:Rotation) (comp: ComponentType) : Rotation = 
    let stransformUpdate(prev_state:Rotation):Rotation =
        match prev_state with
        | R0 -> R90
        | R90 -> R180
        | R180 -> R270
        | R270 -> R0
    match comp with 
    |Custom _ |MergeWires |SplitWire _ -> prev_state
    |_ -> stransformUpdate prev_state

let orientationEncoder (orientation:PortOrientation) : int = 
    match orientation with
    | Right -> 0
    | Bottom -> 1
    | Left -> 2
    | Top -> 3

let orientationDecoder (orientation:int) : PortOrientation = 
    match orientation with
    | 0 -> Right
    | 1 -> Bottom
    | 2 -> Left
    | 3 -> Top
    | _ -> Right


// ----- helper functions for titles ----- //

let titleGen text (buswidth) =  
        if buswidth = 1 then text else text + "(" + string(buswidth-1) + "..0)"

let bustitleGen wob lsb = 
    if wob <> 1 then"(" + string(wob + lsb - 1) + ".." + string(lsb) +  ")" else string(lsb)

///Decodes the component type into component labels
let getLabel compType = 
    match compType with
    | Not | And | Or | Xor | Nand | Nor | Xnor -> "G"
    | Mux2 -> "MUX"
    | Demux2 -> "DM"
    | NbitsAdder _ -> "A"
    | NbitsXor _ -> "XOR"
    | DFF | DFFE -> "FF"
    | Register _ | RegisterE _ -> "REG"
    | AsyncROM1 _ -> "AROM"
    | ROM1 _ -> "ROM"
    | RAM1 _ -> "RAM"
    | AsyncRAM1 _ -> "ARAM"
    | Custom c ->
        c.Name + (if c.Name |> Seq.last |> System.Char.IsDigit then "." else "")
    | Constant1 _ -> "C"
    | BusCompare _ -> "EQ"
    | Decode4 -> "DEC"
    | BusSelection _ -> "SEL"
    | _ -> ""


//-----------------------------Skeleton Model Type for symbols----------------//

// Text to be put inside different Symbols depending on their ComponentType
let getTitle (comp:Component) =
    match comp.Type with
    | And | Nand-> "&"
    | Or | Nor-> "≥1"
    | Xor | Xnor -> "=1"
    | Not -> "1"
    | Decode4 -> "Decode"
    | NbitsAdder n -> titleGen "Adder" n
    | Register n | RegisterE n-> titleGen "Register" n
    | AsyncROM1 _ -> "Async-ROM"
    | ROM1 _ -> "Sync-ROM"
    | RAM1 _ -> "Sync-RAM"
    | AsyncRAM1 _ -> "Async-RAM"
    | DFF -> "DFF"
    | DFFE -> "DFFE"
    | NbitsXor (x)->   titleGen "N-bits-Xor" x
    | Custom x -> x.Name
    | _ -> ""

///Helper function to create portNamesMap for custom components
let customPortNamesMap inputLabels outputLabels =
    let adder x ((name:string),n) (portType:PortType)=
        let key = 
            match portType with
            |PortType.Input -> "I"+ string x
            |PortType.Output -> "O"+ string x
        [(key,name)]
    let inputList = if (List.length inputLabels)  < 1 then [] else ( [0..((List.length inputLabels)-1)] |> List.collect (fun x -> (adder x inputLabels[x] PortType.Input)))
    let outputList = if (List.length outputLabels)  < 1 then [] else ( [0..((List.length outputLabels)-1)] |> List.collect (fun x -> (adder x outputLabels[x] PortType.Output)))
    
    [inputList;outputList] |> List.concat |> Map.ofList

/// Input and Output names of the ports depending on their ComponentType
/// returns map with (port key in APortOffsetsMap, port name)
let portNamesMap (comp:Component) = 
    match comp.Type with
    | NbitsAdder _ -> Map [ ("I0", "Cin"); ("I1", "A"); ("I2","B");("O0","Sum");("O1","Cout") ]
    | Decode4 -> Map [ ("I0", "Sel"); ("I1", "Data"); ("O0","0");("O1","1"); ("O2","2");("O3","3") ]
    | Register _ |DFF -> Map [ ("I0", "D"); ("O0","Q") ] 
    | RegisterE _ |DFFE -> Map [ ("I0", "D"); ("I1","EN"); ("O0","Q") ]
    | ROM1 _ |AsyncROM1 _ -> Map [ ("I0", "Addr");("O0","Dout") ]
    | RAM1 _ -> Map [ ("I0", "Addr"); ("I1", "Din"); ("I2","Wen");("O0","Dout") ]
    | AsyncRAM1 _ -> Map [ ("I0", "Addr"); ("I1", "Din"); ("I2","Wen");("O0","Dout") ]
    | Mux2 -> Map [ ("I0", "0"); ("I1", "1"); ("I2","SEL");("O0","OUT") ]
    | Demux2 -> Map [ ("I0", "IN"); ("I1", "SEL"); ("O0","0");("O1","1") ]
    | NbitsXor _ -> Map [ ("I0", "P"); ("I1", "Q"); ("O0","Out") ]
    | Custom x -> customPortNamesMap x.InputLabels x.OutputLabels 
    |_ -> Map.empty
   // |Mux4 -> (["0"; "1"; "2"; "3" ;"SEL"],["OUT"])
   // |Demux4 -> (["IN"; "SEL"],["0"; "1";"2"; "3";])
   // |Demux8 -> (["IN"; "SEL"],["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7"])
   // |Mux8 -> (["0"; "1"; "2" ; "3" ; "4" ; "5" ; "6" ; "7";"SEL"],["OUT"])
   // |_ -> ([],[])
   // EXTENSION: Extra Components made that are not currently in Issie. Can be extended later by using this code as it is .
   // !! They need to be in Map Format !!



//-----------------------Component and Symbol Creation---------------------//

///Rounds an integer to any given number. The first parameter is the number to round to, the second parameter is the input number that will be rounded
let inline roundToZ (z : int) (number : int) = //IMPLEMENT IT INSIDE ARGS??? -> DETELE IT???
    number + abs((number % z) - z)

//Find the custom component's I/O label with the maximum size 
let customCompMaxLabel (lst : (string * int) list) =
    let labelList = List.map (fst >> String.length) lst
    if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
    else List.max labelList

/// Genererates a component's Input/Output Port list
let portListGen numOfPorts hostID portType =
    if numOfPorts < 1 
    then []
    else
        [0..(numOfPorts-1)]
        |> List.collect (fun x ->
            [{
                Id = JSHelpers.uuid ()
                PortNumber = Some x
                PortType = portType
                HostId = hostID
            }])

/// Helper function to initialise each type of component
let createComponent (pos: XYPos) (comptype: ComponentType) (id:string) (label:string) rotation : Component =
    
    // match statement for each component type. the output is a 4-tuple that is used as an input to makecomponent (see below)
    // 4-tuple of the form ( number of input ports, number of output ports, Height, Width)
    //grid size is set at the begining
    let characteristics =  
        match comptype with
        | ROM _ | RAM _ | AsyncROM _ -> 
            failwithf "What? Legacy RAM component types should never occur"
        | And | Nand | Or | Nor | Xnor | Xor ->  (2 , 1, 2*GridSize , 2*GridSize) 
        | Not -> ( 1 , 1, 2*GridSize ,  2*GridSize) 
        | ComponentType.Input (a) -> ( 0 , 1, GridSize ,  2*GridSize)                
        | ComponentType.Output (a) -> (  1 , 0, GridSize ,  2*GridSize) 
        | ComponentType.Viewer a -> (  1 , 0, GridSize ,  GridSize) 
        | ComponentType.IOLabel  ->(  1 , 1, GridSize ,  2*GridSize) 
        | Decode4 ->( 2 , 4 , 4*GridSize  , 3*GridSize) 
        | Constant1 (a, b,_) | Constant(a, b) -> (  0 , 1, GridSize ,  2*GridSize) 
        | MergeWires -> ( 2 , 1, 2*GridSize ,  2*GridSize) 
        | SplitWire (a) ->(  1 , 2 , 2*GridSize ,  2*GridSize) 
        | Mux2 -> ( 3  , 1, 3*GridSize ,  2*GridSize) 
        // EXTENSION:    | Mux4 -> ( 5  , 1, 5*GridSize ,  2*GridSize)   
        // EXTENSION:    | Mux8 -> ( 9  , 1, 7*GridSize ,  2*GridSize) 
        | Demux2 ->( 2  , 2, 3*GridSize ,  2*GridSize) 
        // EXTENSION:   | Demux4 -> ( 2  , 4, 150 ,  50) 
        // EXTENSION:    | Demux8 -> ( 2  , 8, 200 ,  50) 
        | BusSelection (a, b) -> (  1 , 1, GridSize,  2*GridSize) 
        | BusCompare (a, b) -> ( 1 , 1, GridSize ,  2*GridSize) 
        | DFF -> (  1 , 1, 3*GridSize  , 3*GridSize) 
        | DFFE -> ( 2  , 1, 3*GridSize  , 3*GridSize) 
        | Register (a) -> ( 1 , 1, 3*GridSize  , 4*GridSize )
        | RegisterE (a) -> ( 2 , 1, 3*GridSize  , 4*GridSize) 
        | AsyncROM1 (a)  -> (  1 , 1, 3*GridSize  , 4*GridSize) 
        | ROM1 (a) -> (   1 , 1, 3*GridSize  , 4*GridSize) 
        | RAM1 (a) | AsyncRAM1 a -> ( 3 , 1, 3*GridSize  , 4*GridSize) 
        | NbitsXor (n) -> (  2 , 1, 3*GridSize  , 4*GridSize) 
        | NbitsAdder (n) -> (  3 , 2, 3*GridSize  , 4*GridSize) 
        | Custom x -> 
            let h = GridSize + GridSize * (List.max [List.length x.InputLabels; List.length x.OutputLabels])
            let maxInLength, maxOutLength = customCompMaxLabel x.InputLabels, customCompMaxLabel x.OutputLabels
            let maxW = maxInLength + maxOutLength + label.Length
            let scaledW = roundToZ GridSize (maxW * GridSize / 5) //Divide by 5 is just abitrary as otherwise the symbols would be too wide 
            let w = max scaledW (GridSize * 4) //Ensures a minimum width if the labels are very small
            ( List.length x.InputLabels, List.length x.OutputLabels, h ,  w)

    
    // function that helps avoid dublicate code by initialising parameters that are the same for all component types and takes as argument the others
    let createComponent' (inputPortsNo, outputPortsNo, h, w) label rotation: Component=  
        {
            Id = id 
            Type = comptype 
            Label = label 
            InputPorts = portListGen inputPortsNo id PortType.Input 
            OutputPorts  = portListGen outputPortsNo id PortType.Output 
            X  = int (pos.X - float w / 2.0) 
            Y = int (pos.Y - float h / 2.0) 
            H = h 
            W = w
            R = rotation
            SI = []   
        }

    createComponent' characteristics label rotation
   
/// Function to generate a new symbol
let createNewSymbol (pos: XYPos) (comptype: ComponentType) (label:string) rotation =
    let id = JSHelpers.uuid ()
    let comp = createComponent pos comptype id label rotation
    { 
      Pos = { X = pos.X - float comp.W / 2.0; Y = pos.Y - float comp.H / 2.0 }
      STransform = R0;
      ShowInputPorts = false
      ShowOutputPorts = false
      InWidth0 = None // set by BusWire
      InWidth1 = None
      Colour = "lightgrey"
      Id = ComponentId id
      Compo = comp
      Opacity = 1.0
      Moving = false
      APortOffsetsMap = Map.empty<string,PortOrientationOffset>  //this is initialised as empty and then the map is added in the addsymbol function (line: )
    }                                                            //for readme: this is because the component is not a parameter of the createNewSymbol function 
                                                                 //and adding it requires many changes in the other section of symbol. Ofcourse it is better to have it here so maybe a TODO during group work

/// Function to add ports to port model     
let addPortsToModel (model: Model) (symbol: Symbol) =
    let addOnePort (currentPorts: Map<string, Port>) (port: Port) =
        Map.add port.Id port currentPorts
    
    let addedInputPorts = (model.Ports, symbol.Compo.InputPorts) ||> List.fold addOnePort
    (addedInputPorts, symbol.Compo.OutputPorts) ||> List.fold addOnePort



//////////////////////KEEP ONLY SO THAT THE CODE COMPILES -> IT AFFECTS 2nd part of symbol (lines: 750 - END) ///////////////////////////////////
//-----------------------------------------GET PORT POSITION---------------------------------------------------
// Function that calculates the positions of the ports 

/// hack so that bounding box of splitwire, mergewires can be smaller height relative to ports
let inline getPortPosEdgeGap (ct: ComponentType) =
    match ct with
    | MergeWires | SplitWire _  -> 0.25
    | _ -> 1.0

let getPortPos (comp: Component) (port:Port) = 
    let (ports, posX) =
        if port.PortType = (PortType.Input) then
            (comp.InputPorts, 0.0)
        else 
            (comp.OutputPorts, float( comp.W ))
    let index = float( List.findIndex (fun (p:Port)  -> p = port) ports )
    let gap = getPortPosEdgeGap comp.Type 
    let posY = (float(comp.H))* (( index + gap )/( float( ports.Length ) + 2.0*gap - 1.0))  // the ports are created so that they are equidistant
    {X = posX; Y = posY}
let getPortPosModel (model: Model) (port:Port) =
    getPortPos (Map.find (ComponentId port.HostId) model.Symbols).Compo port
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//---------------------------------------APortOffsetMap HELPERS ----------------------------------------------//
///Helper function to find XYPos of ports on the RHS/LHS
let offsethelper (comp: Component) orientation (port:Port) = 
    let inline getPortPosEdgeGap (ct: ComponentType) =
        match ct with
        | MergeWires | SplitWire _  -> 0.25
        | _ -> 1.0
    let (ports, posX) =
        if port.PortType = (PortType.Input) then
            (comp.InputPorts, 0.0)
        else 
            (comp.OutputPorts, float( comp.W ))
    let index = float( List.findIndex (fun (p:Port)  -> p = port) ports )
    let gap = getPortPosEdgeGap comp.Type 
    let posY = (float(comp.H))* (( index + gap )/( float( ports.Length ) + 2.0*gap - 1.0))  // the ports are created so that they are equidistant 
    {X=posX;Y=posY}

/// Helper function which creates the keys and values of the APortOffsetMap from the Input/Output Port list of component
let portListToMap (portList: Port List) (symbol: Symbol) : Map<string,PortOrientationOffset>=
    let adder (symbol: Symbol) (port: Port) =
        let num = match port.PortNumber with |Some n -> n |_->(-1)
        let key = 
            match port.PortType with
            |PortType.Input -> "I"+ string num
            |PortType.Output -> "O"+ string num 
        let value = 
            match port.PortType with 
            |PortType.Input -> {Side=Left;Offset=(offsethelper symbol.Compo symbol.STransform port);SideIndex= num}
            |PortType.Output -> {Side=Right;Offset=offsethelper symbol.Compo symbol.STransform port;SideIndex= num}
        [(key,value)]
    if (portList.Length)  < 1 then [] |> Map.ofList
    else ( [0..(portList.Length-1)] |> List.collect (fun x -> (adder symbol portList[x])) ) |> Map.ofList

/// APortOffsetsMap generator
let genAPortOffsets (symbol: Symbol) (cType: ComponentType) : Map<string,PortOrientationOffset> = 
    
    //generator for MUX
    let genAPortOffsetsMux (symbol: Symbol) = 
        let getPosY index = (float(symbol.Compo.H))* (( index + 1.0 )/(3.0))
        Map.ofList [ ("I0", {Side=Left;Offset={X=0.0;Y=(getPosY 0.0)};SideIndex= -1});("I1", {Side=Left;Offset={X=0.0;Y=(getPosY 1.0)};SideIndex= -1});("I2", {Side=Bottom;Offset={X=float(symbol.Compo.W)/2.0;Y=float(symbol.Compo.H)*0.9};SideIndex= -1});("O0", {Side=Right;Offset={X=float(symbol.Compo.W);Y=float(symbol.Compo.H)/2.0};SideIndex= -1})]


    //generator for NBitsAdder
    let genAPortOffsetsAdder (symbol: Symbol) = 
        let getPosY index = (float(symbol.Compo.H))* (( index + 1.0 )/(3.0))
        Map.ofList [ ("I0", {Side=Bottom;Offset={X=float(symbol.Compo.W)/3.0;Y=float(symbol.Compo.H)};SideIndex= -1});("I1", {Side=Left;Offset={X=0.0;Y=(getPosY 0.0)};SideIndex= -1}); ("I2", {Side=Left;Offset={X=0.0;Y=(getPosY 1.0)};SideIndex= -1}); ("O0", {Side=Right;Offset={X=float(symbol.Compo.W);Y=float(symbol.Compo.H)/3.0};SideIndex= -1});("O1", {Side=Top;Offset={X=float(symbol.Compo.W)-30.0;Y=0.0};SideIndex= -1})]
        
    //generator for everything else
    let genAPortOffsets' (symbol: Symbol) : Map<string,PortOrientationOffset> =
        let inputList = symbol.Compo.InputPorts
        let outputList = symbol.Compo.OutputPorts
        let map1 = portListToMap inputList symbol
        let map2 = portListToMap outputList symbol
        Map.fold (fun acc key value -> Map.add key value acc) map1 map2
    
    match cType with
    |Mux2 -> genAPortOffsetsMux symbol
    |NbitsAdder _ ->  genAPortOffsetsAdder symbol
    |_ -> genAPortOffsets' symbol
 
/// Rotates port posistion (given the symbol rotation) by updating the APortOffsetsMap
let rotatePortMap (map:Map<string,PortOrientationOffset>) (symbol:Symbol) =
    
    let rotatePortMapMux (map:Map<string,PortOrientationOffset>) (symbol:Symbol) =
        match symbol.STransform with
        |R0 -> Map.ofList [ ("I0", {Side=Top;Offset={X=(float(symbol.Compo.H)*2.0/3.0);Y=(0.0)};SideIndex= -1});("I1", {Side=Top;Offset={X=(float(symbol.Compo.H)/3.0);Y=(0.0)};SideIndex= -1});("I2", {Side=Left;Offset={X=float(symbol.Compo.H)*0.1;Y=float(symbol.Compo.W)/2.0};SideIndex= -1});("O0", {Side=Bottom;Offset={X=float(symbol.Compo.H)/2.0;Y=float(symbol.Compo.W)};SideIndex= -1})]
        |R90 -> Map.ofList [ ("I0", {Side=Right;Offset={X=float(symbol.Compo.W);Y=(float(symbol.Compo.H)*2.0/3.0)};SideIndex= -1});("I1", {Side=Right;Offset={X=float(symbol.Compo.W);Y=(float(symbol.Compo.H)/3.0)};SideIndex= -1});("I2", {Side=Top;Offset={X=float(symbol.Compo.W)/2.0;Y=float(symbol.Compo.H)*0.1};SideIndex= -1});("O0", {Side=Left;Offset={X=0.0;Y=float(symbol.Compo.H)/2.0};SideIndex= -1})]
        |R180 -> Map.ofList [ ("I0", {Side=Bottom;Offset={X=(float(symbol.Compo.H)/3.0);Y=float(symbol.Compo.W)};SideIndex= -1});("I1", {Side=Bottom;Offset={X=(float(symbol.Compo.H)*2.0/3.0);Y=float(symbol.Compo.W)};SideIndex= -1});("I2", {Side=Right;Offset={X=float(symbol.Compo.H)*0.9;Y=float(symbol.Compo.W)/2.0};SideIndex= -1});("O0", {Side=Top;Offset={X=float(symbol.Compo.H)/2.0;Y=0.0};SideIndex= -1})]
        |R270 -> Map.ofList [ ("I0", {Side=Left;Offset={X=0.0;Y=(float(symbol.Compo.H)/3.0)};SideIndex= -1});("I1", {Side=Left;Offset={X=0.0;Y=(float(symbol.Compo.H)*2.0/3.0)};SideIndex= -1});("I2", {Side=Bottom;Offset={X=float(symbol.Compo.W)/2.0;Y=float(symbol.Compo.H)*0.9};SideIndex= -1});("O0", {Side=Right;Offset={X=float(symbol.Compo.W);Y=float(symbol.Compo.H)/2.0};SideIndex= -1})]

    let rotatePortMapAdder (map:Map<string,PortOrientationOffset>) (symbol:Symbol) =
        match symbol.STransform with
            |R0 -> Map.ofList [ ("I0", {Side=Left;Offset={X=0.0;Y=float(symbol.Compo.W)/3.0};SideIndex= -1});("I1", {Side=Top;Offset={X=float(symbol.Compo.H)*2.0/3.0;Y=0.0};SideIndex= -1}); ("I2", {Side=Top;Offset={X=float(symbol.Compo.H)/3.0;Y=0.0};SideIndex= -1}); ("O0", {Side=Bottom;Offset={X=float(symbol.Compo.H)*2.0/3.0;Y=float(symbol.Compo.W)};SideIndex= -1});("O1", {Side=Right;Offset={X=(symbol.Compo.H);Y=float(symbol.Compo.W)-30.0};SideIndex= -1})]
            |R90 -> Map.ofList [ ("I0", {Side=Top;Offset={X=float(symbol.Compo.W)*2.0/3.0;Y=0.0};SideIndex= -1});("I1", {Side=Right;Offset={X=float(symbol.Compo.W);Y=float(symbol.Compo.H)*2.0/3.0};SideIndex= -1}); ("I2", {Side=Right;Offset={X=float(symbol.Compo.W);Y=float(symbol.Compo.H)/3.0};SideIndex= -1}); ("O0", {Side=Left;Offset={X=0.0;Y=float(symbol.Compo.H)*2.0/3.0};SideIndex= -1});("O1", {Side=Bottom;Offset={X=30.0;Y=float(symbol.Compo.H)};SideIndex= -1})]
            |R180 -> Map.ofList  [ ("I0", {Side=Right;Offset={X=float(symbol.Compo.H);Y=float(symbol.Compo.W)*2.0/3.0};SideIndex= -1});("I1", {Side=Bottom;Offset={X=float(symbol.Compo.H)/3.0;Y=float(symbol.Compo.W)};SideIndex= -1}); ("I2", {Side=Bottom;Offset={X=float(symbol.Compo.H)*2.0/3.0;Y=float(symbol.Compo.W)};SideIndex= -1}); ("O0", {Side=Top;Offset={X=float(symbol.Compo.H)/3.0;Y=0.0};SideIndex= -1});("O1", {Side=Left;Offset={X=0.0;Y=30.0};SideIndex= -1})]
            |R270 -> Map.ofList [ ("I0", {Side=Bottom;Offset={X=float(symbol.Compo.W)/3.0;Y=float(symbol.Compo.H)};SideIndex= -1});("I1", {Side=Left;Offset={X=0.0;Y=float(symbol.Compo.H)/3.0};SideIndex= -1}); ("I2", {Side=Left;Offset={X=0.0;Y=float(symbol.Compo.H)*2.0/3.0};SideIndex= -1}); ("O0", {Side=Right;Offset={X=float(symbol.Compo.W);Y=float(symbol.Compo.H)/3.0};SideIndex= -1});("O1", {Side=Top;Offset={X=float(symbol.Compo.W)-30.0;Y=0.0};SideIndex= -1})]

    let rotatePortMap' (map:Map<string,PortOrientationOffset>) (symbol:Symbol) =
        map |> Map.map (fun key port ->
            match port.Side with
            |Right -> {Side=Bottom;Offset={X=port.Offset.Y;Y=port.Offset.X};SideIndex= -1}
            |Bottom -> {Side=Left;Offset={X=0.0;Y=port.Offset.X};SideIndex= -1}
            |Left -> {Side=Top;Offset={X=port.Offset.Y;Y=port.Offset.X};SideIndex= -1}
            |Top -> {Side=Right;Offset={X=float(symbol.Compo.W);Y=port.Offset.X};SideIndex= -1}
        ) //MUX + ADDER requires special treatment
    
    match symbol.Compo.Type with
    |Mux2 -> rotatePortMapMux map symbol
    |NbitsAdder _ -> rotatePortMapAdder map symbol
    |MergeWires |SplitWire _ |Custom _ -> map
    |_ -> rotatePortMap' map symbol 




let flipHPortMap (map:Map<string,PortOrientationOffset>) (symbol:Symbol) =
     map |> Map.map (fun key port ->
         match port.Side with
         |Right -> {port with Side=Left}
         |Left -> {port with Side=Right}
         |_ -> port)

let flipVPortMap (map:Map<string,PortOrientationOffset>) (symbol:Symbol) =
     failwithf "Not implemented Yet"

let changePortSide (map:Map<string,PortOrientationOffset>) (portName: string) (newSide:PortOrientation) (symbol:Symbol) = 
    let rev map: Map<string,string> = 
        Map.fold (fun m key value -> m.Add(value,key)) Map.empty map

    let namesInv = rev (portNamesMap symbol.Compo)

    let portId = Map.find portName namesInv

    let portTarget = Map.find portId map
    let currentSide = portTarget.Side
    let prevSideIndex = portTarget.SideIndex
    if (currentSide = newSide) then
        let temp = 
            Map.map (fun key port ->
                match port.Side with
                |a when a=currentSide -> (if port.SideIndex < prevSideIndex then {Side=port.Side;Offset=port.Offset;SideIndex=port.SideIndex+1} else {Side=port.Side;Offset=port.Offset;SideIndex=port.SideIndex})
                |_ -> port
            ) map
        temp |> Map.change portId (fun x ->
            match x with
            | Some s -> Some {Side=newSide;Offset=s.Offset;SideIndex= 0}   //CHECK
            | None -> None
        )
        // map |> Map.change portId (fun x ->
        //     match x with
        //     | Some s -> Some {Side=newSide;Offset=s.Offset;SideIndex= -1}   //CHECK
        //     | None -> None
        // )
    else 
        let temp = map |> Map.map (fun key port ->
            match port.Side with
            |a when a=currentSide -> (if port.SideIndex > prevSideIndex then {Side=port.Side;Offset=port.Offset;SideIndex=port.SideIndex-1} else {Side=port.Side;Offset=port.Offset;SideIndex=port.SideIndex})
            |b when b=newSide -> {Side=port.Side;Offset=port.Offset;SideIndex=port.SideIndex+1}
            |_ -> port
        )
        temp |> Map.change portId (fun x ->
            match x with
            | Some s -> Some {Side=newSide;Offset=s.Offset;SideIndex= 0}   //CHECK
            | None -> None
        )

let getSides (map:Map<string,PortOrientationOffset>) =
     let lst = Map.toList map
     let sides = lst |> List.map (fun x -> 
         match x with
         |(a,{Side=b;Offset=c;SideIndex=d}) -> b
     )
     sides |> List.map (fun x -> 
         match x with 
         |Right->"R"
         |Left->"L"
         |Top->"T"
         |Bottom->"B"
     ) 

let countsides (map:Map<string,PortOrientationOffset>) =
     let sides = getSides map
     let countmap = List.countBy id sides |> Map.ofList
     let counts = [Map.tryFind "R" countmap; Map.tryFind "B" countmap; Map.tryFind "L" countmap; Map.tryFind "T" countmap]
     let t = counts |> List.map (fun x ->
         match x with 
         |Some a -> a
         |None -> 0
         )
     // t
     (t[0],t[1],t[2],t[3])


let getMaxPortNameLength (map:Map<string,string>) =
     let labelList = List.map (snd >> String.length) (map |> Map.toList)
     if List.isEmpty labelList then 0 //if a component has no inputs or outputs list max will fail
     else List.max labelList

 //find max name.length in all ports, not input + output
 //height is defined by (max_ton_ports_se_Right_kai_left + 2) => gap gia port names on top/bottom 
 //width = max_name_size + max_name_size + (max_name_size) * max_ton_ports_se_top_kai_bottom 
 //////////   LEFT           RIGTH                TOP/BOTTOM
 //Rearrange map  

 //otan allazei ena port thelei -> let symbol' = {symbol with map = changeportside} -> let symbol'' = {redefineCustomHW} -> {symbol'' with map = redefineportmap}

let redefineCustomHW symbol = 
     // let altercomponent comp nh nw = 
     //     {comp with
     //         H = nh
     //         W = nw}
     let namesMap = portNamesMap symbol.Compo
     let maxname = getMaxPortNameLength namesMap
     let r,b,l,t = countsides symbol.APortOffsetsMap
     let maxRL = max r l
     let maxTB = max t b
     let heightNew = (GridSize + GridSize * maxRL)
    //  printf "%A" maxname
     let widthN = max (maxname*14 + symbol.Compo.Label.Length*10) (maxname*14 + maxname*maxTB*7) //find width required to fit everything
    //  printf "%A" widthNew
     let widthNew = max widthN (GridSize * 4)  //ensure minimum width if names too small
     let newcompo = {symbol.Compo with H = heightNew}
     let newcompo'= {newcompo with W = widthNew}
     {symbol with Compo = newcompo'}


let customOffsetHelper w h side sideIndex r l b t : XYPos= 
        let all =
             match side with
             |Right -> r
             |Left -> l
             |Top -> t
             |Bottom -> b
        let gap = 1.0 
        let offY = (float(h))* (( float(sideIndex) + gap )/( float( all ) + 2.0*gap - 1.0))  // the ports are created so that they are equidistant 
        let offX = (float(w))* (( float(sideIndex) + gap )/( float( all ) + 2.0*gap - 1.0))
        // {X=50.0;Y=50.0}
        match side with
            |Left -> {X=0.0;Y=offY}
            |Top -> {X=offX;Y=0.0}
            |Right -> {X=float(w);Y=offY}
            |Bottom -> {X=offX;Y=float(h)}

let redefineCustomPorts symbol (map:Map<string,PortOrientationOffset>) : Map<string,PortOrientationOffset> =
    let keys = map |> Map.toList |> List.map fst
    let values = map |> Map.toList |> List.map snd
    let r,b,l,t = countsides map
    let w,h = symbol.Compo.W, symbol.Compo.H //it needs the new height and width here based on spec above
    // let mutable valuesNew = []
    // for v in values do
        // valuesNew <- (valuesNew @ [{Side=v.Side;Offset=(customOffsetHelper w h v.Side v.SideIndex r l b t);SideIndex= v.SideIndex}])
        // printf $"IL: %i{il}"
    let valuesNew = List.map (fun v -> {Side=v.Side;Offset=(customOffsetHelper w h v.Side v.SideIndex r l b t);SideIndex= v.SideIndex}) values
    // printf $"New Y Offset: %f{valuesNew[0].Offset.Y}"    

//    for i in valuesNew do
  //      printf $"New Y Offset: %f{i.Offset.Y}"
    // valuesNew
    (keys, valuesNew) ||> List.map2 (fun x y -> (x,y)) |> Map.ofList
    // map






//----------------------------------------ROTATION HELPERS-----------------------------------------------
///Symbol Points rotation function
let rotatePoints points rotation = 
    let rotate90 points = 
        match points with
        |[x1;y1;x2;y2;x3;y3] -> [y1;x1;y2;x2;y3;x3]   //constant
        |[x1;y1;x2;y2;x3;y3;x4;y4] -> [y1;x1;y2;x2;y3;x3;y4;x4]  //all basic symbols
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5] ->[y1;x1;y5;x5;y4;x4;y3;x3;y2;x2]  //input,output,wire viewer
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5;x6;y6] -> [y6;x6;y5;x5;y4;x4;y3;x3;y2;x2;y1;x1]  //Wire label
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5;x6;y6;x7;y7;x8;y8] -> [y1;x1;y8;x8;y7;x7;y6;x6;y5;x5;y4;x4;y3;x3;y2;x2] //bus selector/comparator
        |_->points

    let rotate180 points = 
        match points with
        |[x1;y1;x2;y2;x3;y3] -> [x2+x2;y1;x1+x2;y2;x2+x2;y3]  //constant
        |[x1;y1;x2;y2;x3;y3;x4;y4] -> [x1;y2;x2;y1;x3;y4;x4;y3] //all basic symbols 
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5] ->[x3-x2;y1;x3;y2;x3;y4;x3-x2;y5;x1;y3] //input,output, wire viewer
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5;x6;y6] -> [x1;y1;x2;y2;x3;y3;x4;y4;x5;y5;x6;y6] //wire label
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5;x6;y6;x7;y7;x8;y8] -> [x1;y4;x4-x3;y3;x4-x2;y2;x4;y1;x5;y8;x5-x7;y7;x5-x6;y6;x8;y5] //bus selector/comparator
        |_->points

    let rotate270 points = 
        match points with
        |[x1;y1;x2;y2;x3;y3] -> [x2/2.0;y3;x2;y3+y3;x1;y3+y3]  //constant
        |[x1;y1;x2;y2;x3;y3;x4;y4] -> [y2;x1;y3;x4;y4;x3;y1;x2] //all basic symbols
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5] ->[y3;0.0;y5;x3-x2;y4;x3;y1;x3;y2;x3-x2] //input,output, wire viewer
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5;x6;y6] -> [y6;x6;y5;x5;y4;x4;y3;x3;y2;x2;y1;x1] //wire label
        |[x1;y1;x2;y2;x3;y3;x4;y4;x5;y5;x6;y6;x7;y7;x8;y8] -> [y5;x8;y6;x5-x6;y7;x5-x7;y8;x4;y1;x5;y1;x4-x2;y3;x4-x3;y4;x1] //bus selector/comparator
        |_->points
    
    match rotation with
    |R0 -> points
    |R90 -> rotate90 points
    |R180 -> rotate180 points
    |R270 -> rotate270 points



//------------------------------DRAWING SYMBOL HELPERS-------------------------------------

///convert list of points to a concatenated string to use when drawing ReactElements 
let getPointsString coordList =
    let rec combine lst =
        match lst with
        | fst::snd::tl -> (fst.ToString()+",") :: (snd.ToString()+" ") :: (combine tl)
        |[_] -> []
        |[] -> []
    combine coordList |> String.concat ""


/// Text adding function with many parameters (such as bold, position and text)
let private addText posX posY name txtPos weight size=
    let text =
            {defaultText with TextAnchor = txtPos; FontWeight = weight; FontSize = size}
    [makeText posX posY name text]

///Draw all ports contained in the APortOffsetMap
let DrawPorts (portMap: Map<string,PortOrientationOffset>) o (showInput:bool) (showOutput:bool) (symbol: Symbol) : ReactElement List=
    if Map.isEmpty portMap then []
    else
        let mid = portMap |> Map.map (fun key port ->
            match key,port with
            |k,{Side=side;Offset={X=x;Y=y}} -> if (k[0] = 'I' && showInput) || (k[0]='O' && showOutput) then makeCircle x y portCircle else nothing 
        )
        mid |> Map.toList |> List.map snd

///Draw the corresponding text of all ports contained in APortOffsetMap
let DrawPortsText (portMap:Map<string,PortOrientationOffset>) (comp: Component) symbol orientation = 
    let namesM = portNamesMap comp
    if (Map.isEmpty portMap || Map.isEmpty namesM) then []
    else
        let inline charToInt c = int c - int '0'
        let mid = portMap |> Map.map (fun key port ->
            let name = namesM[key]   //TODO: Add error handling with Tryfind key
            match port.Side with
            |Right-> (addText (port.Offset.X-5.0) (port.Offset.Y-7.0) name "end" "normal" "12px")[0]    //[0] to extract react element from list
            |Bottom -> (addText (port.Offset.X) (port.Offset.Y-20.0)  name "Middle" "normal" "12px")[0]  //they are added in a list at the end
            |Left-> (addText (port.Offset.X+5.0) (port.Offset.Y-7.0) name "start" "normal" "12px")[0]  
            |Top -> (addText (port.Offset.X) (port.Offset.Y+7.0)  name "Middle" "normal" "12px")[0]        )
        mid |> Map.toList |> List.map snd


let private createPolygon points colour opacity = 
    [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity}]

let createBiColorPolygon points colour strokeColor opacity strokeWidth= 
    if strokeColor <> "black" then 
        [makePolygon points {defaultPolygon with Fill = colour; Stroke = strokeColor; FillOpacity = opacity; StrokeWidth=strokeWidth}]
    else   
        [makePolygon points {defaultPolygon with Fill = colour; FillOpacity = opacity; StrokeWidth = strokeWidth}]


let drawClock w h rotation colour opacity = 
    let clocktext w h rotation = 
        match rotation with
        |R0 -> addText (10.0) (h-13.0) " clk" "start" "normal" "12px"
        |R90 -> addText (8.0) (10.0) " clk" "middle" "normal" "12px"
        |R180 -> addText (w-10.0) (0.0) " clk" "end" "normal" "12px"
        |R270 -> addText (h-8.0) (w-20.0) " clk" "middle" "normal" "12px"

    let clockpoints = 
        match rotation with
        |R0 -> [0.0;h-1.0;8.0;h-7.0;0.0;h-13.0]
        |R90 -> [(1.0);(0.0);(7.0);(8.0);(13.0);(0.0)]
        |R180 -> [w;(1.0);w-8.0;(7.0);w;(13.0)]
        |R270 -> [(h-1.0);w;(h-7.0);(w-8.0);(h-13.0);w] 
    
    createPolygon (getPointsString clockpoints) colour opacity
    |> List.append (clocktext w h rotation)


let addInvertor w h rotation colour opacity =
    let points = 
        match rotation with 
        |R0 -> [w;h/2.0;w+9.0;h/2.0;w;h/2.0-8.0]
        |R90 -> [w/2.0;h;w/2.0;h+9.0;w/2.0+8.0;h]
        |R180 -> [0.0;h/2.0;(-9.0);h/2.0;0.0;h/2.0+8.0]
        |R270 -> [w/2.0;0.0;w/2.0;(-9.0);w/2.0-8.0;0.0]
    createPolygon (getPointsString points) colour opacity


let addConstantLine w h rotation opacity = 
    let points =
        match rotation with
        |R0 -> [w/2.0;h/2.0;w;h/2.0]
        |R90 -> [w/2.0;h/2.0;w/2.0;h]
        |R180 -> [w/2.0;h/2.0;0.0;h/2.0]
        |R270 -> [w/2.0;h/2.0;w/2.0;0.0]
    createPolygon (getPointsString points) "lightgray" opacity


let drawGateType w h comp = 
    let gType = getTitle comp
    let posX,posY =
        match comp.Type with
        |_ -> (w/2.0), (h/2.0 - 7.0)
    addText posX posY gType "middle" "bold" "14px"


let outlineColor (color:string) =
    match color.ToLower() with
    | "lightgray" | "lightgrey" -> "black"
    | c -> 
        printfn $"color={color}"
        c

let addHorizontalColorLine posX1 posX2 posY opacity (color:string) = // TODO: Line instead of polygon?
    let points = (sprintf "%i,%f %i,%f" posX1 posY posX2 posY)
    let olColor = outlineColor color
    [makePolygon points {defaultPolygon with Fill = "olcolor"; Stroke=olColor; StrokeWidth = "2.0"; FillOpacity = opacity}]

    
let drawLabel label width height rotation =  
        match rotation with
        |R0 -> addText (width/2.0) (-20.0) (label) "middle" "normal" "16px"
        |R90 -> addText (width+5.0) (height/2.0 - 8.0) label "Start" "normal" "16px"
        |R180 -> addText (width/2.0) (height+10.0) (label) "middle" "normal" "16px"
        |R270 -> addText (-5.0) (height/2.0 - 8.0) label "End" "normal" "16px"


/// --------------------------------------- SYMBOL DRAWING ------------------------------------------------------ ///   
let drawSymbol (symbol:Symbol) (comp:Component) (colour:string) (showInputPorts:bool) (showOutputPorts:bool) (opacity: float)= 

    let h = float(comp.H)  //height
    let w = float(comp.W)  //width

    let rotation = symbol.STransform

    let hR,wR = match symbol.STransform with |R90|R270 -> w,h |_ -> h,w     //new height,width after rotation 
    // let hR,wR = h,w

    let mergeSplitLine posX1 posX2 posY msb lsb =
        let text = 
            match msb = lsb, msb >= lsb with
            | _, false -> ""
            | true, _ -> sprintf $"({msb})"
            | false, _ -> sprintf $"({msb}:{lsb})"
        addHorizontalColorLine posX1 posX2 (posY*float(h)) opacity colour @
        addText (float (posX1 + posX2)/2.0) (posY*float(h)-11.0) text "middle" "bold" "9px"

    // Points that specify each symbol 
    let points =            
        match comp.Type with
        | Input _ -> rotatePoints [0.0; 0.0; (w*0.66); 0.0; w; h/2.0; (w*0.66); h; 0.0; h] rotation
        | Constant1 _ -> rotatePoints [0.0; 0.0; (w/2.0); (h/2.0); 0.0; h] rotation
        | IOLabel -> rotatePoints [(w*0.33); 0.0; (w*0.66); 0.0; w; (h/2.0); (w*0.66); h; (w*0.33); h; 0.0; (h/2.0)] rotation
        | Output _ -> rotatePoints [0.0; 0.0; (w*0.66); 0.0; w; (h/2.0); (w*0.66); h; 0.0; h] (stransform_fsm(stransform_fsm rotation comp.Type) comp.Type) //hack for rotation to work -> same as input but rotated twice
        | Viewer _ -> rotatePoints [0.0; 0.0; (w*0.8); 0.0; w; (h/2.0); (w*0.8); h; 0.0; h] (stransform_fsm(stransform_fsm rotation comp.Type) comp.Type) //hack for rotation to work -> same as input (resized) but rotated twice
        | MergeWires -> [(w/2.0); ((1.0/6.0)*h); (w/2.0); ((5.0/6.0)*h)]  //add it to rotatePoints function when implemented
        | SplitWire _ -> [(w/2.0); ((1.0/6.0)*h); (w/2.0); ((5.0/6.0)*h); 0.0] //add it to rotatePoints function when implemented
        | Demux2 -> rotatePoints [0.0; (h*0.2); w; 0.0; w; h; 0.0; (h*0.8)] rotation
        | Mux2 -> rotatePoints [0.0; 0.0; w; (h*0.2); w; (h*0.8); 0.0; h] rotation 
        // EXTENSION: |Mux4|Mux8 ->(sprintf "%i,%i %i,%f  %i,%f %i,%i" 0 0 w (float(h)*0.2) w (float(h)*0.8) 0 h )
        // EXTENSION: | Demux4 |Demux8 -> (sprintf "%i,%f %i,%f %i,%i %i,%i" 0 (float(h)*0.2) 0 (float(h)*0.8) w h w 0)
        | BusSelection _ |BusCompare _ -> rotatePoints [0.0; 0.0; (0.6*w); 0.0; (0.8*w); (0.3*h); w; (0.3*h); w; (0.7*h); (0.8*w); (0.7*h); (0.6*w); h; 0.0; h] rotation
        | Custom _ -> [0.0; 0.0; w; 0.0; w; h; 0.0; h]
        | _ -> rotatePoints [0.0; 0.0; w; 0.0; w; h; 0.0; h] rotation
    
    // Helper function to add certain characteristics on specific symbols (inverter, enables, clocks)
    let extras =       
        match comp.Type with
        | Constant1 (_,_,txt) -> (addConstantLine (float(wR)) (float(hR)) rotation opacity @ addText (float (float(w)/2.0)-5.0) (float(h)-8.0) txt "middle" "normal" "12px") 
        | Nand | Nor | Xnor |Not -> (addInvertor (float(wR)) (float(hR)) rotation colour opacity)
        | MergeWires -> 
            let lo, hi = 
                match symbol.InWidth0, symbol.InWidth1  with 
                | Some n, Some m  -> n, m
                | _ -> -1,-1
            let msb = hi + lo - 1
            let midb = lo
            let midt = lo - 1
            mergeSplitLine 0 (int(w/2.0)) (1.0/6.0) midt 0 @ 
            mergeSplitLine 0 (int(w/2.0)) (5.0/6.0) msb midb @ 
            mergeSplitLine (int(w/2.0)) (int(w)) 0.5 msb 0
        | SplitWire mid -> 
            let msb, mid' = match symbol.InWidth0 with | Some n -> n - 1, mid | _ -> -100, -50
            let midb = mid'
            let midt = mid'-1
            mergeSplitLine (int(w/2.0)) (int(w)) (1.0/6.0) midt 0 @ 
            mergeSplitLine (int(w/2.0)) (int(w)) (5.0/6.0) msb midb @ 
            mergeSplitLine 0 (int(w/2.0)) 0.5 msb 0
        | DFF |DFFE -> (drawClock w h rotation colour opacity)
        | Register _ |RegisterE _ -> (drawClock w h rotation colour opacity)
        | ROM1 _ |RAM1 _ | AsyncRAM1 _ -> (drawClock w h rotation colour opacity)
        | BusSelection(x,y) -> (addText  (wR/2.0) ((hR/2.7)-2.0) (bustitleGen x y) "middle" "normal" "12px")
        | BusCompare (_,y) -> (addText  (wR/2.0) ((hR/2.7)-1.0) ("=" + NumberHelpers.hex(int y)) "middle" "bold" "10px")
        | Input (x) -> (addText  (wR/2.0) (hR/3.0) (titleGen "" x) "middle" "normal" "12px")
        | Output (x) -> (addText  (wR/2.0) ((float(hR)/3.0)) (titleGen "" x) "middle" "normal" "12px")
        | Viewer (x) -> (addText  (w/2.0) ((float(h)/2.7)-1.25) (titleGen "" x) "middle" "normal" "9px")  
        | _ -> []

    let borderColour, strokeWidth =
        match comp.Type with
        | SplitWire _ | MergeWires -> outlineColor colour, "2.0"
        | _ -> "black", "1.0"
   
    // Put everything together 
    (DrawPorts symbol.APortOffsetsMap symbol.STransform showInputPorts showOutputPorts symbol)       //Ports
    |> List.append (DrawPortsText symbol.APortOffsetsMap comp symbol symbol.STransform)              //PortsText
    |> List.append (drawGateType (float(wR)) (float(hR)) comp)                                          //Symbol Title - e.g. N-Bits-Adder(3..0)
    |> List.append (extras)                                                                             //extra components required for specific symbol - e.g. Clock invertor etc.
    |> List.append (drawLabel comp.Label (float(wR)) (float(hR)) symbol.STransform)                     //Label 
    |> List.append (createBiColorPolygon (getPointsString points) colour borderColour opacity "1.0")    //actual symbol

let init () = 
    { Symbols = Map.empty; CopiedSymbols = Map.empty; Ports = Map.empty ; InputPortsConnected= Set.empty ; OutputPortsConnected = Map.empty}, Cmd.none

//----------------------------View Function for Symbols----------------------------//
type private RenderSymbolProps =
    {
        Symbol : Symbol 
        Dispatch : Dispatch<Msg>
        key: string 
    }

/// View for one symbol. Using FunctionComponent.Of to improve efficiency (not printing all symbols but only those that are changing)
let private renderSymbol =
    
    FunctionComponent.Of(
        fun (props : RenderSymbolProps) ->
            let symbol = props.Symbol
            let ({X=fX; Y=fY}:XYPos) = symbol.Pos
            g ([ Style [ Transform(sprintf "translate(%fpx, %fpx)" fX fY) ] ]) (drawSymbol props.Symbol props.Symbol.Compo symbol.Colour symbol.ShowInputPorts symbol.ShowOutputPorts symbol.Opacity)
            
        , "Symbol"
        , equalsButFunctions
        )
    
/// View function for symbol layer of SVG
let getSymbols map =
    let listMoving = 
        Map.filter (fun _ sym -> not sym.Moving) map
        |>Map.toList
        |>List.map snd
    let listNotMoving =
        Map.filter (fun _ sym -> sym.Moving) map
        |>Map.toList
        |>List.map snd
    listMoving @ listNotMoving


let view (model : Model) (dispatch : Msg -> unit) = 
    let start = TimeHelpers.getTimeMs()
    model.Symbols
    |> getSymbols
    |> List.map (fun ({Id = ComponentId id} as symbol) ->
        renderSymbol
            {
                Symbol = symbol
                Dispatch = dispatch
                key = id
            }
    )
    |> ofList
    |> TimeHelpers.instrumentInterval "SymbolView" start


//---------------------------------------------------------------------------------//
//--------------------AP1919 CODE SECTION ENDS-------------------------------------//
//---------------------------------------------------------------------------------//





//------------------------GET BOUNDING BOXES FUNCS--------------------------------used by sheet.
// Function that returns the bounding box of a symbol. It is defined by the height and the width as well as the x,y position of the symbol
let getBoundingBoxofSymbol (sym:Symbol): BoundingBox =
    match sym.STransform with
    | R0 | R180 -> {X = float(sym.Pos.X) ; Y = float(sym.Pos.Y) ; H = float(sym.Compo.H) ; W = float(sym.Compo.W)}
    | R90 | R270 -> {X = float(sym.Pos.X) ; Y = float(sym.Pos.Y) ; H = float(sym.Compo.W) ; W = float(sym.Compo.H)}
let getBoundingBoxes (symModel: Model): Map<ComponentId, BoundingBox> =
    Map.map (fun _ (sym:Symbol) -> (getBoundingBoxofSymbol sym)) symModel.Symbols
let getOneBoundingBox (symModel: Model) (compid: ComponentId ): BoundingBox =
    getBoundingBoxofSymbol (Map.find compid symModel.Symbols)


//--------------------- GETTING PORTS AND THEIR LOCATIONS INTERFACE FUNCTIONS-------------------------------
// Helpers
let getSymbolPos (symbolModel: Model) compId =
    let symbol = Map.find compId symbolModel.Symbols
    symbol.Pos

// Function to generate Port Positions of each port from Symbol Location and Orientation
// Input:  Symbol -> Take the symbol for which to extract positions of ports
// Output: XYPos list -> Return a list of XYPos positions of the ports starting from inputs, anticlockwise.
let canvasPortLocation (sym:Symbol) : XYPos list =
    sym.APortOffsetsMap
    |> Map.toList
    |> List.map snd
    |> List.map (fun i -> i.Offset)
    |> List.map (fun i -> {X=i.X+sym.Pos.X;Y=i.Y + sym.Pos.Y})

// Function to generate the true XYPos of a specified Port on the Canvas given the port and symbol
// Input:  Symbol, Port -> Take the symbol and the specified port
// Output: XYPos-> Return the XYPos position of the ports depending on being inputs or outputs
let getGlobalPortPos (sym: Symbol) (port:Port) : (PortOrientation*XYPos) = 
    let typePort,ports =
        if port.PortType = PortType.Input then
            ("I",sym.Compo.InputPorts)
        else 
            ("O",sym.Compo.OutputPorts)
    let index = float( List.findIndex (fun (p:Port)  -> p = port) ports )
    
    let positionOffset = (Map.find (typePort + string index) sym.APortOffsetsMap)
    (positionOffset.Side,positionOffset.Offset)
     

/// It is used in getInputPortLocation for a single port
let getInputPortsPositionMap (symbols: Symbol list)  = 
    symbols
    |> List.collect (fun sym -> List.map (fun p -> sym,p) sym.Compo.InputPorts)
    |> List.map (fun (sym,port) -> (InputPortId port.Id,((fst (getGlobalPortPos sym port)), posAdd (snd (getGlobalPortPos sym port)) sym.Pos)))
    |> Map.ofList


/// It is used in getOutputPortLocation for a single port
let getOutputPortsPositionMap (symbols: Symbol list)  = 
    symbols
    |> List.collect (fun sym -> List.map (fun p -> sym,p) sym.Compo.OutputPorts)
    |> List.map (fun (sym,port) -> (OutputPortId port.Id ,((fst (getGlobalPortPos sym port)), posAdd (snd (getGlobalPortPos sym port)) sym.Pos)))
    |> Map.ofList

///Returns the port object associated with a given portId
let getPort (symModel: Model) (portId: string) =
    symModel.Ports[portId]

///Returns all the port locations of the given components   
let getPortLocations (symbolModel: Model) (sIds: ComponentId list) = 
    let getSymbols = 
        symbolModel.Symbols 
        |> Map.filter (fun sId _  -> List.contains sId sIds)
        |> Map.toList
        |> List.map snd
        
    let getInputPortMap = getInputPortsPositionMap getSymbols
    let getOutputPortMap = getOutputPortsPositionMap getSymbols
       
    getInputPortMap , getOutputPortMap

///Returns the location of an input portId  
let getInputPortLocation (model:Model) (portId: InputPortId)  = 
    let allSymbols =
        model.Symbols
        |> Map.toList
        |> List.map snd
        
    getInputPortsPositionMap allSymbols 
    |> Map.find portId
    

//Returns the location of an output portId
let getOutputPortLocation (model:Model) (portId : OutputPortId) =
    let allSymbols =
        model.Symbols
        |> Map.toList
        |> List.map snd
        
    getOutputPortsPositionMap allSymbols 
    |> Map.find portId

///Returns the location of a given portId
let getOnePortLocation (symModel: Model) (portId : string) (pType: PortType)=
        match pType with
        | PortType.Input ->
            getInputPortLocation symModel (InputPortId portId)
        | PortType.Output ->
            getOutputPortLocation symModel (OutputPortId portId)

/// Returns the location of a given portId, with better efficiency
let getOnePortLocationNew (symModel: Model) (portId : string) (pType: PortType): PortOrientation * XYPos =
    symModel.Symbols
    |> Map.pick (fun _ sym -> 
        let comp = sym.Compo
        if pType = PortType.Input then
            List.tryFind (fun (po:Port) -> po.Id = portId) comp.InputPorts
        else
            List.tryFind (fun (po:Port) -> po.Id = portId) comp.OutputPorts
        |> Option.map (fun port ->((fst (getGlobalPortPos sym port)), posAdd (snd (getGlobalPortPos sym port)) sym.Pos)))


/// Returns the locations of a given input portId and output portId 
let getTwoPortLocations (symModel: Model) (inPortId: InputPortId ) (outPortId: OutputPortId) =
    match inPortId, outPortId with
    | InputPortId inputId, OutputPortId outputId ->
        (getOnePortLocationNew symModel inputId PortType.Input, getOnePortLocationNew symModel outputId PortType.Output)

/// Interface function to get componentIds of the copied symbols
let getCopiedSymbols (symModel: Model) : ComponentId list =
    symModel.CopiedSymbols
    |> Map.toList
    |> List.map fst

/// Function to filter out terminal non-letter characters.
/// Modified to capitalise labels
let filterString (string: string) = 
    string.ToUpper()
    |> Seq.rev
    |> Seq.skipWhile System.Char.IsDigit
    |> Seq.rev
    |> Seq.map System.Char.ToString
    |> String.concat ""
   
///Returns the number of the component label (i.e. the number 1 from IN1 or ADDER16.1)
let regex (str : string) = 
    let index = Regex.Match(str, @"\d+$")
    match index with
    | null -> 0
    | _ -> int index.Value

let getCompList compType listSymbols =
    match compType with 
       | Not | And | Or | Xor | Nand | Nor | Xnor -> 
            listSymbols
            |> List.filter (fun sym ->
                (sym.Compo.Type = Not || sym.Compo.Type = And 
                || sym.Compo.Type = Or || sym.Compo.Type = Xor
                || sym.Compo.Type = Nand || sym.Compo.Type = Nor
                || sym.Compo.Type = Xnor)
                )
       | DFF | DFFE -> 
            listSymbols
            |> List.filter (fun sym ->
                (sym.Compo.Type = DFF || sym.Compo.Type = DFFE))
       //The following components require this pattern matching in order to correctly identify all of the components in the circuit of that type
       //Normally this is because they are defined by a width as well as a type
       | Register _ | RegisterE _ ->
            listSymbols
            |> List.filter (fun sym ->
                match sym.Compo.Type with 
                | Register _ | RegisterE _ -> true
                | _ -> false)
       | Constant1 _ ->
            listSymbols
            |> List.filter (fun sym ->
                match sym.Compo.Type with 
                | Constant1 _ -> true
                | _ -> false)
       | Input _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Input _ -> true
               | _ -> false)
       | Output _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Output _ -> true
               | _ -> false)
       | Viewer _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | Viewer _ -> true
               | _ -> false)
       | BusSelection _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | BusSelection _ -> true
               | _ -> false)
       | BusCompare _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | BusCompare _ -> true
               | _ -> false)
       | NbitsAdder _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | NbitsAdder _ -> true
               | _ -> false)
       | NbitsXor _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | NbitsXor _ -> true
               | _ -> false)
       | AsyncROM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | AsyncROM1 _ -> true
               | _ -> false)
       | ROM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | ROM1 _ -> true
               | _ -> false)
       | RAM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | RAM1 _ -> true
               | _ -> false)
       | AsyncRAM1 _ ->
           listSymbols
           |> List.filter (fun sym ->
               match sym.Compo.Type with 
               | AsyncRAM1 _ -> true
               | _ -> false)

       | _ ->
            listSymbols
            |> List.filter (fun sym -> sym.Compo.Type = compType)

let getIndex listSymbols compType =
    let symbolList = 
        getCompList compType listSymbols

    match compType with
    | MergeWires | SplitWire _ -> ""
    | _ ->
        if List.isEmpty symbolList then 1 
        else symbolList
            |> List.map (fun sym -> regex sym.Compo.Label)
            |> List.max
            |> (+) 1
        |> string

///Generates the number to be put in the title of symbols
let labelGenNumber (model: Model) (compType: ComponentType) (label : string) = 
    let listSymbols = List.map snd (Map.toList model.Symbols) 
    match compType with
    | IOLabel -> label
    | _ -> filterString label + (getIndex listSymbols compType)

///Generates the label for a component type
let generateLabel (model: Model) (compType: ComponentType) : string =
    labelGenNumber model compType (getLabel compType)

/// Interface function to paste symbols. Is a function instead of a message because we want an output
/// Currently drag-and-drop
let pasteSymbols (symModel: Model) (mPos: XYPos) : Model * ComponentId list =
    let createNewSymbol (basePos: XYPos) ((currSymbolModel, pastedIdsList) : Model * ComponentId List) (oldSymbol: Symbol) : Model * ComponentId List =
        let newId = JSHelpers.uuid()
        let posDist = posDiff oldSymbol.Pos basePos
        let newPos = posAdd posDist mPos
        
        let pastedSymbol =
            { oldSymbol with
                Id = ComponentId newId
                Compo = createComponent newPos oldSymbol.Compo.Type newId (labelGenNumber { symModel with Symbols = currSymbolModel.Symbols } oldSymbol.Compo.Type oldSymbol.Compo.Label) R0// TODO: Change label later
                Pos = newPos
                ShowInputPorts = false
                ShowOutputPorts = false }
             
        let newSymbolMap = currSymbolModel.Symbols.Add ((ComponentId newId), pastedSymbol) // List needs to be in this order
        let newPorts = addPortsToModel currSymbolModel pastedSymbol
        { currSymbolModel with Symbols = newSymbolMap; Ports = newPorts }, pastedIdsList @ [ pastedSymbol.Id ]
        
    let oldSymbolsList =
        symModel.CopiedSymbols
        |> Map.toList
        |> List.map snd

    match List.sortBy (fun sym -> sym.Pos.X) oldSymbolsList with
    | baseSymbol :: _ ->
        let basePos = posAdd baseSymbol.Pos { X = (float baseSymbol.Compo.W) / 2.0; Y = (float baseSymbol.Compo.H) / 2.0 }
        
        ((symModel, []), oldSymbolsList) ||> List.fold (createNewSymbol basePos)
    | [] -> symModel, []

    
/// Given two componentId list of same length and input / output ports that are in list 1, return the equivalent ports in list 2.
/// ComponentIds at same index in both list 1 and list 2 need to be of the same ComponentType
/// CompIds1 need to be in model.CopiedSymbols
let getEquivalentCopiedPorts (model: Model) (copiedIds) (pastedIds) (InputPortId copiedInputPort, OutputPortId copiedOutputPort) =
    let findEquivalentPorts compId1 compId2 =
        let copiedComponent = model.CopiedSymbols[compId1].Compo
        let pastedComponent = model.Symbols[compId2].Compo // TODO: These can be different for an output gate for some reason.
        
        let tryFindEquivalentPort (copiedPorts: Port list) (pastedPorts: Port list) targetPort =
            if copiedPorts.Length = 0 || pastedPorts.Length = 0
            then None
            else
                match List.tryFindIndex ( fun (port: Port) -> port.Id = targetPort ) copiedPorts with
                | Some portIndex -> 

                    Some pastedPorts[portIndex].Id // Get the equivalent port in pastedPorts. Assumes ports at the same index are the same (should be the case unless copy pasting went wrong).
                | _ -> None
        
        let pastedInputPortId = tryFindEquivalentPort copiedComponent.InputPorts pastedComponent.InputPorts copiedInputPort
        let pastedOutputPortId = tryFindEquivalentPort copiedComponent.OutputPorts pastedComponent.OutputPorts copiedOutputPort
    
        pastedInputPortId, pastedOutputPortId
        
    let foundPastedPorts =
        List.zip copiedIds pastedIds
        |> List.map (fun (compId1, compId2) -> findEquivalentPorts compId1 compId2)
    
    let foundPastedInputPort = List.collect (function | Some a, _ -> [a] | _ -> []) foundPastedPorts
    let foundPastedOutputPort = List.collect (function | _, Some b -> [b] | _ -> []) foundPastedPorts
    
    match foundPastedInputPort, foundPastedOutputPort with 
    | [pastedInputPort], [pastedOutputPort] -> Some (pastedInputPort, pastedOutputPort) 
    | _ -> None // If either of source or target component of the wire was not copied then we discard the wire
  
 
/// Given a model return a model with a new Symbol and also the component id
let addSymbol (model: Model) pos compType lbl rotation=
    let newSym = createNewSymbol pos compType lbl rotation
    let newSymbolWithMap = {newSym with APortOffsetsMap = (genAPortOffsets newSym newSym.Compo.Type)} 
    let newPorts = addPortsToModel model newSymbolWithMap
    let newSymModel = Map.add newSymbolWithMap.Id newSymbolWithMap model.Symbols
    { model with Symbols = newSymModel; Ports = newPorts }, newSymbolWithMap.Id

// Helper function to change the number of bits expected in a port of each component type
let changeNumberOfBitsf (symModel:Model) (compId:ComponentId) (newBits : int) =
    
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
        | Input _ -> Input newBits
        | Output _ -> Output newBits
        | Viewer _ -> Viewer newBits
        | NbitsAdder _ -> NbitsAdder newBits
        | NbitsXor _ -> NbitsXor newBits
        | Register _ -> Register newBits
        | RegisterE _ -> RegisterE newBits
        | SplitWire _ -> SplitWire newBits
        | BusSelection (_,b) -> BusSelection (newBits,b)
        | BusCompare (_,b) -> BusCompare (newBits,b)
        | Constant1 (_,b,txt) -> Constant1 (newBits,b,txt)
        | c -> c

    let newcompo = {symbol.Compo with Type = newcompotype}
    {symbol with Compo = newcompo}

// Helper function to change the number of bits expected in the LSB port of BusSelection and BusCompare
let changeLsbf (symModel:Model) (compId:ComponentId) (newLsb:int64) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
        | BusSelection (w, _) -> BusSelection (w, int32(newLsb))
        | BusCompare (w, _) -> BusCompare (w, uint32(newLsb)) 
        | Constant1(w, _,txt) -> Constant1 (w, newLsb,txt)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Compo with Type = newcompotype}
    {symbol with Compo = newcompo}

let changeConstantf (symModel:Model) (compId:ComponentId) (constantVal:int64) (constantText: string) =
    let symbol = Map.find compId symModel.Symbols
    let newcompotype = 
        match symbol.Compo.Type with
        | Constant1 (w, _, _) -> Constant1 (w, constantVal,constantText)
        | _ -> failwithf "this shouldnt happen, incorrect call of message changeLsb"
    let newcompo = {symbol.Compo with Type = newcompotype}
    printfn "Changing symbol to: %A" newcompotype
    {symbol with Compo = newcompo}
    
/// update function which displays symbols
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | DeleteSymbols compList ->
        let newSymbols = List.fold (fun prevModel sId -> Map.remove sId prevModel) model.Symbols compList
        { model with Symbols = newSymbols }, Cmd.none //filters out symbol with a specified id

    | AddSymbol (pos,compType, lbl) ->
        let newModel, _ = addSymbol model pos compType lbl R0
        newModel, Cmd.none

    | CopySymbols compIds ->
        let copiedSymbols = Map.filter (fun compId _ -> List.contains compId compIds) model.Symbols
        { model with CopiedSymbols = copiedSymbols }, Cmd.none

    | ShowAllInputPorts ->
        { model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = true; ShowOutputPorts = false}) model.Symbols },
        Cmd.none

    | ShowAllOutputPorts ->
        {model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = true}) model.Symbols },
        Cmd.none

    | DeleteAllPorts ->
        { model with Symbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = false}) model.Symbols },
        Cmd.none //demo

    | ShowPorts compList -> //show ports of one component (shown in demo for a random component, sheet gives list in group phace)  find showPorts in other interfaces (above)
        let resetSymbols = Map.map (fun _ sym -> {sym with ShowInputPorts = false; ShowOutputPorts = false}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with ShowInputPorts = true; ShowOutputPorts = true} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | MoveSymbols (compList, move) -> 
        let resetSymbols = Map.map (fun _ sym -> { sym with Moving = false}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId ->
                let (newCompo: Component) = {model.Symbols[sId].Compo with X = int (model.Symbols[sId].Pos.X + move.X);Y = int (model.Symbols[sId].Pos.Y + move.Y )}
                Map.add sId {model.Symbols[sId] with Moving = true; Pos = {X = (model.Symbols[sId].Pos.X + move.X);Y = (model.Symbols[sId].Pos.Y + move.Y)} ; Compo = newCompo} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | SymbolsHaveError compList ->
        let resetSymbols = Map.map (fun _ sym -> {sym with Colour = "Lightgray"}) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none

    | SelectSymbols compList -> //select a symbol (shown in demo for a random component, sheet gives list in group phase)
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols) resetSymbols compList)
        { model with Symbols = newSymbols }, Cmd.none  

    | RotateSymbols compList -> //select a symbol to Rotate
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let newSymbols = 
            // if ctrl is pressed make yellow initially, then try to change STransform for every time ctrl+R is pressed
            List.fold (fun prevSymbols sId ->
                let compo = model.Symbols[sId].Compo
                // let hR,wR = match stransform_fsm(model.Symbols[sId].STransform) with |R90|R270 -> compo.W,compo.H |_ -> compo.H,compo.W 
                let newcompo = {compo with R = stransform_fsm model.Symbols[sId].STransform compo.Type ;}
                Map.add sId {model.Symbols[sId] with Compo = newcompo ; STransform = stransform_fsm model.Symbols[sId].STransform compo.Type ; APortOffsetsMap = rotatePortMap model.Symbols[sId].APortOffsetsMap model.Symbols[sId]} prevSymbols) resetSymbols compList
        { model with Symbols = newSymbols }, Cmd.none
    //////////////PENDING///////////////////
    
    // | FlipHSymbols compList -> // NEW: flip a symbol Horizontally
    //     let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
    //     let newSymbols =
    //         // The selected symbol is rotated by incrementing Stransform rotation and updating new APortOffsetsMap and Symbol Pos 
    //         List.fold (fun prevSymbols sId ->
    //             Map.add sId {model.Symbols[sId] with STransform = stransform_fsm(stransform_fsm model.Symbols[sId].STransform); APortOffsetsMap = flipHPortMap model.Symbols[sId].APortOffsetsMap model.Symbols[sId]} prevSymbols) resetSymbols compList
    //     { model with Symbols = newSymbols }, Cmd.none
    
    // | FlipVSymbols compList ->
    //     let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
    //     let newSymbols =
    //         // The selected symbol is rotated by incrementing Stransform rotation and updating new APortOffsetsMap and Symbol Pos 
    //         List.fold (fun prevSymbols sId ->
    //             Map.add sId {model.Symbols[sId] with STransform = stransform_fsm(stransform_fsm(model.Symbols[sId].STransform)); APortOffsetsMap = flipVPortMap model.Symbols[sId].APortOffsetsMap model.Symbols[sId]} prevSymbols) resetSymbols compList
    //     { model with Symbols = newSymbols }, Cmd.none

    | FlipHSymbols compList -> // NEW: flip a symbol Horizontally
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let newSymbols =
            List.fold (fun prevSymbols sId ->
                Map.add sId model.Symbols[sId] prevSymbols) resetSymbols compList  //NEED TO DO APPROPRIATE CHANGES HERE, SEE ROTATION FOR INSPIRATION
        { model with Symbols = newSymbols }, Cmd.none
    
    | FlipVSymbols compList ->
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let newSymbols =
            List.fold (fun prevSymbols sId ->
                Map.add sId model.Symbols[sId] prevSymbols) resetSymbols compList  //NEED TO DO APPROPRIATE CHANGES HERE, SEE ROTATION FOR INSPIRATION
        { model with Symbols = newSymbols }, Cmd.none

    | ErrorSymbols (errorCompList,selectCompList,isDragAndDrop) -> 
        let resetSymbols = Map.map (fun _ sym ->  { sym with Colour = "Lightgray"; Opacity = 1.0 }) model.Symbols
        let selectSymbols =
            List.fold (fun prevSymbols sId -> 
                            if not isDragAndDrop then 
                                Map.add sId {resetSymbols[sId] with Colour = "lightgreen"} prevSymbols
                            else 
                                Map.add sId { resetSymbols[sId] with Opacity = 0.2 } prevSymbols
                        ) resetSymbols selectCompList
        let newSymbols = 
            (List.fold (fun prevSymbols sId -> Map.add sId {resetSymbols[sId] with Colour = "Red"} prevSymbols) selectSymbols errorCompList)
        { model with Symbols = newSymbols }, Cmd.none 
        
    | MouseMsg _ -> model, Cmd.none // allow unused mouse messags

    | ChangeLabel (sId, newLabel) ->
        let tempsym = Map.find sId model.Symbols
        let newcompo = {tempsym.Compo with Label = newLabel}
        let addsym = {tempsym with Compo = newcompo}
        { model with Symbols = Map.add sId addsym model.Symbols }, Cmd.none
    | ChangePort (sId, portName, portSide) ->
        let extract map =
            let lst = map |> Map.toList
            lst |> List.map (fun x -> 
                match x with
                |(a,{Side=b;Offset=c;SideIndex=d}) -> ((orientationEncoder b),d)
            )
        let tempsym = Map.find sId model.Symbols
        printf $"Selected Port: %s{portName}"
        printf $"Selected Side: %s{portSide}"
        let newSide =
            match portSide with
            | "Top"    -> Top
            | "Bottom" -> Bottom
            | "Left"   -> Left
            | "Right"  -> Right
            | _ -> failwithf "Side not implemented"

        let symbol' = {tempsym with APortOffsetsMap = (changePortSide tempsym.APortOffsetsMap portName newSide tempsym)}
        let symbol'' = redefineCustomHW symbol'
        let symbol'''  = {symbol'' with APortOffsetsMap = redefineCustomPorts symbol'' symbol'.APortOffsetsMap}
        let newcompo = {symbol'''.Compo with SI=extract symbol'''.APortOffsetsMap}
        let symbol'''' = {symbol''' with Compo = newcompo}
        { model with Symbols = Map.add sId symbol'''' model.Symbols }, Cmd.none

    | PasteSymbols compList ->
        let newSymbols =
            (List.fold (fun prevSymbols sId -> Map.add sId { model.Symbols[sId] with Opacity = 0.4 } prevSymbols) model.Symbols compList)
        { model with Symbols = newSymbols }, Cmd.none  
    
    | ColorSymbols (compList, colour) -> 
        let newSymbols = 
            Map.map (fun sId sym -> if List.contains sId compList then {sym with Colour = string colour} else sym) model.Symbols
        { model with Symbols = newSymbols }, Cmd.none 
    
    | ChangeNumberOfBits (compId, newBits) ->
        let newsymbol = changeNumberOfBitsf model compId newBits
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ChangeLsb (compId, newLsb) -> 
        let newsymbol = changeLsbf model compId newLsb
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none

    | ChangeConstant (compId, newVal, newText) -> 
        let newsymbol = changeConstantf model compId newVal newText
        let symbolswithoutone = model.Symbols.Remove compId
        let newSymbolsWithChangedSymbol = symbolswithoutone.Add (compId, newsymbol)
        { model with Symbols = newSymbolsWithChangedSymbol }, Cmd.none
    
    | ResetModel -> { model with Symbols = Map.empty; Ports = Map.empty }, Cmd.none
    
    | LoadComponents comps ->
        let findrotatedportmap symbol rotation =
            let once = rotatePortMap (genAPortOffsets symbol symbol.Compo.Type) symbol
            let twice = rotatePortMap once symbol
            let th = rotatePortMap twice symbol
            match rotation with
            |R0 ->  genAPortOffsets symbol symbol.Compo.Type
            |R90 -> once
            |R180 -> twice
            |R270 -> th

        let totalsides sidesString =
            let countmap = List.countBy id sidesString |> Map.ofList
            let counts = [Map.tryFind "R" countmap; Map.tryFind "B" countmap; Map.tryFind "L" countmap; Map.tryFind "T" countmap]
            let t = counts |> List.map (fun x ->
                match x with 
                |Some a -> a
                |None -> 0
                )
            // t
            (t[0],t[1],t[2],t[3])

        let reconstructCustomPortMap lst map w h =
            let keys = map |> Map.toList |> List.map fst
            let sides = List.map (fun (side,index)->side) lst
            let sidesString = 
                sides |> List.map (fun x ->
                    match x with
                    |0 -> "R"
                    |1 -> "B"
                    |2 -> "L"
                    |3 -> "T"
                    |_ -> "R"
                    ) 
            
            let r,b,l,t = totalsides sidesString
            // let w,h = symbol.Compo.W, symbol.Compo.H //it needs the new height and width here based on spec above

            let valuesNew = List.map (fun (side,index) -> {Side=(orientationDecoder side);Offset=(customOffsetHelper w h (orientationDecoder side) index r l b t);SideIndex= index}) lst

            (keys, valuesNew) ||> List.map2 (fun x y -> (x,y)) |> Map.ofList

        let compIdsWithSymbols =
            comps
            |> List.map ( fun comp -> (
                                        let xyPos = {X = float comp.X; Y = float comp.Y}
                                        let (h,w) =
                                            if comp.H = -1 && comp.W = -1 then
                                                let comp' = createComponent xyPos comp.Type comp.Id comp.Label R0
                                                comp'.H,comp'.W
                                            else
                                                comp.H, comp.W
                                        ComponentId comp.Id,
                                        let s = { Pos = xyPos;
                                           STransform = comp.R;
                                           ShowInputPorts = false ;//do not show input ports initially
                                           ShowOutputPorts = false ;//do not show output ports initially
                                           Colour = "lightgrey"     ;// initial color 
                                           Id = ComponentId comp.Id;
                                           Compo = {comp with H=h ; W = w};
                                           Opacity = 1.0;
                                           Moving = false;
                                           InWidth0 = None;
                                           InWidth1 = None;
                                           APortOffsetsMap = (Map.empty<string,PortOrientationOffset>)
                                         }
                                        match comp.SI with 
                                        |[] -> {s with APortOffsetsMap = (findrotatedportmap s s.STransform)}   //SI is empty for all other components except custom + when custom has not been altered
                                        |_ -> {s with APortOffsetsMap = reconstructCustomPortMap comp.SI (findrotatedportmap s R0) comp.W comp.H}   //if custom has been altered, reconstruct based on SI member of component 
                                        ))
        let symbolList =
            compIdsWithSymbols
            |> List.map snd

        let symbolMap =
            compIdsWithSymbols   
            |> Map.ofList
        
        let folder currModel sym =
            { currModel with Ports = addPortsToModel currModel sym }
            
        let newModel = ( model, symbolList ) ||> List.fold folder
        { newModel with Symbols = symbolMap }, Cmd.none
 
    | WriteMemoryLine (compId, addr, value) ->
        let symbol = model.Symbols[compId]
        let comp = symbol.Compo
        
        let newCompType =
            match comp.Type with
            | RAM1 mem -> RAM1 { mem with Data = Map.add addr value mem.Data }
            | AsyncRAM1 mem -> AsyncRAM1 { mem with Data = Map.add addr value mem.Data }
            | ROM1 mem -> ROM1 { mem with Data = Map.add addr value mem.Data }
            | AsyncROM1 mem -> AsyncROM1 { mem with Data = Map.add addr value mem.Data }
            | _ -> comp.Type
        
        let newComp = { comp with Type = newCompType }
        
        let newSymbols = Map.add compId { symbol with Compo = newComp } model.Symbols
        
        { model with Symbols = newSymbols }, Cmd.none
    | WriteMemoryType (compId, memory) ->
        let symbol = model.Symbols[compId]
        let comp = symbol.Compo       
        let newCompType =
            match comp.Type with
            | RAM1 mem | AsyncRAM1 mem -> memory
            | ROM1 mem -> memory
            | AsyncROM1 mem -> memory
            | _ -> 
                printfn $"Warning: improper use of WriteMemoryType on {comp} ignored"
                comp.Type
        
        let newComp = { comp with Type = newCompType }
        
        let newSymbols = Map.add compId { symbol with Compo = newComp } model.Symbols
        
        { model with Symbols = newSymbols }, Cmd.none
        
// ----------------------interface to Issie----------------------------- //
let extractComponent (symModel: Model) (sId:ComponentId) : Component = 
    symModel.Symbols[sId].Compo

let extractSymbol (symModel: Model) (sId:ComponentId) : Symbol = 
    symModel.Symbols[sId]

let extractComponents (symModel: Model) : Component list =
    symModel.Symbols
    |> Map.toList
    |> List.map (fun (key, _) -> extractComponent symModel key)