# Code Analysis Readme

## BusWire 

### 1. Add new auto-routing cases 

A new type called *routeType* has been created to pass the information between functions

1. Oppositeside - 3 / 5 Segments Wire (Original Issie implementation)

2. Sameside - 3 / 5 Segments Wire (Ports are at the same side)

3. Rightangle - 2 / 4 Segments Wire (Difference of Port Orientation = 90 degree)

type *Direction* determine the Wire rotation 

`makeInitialASegList`

1. Take the input and output Port position and Orientation from Symbol

2. Match the orientation of the two port to the routeType, the rotation and yreflect applied to the default cases of routing in the spec.

3. Find the location of the input port for the default cases by reversing the rotation and yreflect applied using relative location of input port to the output port

4. Get the Vertices from `initialWireVerticesFromPorts` and convert them to ASegs using `convertVerticesToASegs`

`initialWireVerticesFromPorts`

- Oppositeside contains the same implementation of the original Issie

- Sameside contains 3 conditions

  - Default three segments with endY >= startY

  - Three segments with endY < startY

  - Five segments where the endX is near with the startX

- Rightangle contrains 3 conditions

  - Default two segments that endY>startY and endX>startX

  - Four Segments that the implement equidistance Horizontal Segments where (startX + endX) / 2.0)>(startX+Wire.stickLength)

  - Four Segments that do not implement equidistance Horizontal Segments: it is used for the edge cases from the second condition where the first horizontal segments after the first stick would go into opposite direction of the stick and through the component. Although at a cost that some cases could still implement equidistance segments after the rotation would fall into this case with the restriction of the second condition.

  ![Weird RightAngle Case](/docs/img/Rightangle.png)

`convertVerticesToASegs`

1. Assign the orientation and Dragable and the Direction of the ASeg from the Vertices

2. Convert them to RISeg in order to rotate the segments back to original routing from default cases

3. Calculate the Start for each RISeg and convert back to ASeg
### 2. Add rotation invariant Segment type

#### Changes in files

- __BusWire.fs__
  - `Type RISeg` (Lines: 47-59)
  - `riSegEnd` (Lines: 62-68)
  - `jumpDistToJumpCoord` and `jumpCoordToJumpDist` (Lines: 71-76)
  - `riSegToASeg` and `aSegToRISeg` (Lines: 79-113)

#### Explanation

New segment type that is similar to the old `Segment` but stores the `Length` of the segment instead of the `End` position. The helper function `riSegEnd` allows for easy replacement of an RISeg where ASeg.End was needed.

The remaining helper functions allow for conversion between an ASeg and RISeg, eg. with `List.map` when some functions have been converted to use RISegs and some have not.

### 3. Refactor segment intersection detection function

#### Changes in files

- __BusWire.fs__
  - `segmentIntersectsSegment` (Lines: 735-765)

#### Explanation

Function `segmentIntersectsSegment` takes in the start and end `XYPos` of two segments and returns `true` if the segments intersect or `false` otherwise. The points are made positive, incase the sign of the position is being used to store data. The start and end of each segment is used to determine if the segment is Horizontal or Vertical, which is then compared. If both segments have the same orientation, they cannot intersect at a point and `false` is immediately returned. Next, the common coordinate of each segment (Y for a Horizontal segment, X for a Vertical segment) is checked against the other segment. If both common coordinates lie on or between the changing coordinate of the other segment, the segments must intersect, and the function returns `true`.

### 4. Rounded corner segment rendering

#### Changes in files

- __BusWire.fs__
  - `renderRISegAndCorner` (Lines: 829-885)
  - `renderRISegList` (Lines: 888-900)
  - `singleWireView` (Lines: 918) *To use renderRISegList inplace of renderSegment*

#### Explanation

Segments with `Length = 0` are filtered out, as they do not need to be considered when rendering, and would impact the calculation of which corners to round later. Then the remaining segments are reindexed, so that the indexes of segments are consecutive again. Segments are then zipped (`zip3`) as contextual information about the 2 connecting segments are needed when rendering a segment, eg. the `Index` and `Length` of the connecting segments.

__Here, `Seq.zip3` is used instead of `List.zip3` as the former allows for uneven length sequences to be zipped, by truncating the longer sequences, rather than throwing `System.ArgumentException`. This avoids an expensive operation to remove the last element of `riSegs` when `dummyStartRISeg` is prepended.__

The presence of start and end rounded corners is calculated next. If the adjoining segment is too short, or if there is no connecting segment (ie. the current segment is the start / end), there is no rounded corner. If a rounded corner is present, the respective coordinates (start / end) of the segment are adjusted to meet the `svg path` of the rounded corner, and the path is drawn using a Bezier Curve, where the second control point lies on the end coordinate of the path.

## Symbol - Section 1

### 0. New Types

- `Rotation`: DU with values `R0 | R90 | R180 | R270` used to determine the rotation of each symbol (defaults to R0)
- `PortOrientation`: DU with values `Rigth | Bottom | Left | Top` used to determine on which side of a symbol a port is located
- `PortOrientationOffset {Side : PortOrientation, Offset : XYPos, SideIndex : int}`: Contains all the information required for each port -> Which side it is on, its offset from the top-left corner of the symbol and the order of the port on its side 
- `APortOffsetMap`: A map from each port to its main information (PortOrientationOffset). The keys are strings generated as follows: "I" for input / "O" for output followed by the number of the port (e.g. a complete key is "I0" or "01"). `APortOffsetMap` is used for all drawing functions, for loading components, and allows easy change of ports' locations. (See also subsection 'APortOffsetMap Helpers')
- `Component` was altered and 2 extra fields were added to it: `R : Rotation` which stands for Rotation and `SI : (int*int) List` which stands for SideIndex Information. `R` defaults to R0 and is used to store the rotation of a symbol so that it can be loaded correctly upon reload. `SI` defaults to empty and it is only changed when ports' sides/order of custom components are changed.  


### 1. Locate port position and orientation, given symbol position and orientation
#### Changes in files:
- __Symbol.fs__
    - `canvasPortLocation` (Lines: 743-748)
    - `getGlobalPortPos` (Lines: 753-761)

#### Explanation:
Functions `canvasPortLocation` and `getGlobalPortPos` were created to produce the "Global" Coordinates of the ports
for each symbol at any given rotation and position. `canvasPortLocation` extracts the `APortOffsetsMap` of the specified
symbol and returns a list of the global XYPos ports of the symbol by combining the offsets with the top-left corner of the symbol.
These coordinates are shown by toggling Developer tools in Issie and are outputted every time a selected symbol is rotated
showing its global coordinates in the order described in Types. Keep in mind the coordinates shown are those before the symbol is rotated.  
Similar to `canvasPortLocation`, `getGlobalPortPos` is used to generate the "Global" XYPos of a specified Port on the Canvas
given the port and symbol specified. Since, the `APortOffset` map and `STransform` are part of the Symbol type, the entire symbol is passed to the function,
rather than these separately. After categorizing the port passed to the function as type input or output, the appropriate index of the port is extracted
and is used as a key to extract the correct coordinates of the port to display and to be used by buswire.fs. Since the `APortOffsetMap` is updated
at every rotation (discussed in later extensions), `getGlobalPortPos` will always receive a new `APortOffsetMap` at every orientation and thus
not require the current orientation as input. This can be tested by rotating a symbol and connecting another symbol's ports to its ports.
It can be observed that the gray circles used to distinguish which ports connection can be made are correctly displayed on the rotated symbol, and
the buswire correctly connects to that rotated port location. However, two issues seem to arise. The wire is connected parallel to the
port, if the symbol is rotated as the `buswire.fs` is expecting the symbol to be non-rotated. Additionally, the `buswire.fs` seems to only
update when the symbol is moved. As a result, a communication between the rotations and orientations of the symbol need to be passed
to `buswire.fs` to ensure that the wires are correctly displayed during rotation. This will be done later on in the group work.

### 2. New View function and Helpers to work with Symbol Rotation

#### Explanation:
Most of the functionality is implemented within the `Rotation Helpers` section (lines 586-620), `APortOffsetMap Helpers` section (lines 365-468) and `Drawing Helpers` section (lines 624-745) so please refer to the Helpers explanation as well.
The rotation helpers are used as follows: When a symbol is added/moved/pasted/loaded/rotated:
- The initial point coordinates of the symbol pass through the rotate points function which returns the correct coordinates based on the rotation, which are then drawn in the canvas.
- The `APortOffsetMap` is modified by the `rotatePortMap` function to produce the correct map based on the rotation, which are then drawn at the correct locations
- All other information of a symbol (title,label,etc.) is drawn using the Drawing Helpers which also take into account the current rotation of a symbol.   

#### APortOffsetMap Helpers

* The `genAPortOffsets` function is the one that return the map with the ports. It consists of three sub functions: genAPortOffsetsMux, `genAPortOffsetsAdder`, `genAPortOffsets'`. The `genAPortOffsets'` is the one that creates the map for all the components that have their ports on the LHS/RHS (without rotation). It is quite similar with the previous implementation of this functionality, using the functions `offsethelper` (find XYPos) and `portListToMap` (map creation). The other two functions exist as MUX and Adder require different treatment. This is because they have ports on other edges as well. Because of this the Map for these two symbols is created manually.
* This implementation also makes it easy to improve port symbols in the future. Simply add a generator for the component we want to change its ports.
* The `rotatePortMap` function does exactly what its name suggests: it alters the Map so that the ports are on the correct position based on rotation. Again it has 3 subfunctions, one for MUX, one for Adder, and One for everything else. The reason the Mux one is required is because the select port is not on the bounding box (full w or full h), but at h*0.9, and thus it doesn't work with the normal rotation function. The reason the adder one is required is because apart from making Cin and Cout appear on bottom/top, I have moved them towards the corners (see it in Issie) instead of them being at width/2, in order to make the symbol readable no matter the rotation. In the match case of this function we see also a `|MergeWires |SplitWire _ |Custom _ -> map`. This is because rotation for this components hasn't been implemented yet, and thus, it returns the map without any alterations. 


#### Rotation Helpers

* The rotate function takes as an input the initial points of each symbol and its rotation and returns the points of the rotated symbol. It consists of 3 sub-functions: `rotate90`, `rotate180`, `rotate270` which all have 6 different cases (3-point,4-point,5-point,6-point,8-point symbols, and all other symbols not implemented yet), each of which returns the correct points based on which the symbol will be drawn. 
* This implementation SHOULD work for other symbols added in the future as well (3,4,5,6,8-point) and is easy to improve if a 7-point symbol for example was added.

#### Drawing Helpers

* All functions here have been altered to work with the current rotation of the symbol.
* Ports and their text are added based on APortOffsetsMap
* The function `getStringPos` converts a list of floats (points) to the string required to draw each symbol and its sub-parts.
* All other additions (clock,inverter...) now take into account the rotation as well.
* The label is rotating with the symbol so that it is always on a 'port-free' side.
* The title is added in the center of the symbol so that the ports' text is readable no matter how the symbol is rotated.   



### 3. UI to rotate symbol
#### Changes in files:
- __Symbol.fs__
    - `RotateSymbols compList` (Lines: 1164-1173)

- __Sheet.fs__
    - `Type KeyboardMsg Rotate` (Lines: 86)
    - `Keypress Rotate` (Lines: 859-863)


- __Renderer.fs__
    - `makeItem "Rotate Symbol` (Lines: 151-152)
#### Explanation:
The UI was altered to incorporate the use of symbol rotation. Thus was done by first altering the `Renderer.fs`. Line 151 was added
to add the menu item "Rotate Symbol" in order to be able to rotate any symbol selected. This option was then linked through `Sheet.fs`
This functionality can be found on the menu bar under "View". Further implementation was added to rotation by adding a keyboard shortcut.
This was done in line 86 of `Sheet.fs` where the KeybrdMsg "Rotate" was added to implement this. Lines 859-863 were added to link
the menu item and keyboard shortcut "Shift+R" to Symbol.RotateSymbols in `Symbol.fs` to rotate the symbol selected.  
The case "RotateSymbols" was added to the "update" function used in `Symbol.fs` to update the symbols on the canvas. As seen in lines
"1164-1173" a new map of symbols, to be displayed and were existing on the canvas, is created where the symbol with the specified id
found after being seelcted, is changed by incrementing its STransform value to the next Rotation using stransform_fsm and
updating the `APortOffsetMap` using RotatePortMap provided by Section 1. Finally, the model Symbols are replaced by these new symbols.
As a result, the selected symbol is updated with its 90 deg rotated version. Worth noting is the addition of the `canvasPortLocation`
function in this case to print the global port locations as explained previously.  
The above can be fully tested by selecting a symbol on the canvas and pressing either `Shift+R` or `View > Rotate Symbol`.
The "Global" port coordinates of the selected symbol will also appear in the console if Developer Tools are toggled on.

### 4. Make symbol bounding box work with rotation
#### Changes in files:
- __Symbol.fs__
    - `getBoundingBoxofSymbol` (Lines: 724-727)
    - `getBoundingBoxes` (Lines: 728-729)
    - `getOneBoundingBox` (Lines: 730-731)

#### Explanation:
The function `getBoundingBoxofSymbol` was updated to correctly alter the bounding box border of each symbol by taking into account
the current orientation of the symbol. This is done using the STransform passed from the Symbol as input of the function. This is
used as a match case where the bounding box is altered as `HxW` and `WxH` based on the current orientation of the symbol.
The height and weight of the symbol are passed to the border width and height in order or in reverse. This ensures that for every orientation
of the symbol, the correct border is used. The new `getBoundingBoxofSymbol` function is passed to the `getBoundingBoxes` and `getOneBoundingBox`
to be used by `Sheet.fs` to correctly display the Bounding box of the symbol at each orientation.  
This can be tested by selecting the symbol to be tested, then rotated and checking with another symbol whether the bounding box
correctly responds when disrupted.

The Symbol enhancements for section 2 described above are fully working without  producing any errors during build and runtime

### 5. Custom symbols' port movement

The ports' change side and rearrangement functionality of custom symbols is implemented in the following code snippet (lines 1362-1387), using the `changePortSide`, `redefineCustomHW`, `redefineCustomPortsOffset` helpers which are in lines 475-585.  

```
ChangePort (sId, portName, portSide) ->
        
    //extract info required from Map to store in SI field of Component
    let extractToSI map =
        let lst = map |> Map.toList
        lst |> List.map (fun x -> 
            match x with
            |(a,{Side=b;Offset=c;SideIndex=d}) -> ((orientationEncoder b),d)
        )

    let targetSymbol = Map.find sId model.Symbols

    let newSide =
        match portSide with
        | "Top"    -> Top
        | "Bottom" -> Bottom
        | "Left"   -> Left
        | "Right"  -> Right
        | _ -> failwithf "Undefined Side"

    //change ports' Side and SideIndex fields
    let symbol' = {targetSymbol with APortOffsetsMap = (changePortSide targetSymbol.APortOffsetsMap portName newSide targetSymbol)}  

    //change Height Width based on new sides (total ports on each side)
    let symbol'' = redefineCustomHW symbol'  
    
    //change Offset of each port in the map based on new Height Width Side and SideIndex
    let symbol'''  = {symbol'' with APortOffsetsMap = redefineCustomPortsOffset symbol'' symbol'.APortOffsetsMap}  
    
    //add the new Side and SideIndex fields in the SI field of component to store the information
    let newcompo = {symbol'''.Compo with SI=extractToSI symbol'''.APortOffsetsMap} 
    let symbol'''' = {symbol''' with Compo = newcompo}   
    
    { model with Symbols = Map.add sId symbol'''' model.Symbols }, Cmd.none
```

#### CustomPortMovement Helpers

- `changePortSide` returns the updated APortOffsetMap which will have the newSide in the side field of the port the user has chosen to change, as well as the updated SideIndex fields in all affected ports by the spe
- `redefineCustomHW` calculates the new Height/Width the custom symbol needs to have so that no text overlaps and there is enough space for all the ports based on the total number of ports on each side as well as the length of the largest port name. It returns the same symbol but with the updated `Symbol.Compo.W` `Symbol.Compo.H` values 
- `redefineCustomPortsOffset` uses the information produced by `changePortSide` and `redefineCustomHW` to calculate the updated Offset fields of all ports on the symbol.

### 6. Custom Symbol Port Side Change UI
#### Changes in files:

- __SelectedComponentView.fs__
    - `viewSelectedComponent` (Lines: 388-486)
- __PopupView.fs__
    - `setComponentPortUpdate` (Lines: 93-96)
- __Sheet.fs__
    - `member this.ChangePort` (Lines: 173-175)
- __Symbol.fs__
    - `ChangePort` (Lines: 94, 1418-1441)

#### Explanation:
The function `viewSelectedComponent` was changed to include a Division field with a list of the ports available to be changed depending on the current symbol selected and a list of the available sides to be moved. In order to move the symbol the port needed to be changed and side selected the most recently changed port on a specific side would be indexed first (ports whose side selected was the same as before would be moved to 1st index). By pressing the submit button the specific change of side will be made. This call is made through `PopupView.fs` of the `setComponentPortUpdate` function responsible for passing the call to the sheet in order to dispatch this change to `Symbol.fs` and `Buswire.fs`. This dispatches are made by the member `this.ChangePort` accessed from `PopupView.fs` were the `UpdateWires` function is called in order and in `Symbol.fs` the Update function `ChangePort` is selected where the construction of a new symbol whose specified port is moved to the side requested is built and made to replace the previous symbol with the specified id passed from `SelectedComponentView.fs`.


### 7. Correct Loading of components

Here is where the new `R` and `SI` fields of component are used. As explaned in subsection 0 (New Types), these fields default to R0 and empty list []. Whenever we change either the Rotation of normal symbols or we alter the port locations of custom symbols we also save the relevant information to `R`, `SI`, `H` and `W` so that on reload, the symbols will appear exactly as they were before. 

For the loading of symbols 3 new Helper functions were created: `findrotatedportmap` and `reconstructCustomPortMap`.

- `findrotatedportmap` is used by normal symbols and it works as follows: The `APortOffsetMap` is generated using the APortOffsetMap Helpers and then is rotated X times, where X depends on the saved rotation (R0,R90,R180,R270).  
- `reconstructCustomPortMap` is used by custom symbols only. It re-constructs the `APortOffsetMap` of custom symbols using the information stored in the `SI` field. The procedure it follows is similar to `redefineCustomPortsOffset` function analysed in subsection 5. It calculates the total number of ports in each side and then uses this information along with the height and width to calculate the offset of each port. Based on all this information it re-constructs the APortOffsetMap which is then used to draw all ports and ports' text on the canvas. SI is a bit different and requires further explanation. What we store in `SI` is an `(int*int) List` whose size is equal to the number of ports of each symbol. Its first element has the Side and SideIndex of port "I0", its second element the Side and SideIndex of port "I1" and so on. As you can see its type is `(int*int) List` instead of `(Rotation*int) List`. When we store and load components we use a encoder/decoder created (lines 113-129) to transform Side (Rotation -> int -> Rotation)
