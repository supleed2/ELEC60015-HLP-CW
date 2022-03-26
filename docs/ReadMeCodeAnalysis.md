# Code Analysis Readme

## BusWire - Section 1

### 1. Add new auto-routing cases (Phillip?)

### 2. Add rotation invariant Segment type
(along with helper functions for converting between and using the two types interchangeably)

### 3. Refactor segment intersection detection function (could be skipped?)
(used when calculating jumps as well as when detecting if a click is within range of a segment)

### 4. Rounded corner segment rendering
- Rounded corners
  - 2 standalone functions, `renderRISegList` and `renderRISegAndCorner`
  - Can be easily swapped in place of `List.map renderSegment` within `singleWireView, renderWireSegmentList`
  - Renders only necessary segments, ignoring 0 length segments, either because they are covered by the rounded corners or are 0 length originally
  - Does not impact underlying position and click detection

## BusWire - Section 2

## BusWire - Section 3

## Symbol - Section 1

## Symbol - Section 2

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

### 2. UI to rotate symbol
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

### 3. Make symbol bounding box work with rotation
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


### 4. Custom Symbol Port Side Change UI
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

