# README for individual code submission - GV319

## Admin and quick access links

Link to: 
[Common repo Team 6 file](https://github.com/tomcl/hlp22docs/blob/main/Team6.md)

Link to altered files:  
* [Symbol (Section 2)](src/Renderer/DrawBlock/Symbol.fs)  
* [Sheet (Section 2)](src/Renderer/DrawBlock/Sheet.fs)  
* [Renderer (Section 2)](src/Renderer/Renderer.fs)  

I am responsible for making the following changes in my code:
* __Symbol.fs:__ 
Includes changes to existing code for Section 2 and new functions implementing the extensions.
  * Lines : 718-1315 (Section 2)
  * Lines : 97-102 (stransform_fsm) 
* __Sheet.fs:__ 
Includes changes required to make extensions for rotation work.
  * Lines 86, 859-863 (Extensions)
* __Renderer.fs:__ 
Includes changes required to make extensions for UI rotation work.
  * Lines: 151 (Extensions)

## Analysis

### Types
The types decided in the first part of Symbol.fs were `STransform` of type `Rotation: R0, R90, R180, R270` and `PortOrientationOffset` of type {`PortOrientation: Right, Bottom, Left, Top`,`Offset: XYPos`} to be used to describe
the different rotations fo the symbol and the side of the symbol its ports are at each rotation.
`APortOffsetsMap` of type `Map<string,PortOrientationOffset>` was used to describe the port locations relative to the top left corner of the symbol.
This map uses string keys `I0..IN` and `O0..ON`, mapped in this order, to correctly distinguish and store the input and output port offsets in the map.

### Changes to pre-existing code


Overall small changes were made to the pre-existing code, as the main focus was the correct implementation
of the extensions and symbol enhancements and to ensure their full functionality.

Minor changes were made by removing unnecessary parentheses adding comments where necessary and refactoring the code where unnecessary arguments were passed to functions.  


#### changeLsbf, changeConstantf
Renamed to changeLSBbits and changeConstant respectively to avoid confusion of f being regarded as using an input of type float.

#### createCustomPortNamesMap
Removed parantheses and unnecessary use of input parameter n, as it wasn't used and so that it wasn't required when used.

#### getInputPortsPositionMap, getOutputPortsPositionMap,getPortLocations, getInputPortLocation, getOutputPortLocation
Since getInputPortsPositionMap, getOutputPortsPositionMap are now using getGlobalPortPos which is explained later on in the Extensions section the input parameter of model is not required but only the Symbol list.
Therefore, to avoid confusion and improve readability it was removed as a parameter and as input to the following function addressed above.

Apart from these changes the main focus was targeted at the Extensions of this Section due to their importance.

### Analysis of how/why code works
The code described below are the fully working implemented Symbol - enhancements for the Individual Coding of Section 2 ([Project Spec: Slide 6](https://intranet.ee.ic.ac.uk/t.clarke/hlp/lectures/project22-spec.pdf))

The code demonstrated in the 5 minute feedback will be to:
* Show different UI implementations to perform rotation for each symbol.
* Show how ports on different symbols are mapped with rotation to the different sides of the symbol.
* Show how the wiring between symbols is also altered with each rotation.
* Show that the bounding box of each symbol changes with each rotation for each symbol.

# Extensions
## 1. Locate port position and orientation, given symbol position and orientation
### Changes in files:
  - __Symbol.fs__
    - `canvasPortLocation` (Lines: 743-748)
    - `getGlobalPortPos` (Lines: 753-761)

### Explanation:
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

## 2. UI to rotate symbol
### Changes in files:
  - __Symbol.fs__
    - `RotateSymbols compList` (Lines: 1164-1173)

  - __Sheet.fs__
    - `Type KeyboardMsg Rotate` (Lines: 86)
    - `Keypress Rotate` (Lines: 859-863)


  - __Renderer.fs__
    - `makeItem "Rotate Symbol` (Lines: 151-152)
### Explanation:
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

## 3. Make symbol bounding box work with rotation
### Changes in files:
  - __Symbol.fs__
    - `getBoundingBoxofSymbol` (Lines: 724-727)
    - `getBoundingBoxes` (Lines: 728-729)
    - `getOneBoundingBox` (Lines: 730-731)

### Explanation:
The function `getBoundingBoxofSymbol` was updated to correctly alter the bounding box border of each symbol by taking into account
the current orientation of the symbol. This is done using the STransform passed from the Symbol as input of the function. This is
used as a match case where the bounding box is altered as `HxW` and `WxH` based on the current orientation of the symbol.
The height and weight of the symbol are passed to the border width and height in order or in reverse. This ensures that for every orientation
of the symbol, the correct border is used. The new `getBoundingBoxofSymbol` function is passed to the `getBoundingBoxes` and `getOneBoundingBox`
to be used by `Sheet.fs` to correctly display the Bounding box of the symbol at each orientation.  
This can be tested by selecting the symbol to be tested, then rotated and checking with another symbol whether the bounding box
correctly responds when disrupted.

The Symbol enhancements for section 2 described above are fully working without  producing any errors during build and runtime