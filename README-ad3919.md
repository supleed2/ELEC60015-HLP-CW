# ad3919 Individual Code Submission

## Admin and quick access links

[Team Contribution Doc](https://github.com/tomcl/hlp22docs/blob/main/Team6.md)

[BusWire (Section 1)](src/Renderer/DrawBlock/BusWire.fs#L224-L624)

Section 1 is lines 224 - 624 indicated using `/// ad3919 code ///`

## Individual Changes

- Adapter Functions
  - [getRISegEnd](src/Renderer/DrawBlock/BusWire.fs#L60)
    - Function to get segment.End from RISeg, needed in a couple of places when working with endpoint
  - [jumpDistToJumpCoord](src/Renderer/DrawBlock/BusWire.fs#L65)
  - [jumpCoordToJumpDist](src/Renderer/DrawBlock/BusWire.fs#L68)
  - [riSegToASeg](src/Renderer/DrawBlock/BusWire.fs#L71)
    - Uses jumpDistToJumpCoord
  - [aSegToRISeg](src/Renderer/DrawBlock/BusWire.fs#L87)
    - Uses jumpCoordToJumpDist
- segmentsToVertices
  - Renamed: [ASegsToVertices](src/Renderer/DrawBlock/BusWire.fs#L228)
  - List.mapi swapped for List.map as i is unused
  - Added [RISegsToVertices](src/Renderer/DrawBlock/BusWire.fs#L233)
- makeInitialWireVerticesList
  - Renamed: [initialWireVerticesFromPorts](src/Renderer/DrawBlock/BusWire.fs#L239)
  - Repeated creation of wire points moved into if statement so only 1 is made, comments reduced
- inferDirectionfromVertices
  - Renamed: [inferOrientationFromVertices](src/Renderer/DrawBlock/BusWire.fs#L285) (output is Orientation option)
    - Returns an Orientation, used to indicate whether leftToRight or rightToLeft ?????????
- xyVerticesToSegments
  - Renamed: [convertVerticesToASegs](src/Renderer/DrawBlock/BusWire.fs#L304)
  - Added [convertVerticesToRISegs](src/Renderer/DrawBlock/BusWire.fs#L331), needs to be replaced with native RISeg version (TODO)
- issieVerticesToSegments
  - Renamed: [issieVerticesToASegs](src/Renderer/DrawBlock/BusWire.fs#L337)
  - Added [issieVerticesToRISegs](src/Renderer/DrawBlock/BusWire.fs#L359), needs to be replaced with native RISeg version (TODO)
- [extractConnection](src/Renderer/DrawBlock/BusWire.fs#L368)
  - Reduce XML Comment size
  - Rename conn -> wire (is type BusWire.Wire)
- [extractConnections](src/Renderer/DrawBlock/BusWire.fs#L380)
  - Reduce XML Comment size
  - Rename key -> connectionId (mild readability improvement)
- getAbsXY
  - Renamed: [absXYPos](src/Renderer/DrawBlock/BusWire.fs#L386), is just abs for XYPos
  - Not sure why it's needed, very simple though
- onSegment
  - Only used within segmentIntersectsSegment, so changed to sub-function, then replaced
- orientation
  - Only used within segmentIntersectsSegment, so changed to sub-function, then replaced
- [segmentIntersectsSegment](src/Renderer/DrawBlock/BusWire.fs#L390)
  - Could be given inline keyword so it is placed within segmentIntersectsSegmentCoordinates by the compiler
  - Rewritten to be much shorter, using a simpler check for orientations, and checking that both segments are within the other
- makeSegPos
  - Not sure why it's needed, but it is easy to read and simple enough
  - Renamed: [makeASegPos](src/Renderer/DrawBlock/BusWire.fs#L423), Added [makeRISegPos](src/Renderer/DrawBlock/BusWire.fs#L428)
- distanceBetweenTwoPoints
  - No usages of this function in entire project / solution, removed to reduce code bloat
- makeInitialSegmentsList
  - Renamed: [makeInitialASegList](src/Renderer/DrawBlock/BusWire.fs#L432)
  - Added [makeInitialRISegList](src/Renderer/DrawBlock/BusWire.fs#L437), needs to be replaced with native RISeg version (TODO)
- [renderSegment](src/Renderer/DrawBlock/BusWire.fs#L443)
  - add halfWidth to `match _ with`, to allow for individual adjustment (slightly rounded corners for 1 bit wide wire)
  - `segmentJumpHorizontalSize`, `segmentJumpVerticalSize` -> `jumpSizeX`, `jumpSizeY`: reduce line length when used
  - [renderWireSubSegment](src/Renderer/DrawBlock/BusWire.fs#L458)
    - Replace cons `::` operator with direct list construction, remove syntactic noise, improves readability
  - [renderSingleSegmentJump](src/Renderer/DrawBlock/BusWire.fs#L470)
    - Replace cons `::` operator with direct list construction, remove syntactic noise, improves readability
  - [renderMultipleSegmentJumps](src/Renderer/DrawBlock/BusWire.fs#L482)
    - Fine as is, reduced whitespace and added short explanations
  - completeWireSegmentRenderFunction
    - Renamed: [renderWireSegment](src/Renderer/DrawBlock/BusWire.fs#L501)
    - Minor renaming to reduce line length + small comments
  - Used only in singleWireView, could add `inline`
    - Allows both segment types to type-check in different locations
    - Potential compiler performance improvements
- [WireRenderProps](src/Renderer/DrawBlock/BusWire.fs#L552)
  - Swapped from ASeg list to RISeg list, relevant functions (singleWireView and view) updated
- memoOf
  - No usages of this function in entire project / solution, removed to reduce code bloat
- [singleWireView](src/Renderer/DrawBlock/BusWire.fs#L561)
  - React.js Magic, designed for efficient caching
- MapToSortedList
  - No usages of this function in entire project / solution, removed to reduce code bloat
- [view](src/Renderer/DrawBlock/BusWire.fs#L588)
  - Uses Array over List as it deals with all the wires in the model, so this likely has a large performance impact
  - Times sections of the function
    - Replace instrumentInterval with instrumentTime when passthrough output is ()

## Analysis

### Bad Functions in Existing Code

- [inferDirectionfromVertices](src/Renderer/DrawBlock/BusWire.fs#L285)
  - Uses Orientation (`Horizontal` or `Vertical`) to represent whether wire is of 3 or 5 segment type, confusing when trying to understand the function
- segmentIntersectsSegment
  - Has separate functions `orientation` and `onSegment` which are not used anywhere else
    - Both of these require the comments to understand
  - Hard to follow flow as there are many similar lines with names that are too short to understand easily
  - Rewritten: [segmentIntersectsSegment](src/Renderer/DrawBlock/BusWire.fs#L390)
- renderSegment
  - Uses cons `::` operator unnecessarily in construction of ReactElement lists
