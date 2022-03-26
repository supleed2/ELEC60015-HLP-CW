# Features

## Buswire

 - Wire type
 - Render Wires (check for port facing down) ELSE NO
 - Rounded corners
   - 2 standalone functions, `renderRISegList` and `renderRISegAndCorner`
   - Can be easily swapped in place of `List.map renderSegment` within `singleWireView, renderWireSegmentList`
   - Renders only necessary segments, ignoring 0 length segments, either because they are covered by the rounded corners or are 0 length originally
   - Does not impact underlying position and click detection
 - Autorouting
 - Partial Autorouting (almost fine -> not working when input/output are at top/bottom)
 - No wire jumps because of rounded corners
   - Code for calculating jumps on wire move can be disabled when rounded corners are enabled, possible performance improvement
 - Drag Segments NO

## Symbol

 - New Types: (Rotation, PortOrientation, PortOrientationOffset, New Component -> +R + SI , AportOffsetMap)
 - Rotation: All symbols can be rotated except for Split/MergeWire and Custom (since you can alter the ports' positions any way you like) 
 - Mux + Adder with ports on different edges
 - Clock on custom -> NO
 - APortOffsetMap: used for all drawing functions, for loading components, and allows easy change of ports' locations
 - UI to rotate : This can be accessed by either pressing the keyboard combination SHIFT+R or accessing this through View>Rotate Symbol.
 - Move ports of custom : This can be accessed through the Symbol properties tab and selecting the port and preferred side to be moved to. Also by selecting the same side a port currently is it will move it to be the first port in that side. 
 - Auto-align : Not implemented.
 - Bounding box : Bounding box height and width was changed to take into account the rotation of the symbol. This can be seen by rotating the symbol and testing the new bouding box.

