# Features

## Buswire

 - Wire type
 - Render Wires (check for port facing down) ELSE NO
 - Radiused corners
 - Autorouting
 - Partial Autorouting (almost fine -> not working when input/output are at top/bottom)
 - No wire jumps because of radiused corners
 - Drag Segments NO

## Symbol

 - New Types: add extra info (Rotation, Orientation, New Component, AportOffsetMap)
 - Rotation: add info
 - Mux + Adder with ports on different edges
 - Clock on custom -> NO
 - APortOffsetMap: Map which uses as key the port name and value the PortOrientationOffset which consists of the Port Side Offset from top left corner and index from list of ports on that symbol side.
 - UI to rotate : This can be accessed by either pressing the keyboard combination SHIFT+R or accessing this through View>Rotate Symbol.
 - Move ports of custom : This can be accessed through the Symbol properties tab and selecting the port and preferred side to be moved to. 
 - Auto-align : Not implemented.
 - Bounding box : Bounding box height and width was changed to take into account the rotation of the symbol. This can be seen by rotating the symbol and testing the new bouding box.

