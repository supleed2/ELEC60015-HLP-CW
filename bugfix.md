# Post Demo Bugfixes

- yhp19
  - Fixing the Rightangle (2-4 Segment) case for the Autorouting algorithms. Equidistance is attempted whenever is possible (more explanation in the code analysis)
- ad3919
  - with help of yhp19 in understanding routing code
  - Dragging does not work correctly in some instances
    - Opposite Side, (standard Issie) works as expected
    - Same Side, forces wire between two ports (as if it was opposite side) instead of only away from ports
    - Right Angle, prevents movement as both ports are preventing movement perpendicular to their direction
  - Position of auto-routed wires has been fixed for some orientations
    - multiple 4 segment right-angle cases
