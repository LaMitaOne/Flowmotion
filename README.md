# Flowmotion
Delphi Gallery Component – Lightweight Animated Coverflow / Masonry Viewer (BETA)

Sample Video: ---- https://www.youtube.com/watch?v=38Dcwo1VNqQ ----- 

Homepage https://lamita.jimdosite.com/

A fast, lightweight, Coverflow-style gallery component for Delphi — fully animated without any 3D engine.
Low CPU usage, easy to integrate, and smooth visual effects even with large image sets.

Features:

Pinterest-like masonry layout
Animated appearance (slide-in, “falling” effect on clear/page change)
Select / focus item and move or zoom it into a target rect
HotTrack & HotZoom hover effects
Smooth transitions, similar in spirit to TMS GUIMotions but much lighter

Works on Delphi 7 and Delphi 11.3

Not Finished completely yet, but already working inside MEDIA Revolution X (freeware): https://lamita.jimdosite.com/
At first wanted to make only some little pic gallery for my player, but its getting a bit more :D 
So i thought maybe some of you may like it too :)
Still playing around at it and some things not work properly sure... but basically it works stable in my player already and looking nice :D

Latest changes:
  v 0.98
    - new TImageEntryStyle -> Flexible entry/fly-in styles for new images:
      iesFromTop and so on for moving to sides and new:
      iesFromCenter, // pop-in from image center
      iesFromPoint  //move to target rect
      for falling normal pics and selected different target possible
    - Clear & remove got TImageEntryStyle and FallingTargetPos too
    - sample updated with some of those functions shown
  v 0.97
    - hotzoom now faster zooms in, slower zooms out
    - selected image breathing effect while mouseover
    - ZoomSelectedtoCenter optional now, more grid style then
    - Images zoom down now on mousedown
    - some bugfixes
