# Flowmotion
Delphi Gallery Component – Lightweight Animated Coverflow / Masonry Viewer (BETA)  

Sample Video: ---- https://www.youtube.com/watch?v=38Dcwo1VNqQ -----  
  
![screenshot](https://github.com/user-attachments/assets/6e44182f-b24e-4280-b871-e20cfe994212)  
  
Homepage https://lamita.jimdosite.com/  

A fast, lightweight, Coverflow-style gallery component for Delphi — fully animated without any 3D engine.  
Low CPU usage, easy to integrate, and smooth visual effects even with large image sets.  

### Features:  
- Pinterest-like masonry layout  
- Animated appearance (slide-in, “falling” effect on clear/page change)  
- Select / focus item and move or zoom it into a target rect  
- HotTrack & HotZoom hover effects  
- Smooth transitions, similar in spirit to TMS GUIMotions but much lighter  
- Works on Delphi 7 and Delphi 11.3  

### Performance (compared to ImageEN MView Coverflow)  
On my Asus Zenbook ux305ca (dual-core toy):  
- ImageEn: 1/4 screen, 30 pics → 50 % CPU and almost unusable  
- Flowmotion: Fullscreen, 80 pics → 25 % CPU usage, still perfectly smooth :)
  
Not finished completely yet, but already in use in **MEDIA Revolution X** (freeware):  
https://lamita.jimdosite.com/
  
At first I just wanted a little picture gallery for my player… but it grew a bit :D  
So I thought maybe some of you will like it too :)  
  
Still tweaking and some things are not perfect yet, some not fully implemented,   
but it’s already basically stable and looks really nice in my player!  
  
### Latest changes:   
  
**v 0.98**  
- New `TImageEntryStyle` → flexible entry/fly-in styles for new images:  
  `iesFromTop`, `iesFromLeft`, `iesFromRight`, etc. and new:  
  `iesFromCenter` – pop-in from image center  
  `iesFromPoint` – move to any target rect  
  → different styles for normal/falling images and selected images possible  
- Clear & remove now also support `TImageEntryStyle` and `FallingTargetPos`  
- Sample project updated to demonstrate these effects  
- Hotzoomed now ...always ^^ animate back to min...mostly :D  
- prev selected if hot sometimes dissapears while move to old pos - fixed  
- simplified animated clear cycle  
- some bugfixes  
  
**v 0.97**  
- HotZoom now zooms in faster, zooms out slower  
- Selected image has a subtle “breathing” effect on mouse-over  
- `ZoomSelectedtoCenter` is now optional (more grid-style possible)  
- Images zoom down slightly on mouse-down  
- Various bugfixes  
