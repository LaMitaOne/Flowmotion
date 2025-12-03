# Flowmotion
  
Delphi Gallery Component – Lightweight Animated Coverflow / Masonry Viewer (Alpha)  
  
![screenshot](https://github.com/user-attachments/assets/6e44182f-b24e-4280-b871-e20cfe994212)  
    
[https://www.youtube.com/watch?v=38Dcwo1VNqQ](https://youtu.be/D5KmriyDTWk?si=yKdP_7NnGoMoiCrJ)  
  
  
[![Flowmotion Demo – klick zum Abspielen](https://img.youtube.com/vi/38Dcwo1VNqQ/maxresdefault.jpg)]([https://www.youtube.com/watch?v=38Dcwo1VNqQ](https://youtu.be/D5KmriyDTWk?si=yKdP_7NnGoMoiCrJ))

  
Homepage https://lamita.jimdosite.com/  
  
If you want tip me a coffee :)  

<p align="center">
  <a href="https://www.paypal.com/donate/?hosted_button_id=RX5KTTMXW497Q">
    <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="Donate with PayPal"/>
  </a>
</p>
  
A fast, lightweight, Coverflow-style gallery component for Delphi — fully animated without any 3D engine.  
Low CPU usage, easy to integrate, and smooth visual effects even with large image sets.  
  
### Features:  
- Pinterest-like masonry layout  
- Animated appearance (slide-in, “falling” effect on clear/page change)  
- Select / focus item and move or zoom it into a target rect   
- HotTrack & HotZoom hover effects  
- Threaded Animation  
- Smooth transitions, similar in spirit to TMS GUIMotions but much lighter  
- Works on Delphi 7 and Delphi 11.3  
  
### Performance (compared to ImageEN MView Coverflow)  
On my Asus Zenbook ux305ca (dual-core toy):  
- ImageEn: 1/4 screen, 30 pics → 50 % CPU and almost unusable  
- Flowmotion: Fullscreen, 80 pics → 25 % CPU usage, still perfectly smooth :)  
   (since threading of animation its smooth like its running in some 3d engine)
   
Not finished completely yet, but already in use in **MEDIA Revolution X** (freeware):  
https://lamita.jimdosite.com/
   
At first I just wanted a little picture gallery for my player… but it grew a bit :D  
So I thought maybe some of you will like it too :)  
   
Still tweaking and some things are not perfect yet, some not fully implemented,   
but it’s already basically stable and looks really nice in my player!   
  
!!! --- NOT FINISHED - Partly stable, parts not working so far.  
Stable atm: Addimages (not async), SelectNextImage, SelectPreviousImage, Animations, hotzoom, breathing at all stable,  
Clear animated to direction or target, selected can go to other target, setting Background color/pic, Glow and all that,   
DeselectZoomedImage, those i am using in my player already and no problems at all,   
other functions may work, not work, or it explodes right into ur face :D nah...  
I am working on getting more of it done :)  
    
   
### Latest changes:  
   
**v 0.984**   
- higher hotzoomed get painted above lower hotzoomed   
  (think that way almost perfect z-order... for now)  
- fixed z-order of new animated incoming single images from Addimage  
- fixed z-order of prev selected animating back from selectnext or prev pic function    
- fixed that last flicker sometimes of just hotzoomed down, in line,     
  that moment before it gets static pic again. Now all...perfect smooth,   
  no flicker, looks really awesome)   
        
**v 0.983**
- Animations now Threaded, massive performance gain like 20 times faster and smoother
      
**v 0.982**  
- Paint routine optimized  
- fixed some wrong Z-orders of prevsel or prevhot back zooming pictures getting painted below static pics
    
**v 0.981**  
- combined HottrackTimer into TimerAnimation (looks way better, and a LOT faster)  
- optimized TimerAnimation for less useless paints when nothing changed  
     
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
