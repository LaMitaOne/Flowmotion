# Flowmotion
  
Delphi Gallery Component – Lightweight Animated Coverflow / Masonry Viewer (Beta)  
    
Skia alpha Version here: https://github.com/LaMitaOne/skia-flowmotion    
       
      
If you want to tip me a coffee.. :)   

<p align="center">
  <a href="https://www.paypal.com/donate/?hosted_button_id=RX5KTTMXW497Q">
    <img src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif" alt="Donate with PayPal"/>
  </a>
</p>
    
   
Homepage https://lamita.jimdosite.com/  
   

  
  
![screenshot](https://github.com/user-attachments/assets/6e44182f-b24e-4280-b871-e20cfe994212)  
    
[https://www.youtube.com/watch?v=38Dcwo1VNqQ](https://youtu.be/D5KmriyDTWk?si=yKdP_7NnGoMoiCrJ)  
    



A fast, lightweight, Coverflow-style gallery component for Delphi — fully animated without any 3D engine or dlls needed.   
Pure Delphi 7 compatible code, no special components.    
Low CPU usage, easy to integrate, and smooth visual effects even with large image sets.   
  
### Features:  
- Pinterest-like masonry layout
- freefloat Layout with save/load positions
- Animated appearance (slide-in, “falling” effects, breathing)  
- Select / move or zoom it into a target rect   
- HotTrack & HotZoom hover effects   
- Dragging selected or all free floating   
- Threaded Animation
- Paging with falling animation  
- ActivationZone -> move selected to defined areas and trigger event   
- Smooth transitions, similar in spirit to TMS GUIMotions but much lighter  
- Works on Delphi 7 and Delphi 11.3 here, so probably on any Delphi    

  Compiled exe included, play some with it :) you will like it   
  
### Performance (compared to ImageEN MView Coverflow)  
On my Asus Zenbook ux305ca (dual-core toy):  
- ImageEn: 1/4 screen, 30 pics → 50 % CPU and almost unusable (even on big pc ^^)  
- Flowmotion: Fullscreen, 80 pics → 25 % CPU usage(on big pc around 3-5%), even at > 1000 pics still perfectly smooth :)  
   (since threading of animation its smooth like its running in some 3d engine)   

                    
 <img width="2560" height="1440" alt="Unbenannt" src="https://github.com/user-attachments/assets/a1930022-e8a9-4bb5-9eb9-ba1dbd2108e0" />     
That was around 830 pictures...2560x1440 screen resolution...no problem... 7% cpu usage(ryzen 7 5800x)...       
Not bad for "old delphi" and only canvas and a thread :D       
  
   
Not finished completely yet, but already in use in **MEDIA Revolution X** (freeware):  
https://lamita.jimdosite.com/
   
At first I just wanted a little picture gallery for my player… but it grew a bit :D  
So I thought maybe some of you will like it too :)  
    
Some things are not perfect yet, some not fully implemented,   
but it’s already basically stable and looks really nice in my player!   
  
!!! --- NOT FINISHED - Alpha fade missing still, but else most should work now...getting beta, testing to find more possible problems  

          
### Latest changes:      
   
**v 0.993**  
- NEW: Added SmallPic / Image Overlay support.   
- NEW: Added 'SmallPicImageList' property (TImageList) for efficient icon management.   
- NEW: Added 'SmallPicIndex' to TImageItem to assign icons from the ImageList.   
- NEW: Added 'SmallPicPosition' (TopLeft, TopRight, BottomLeft, BottomRight) to TImageItem.   
- NEW: Added 'SmallPicVisible' global property to toggle overlays on/off.  
- FIX: Added master list 'FAllSmallPicIndices' to persist icon assignments during Paging.   
- IMPROVED: All loading methods (AddImage, AddImages, Async, etc.) now accept SmallPicIndex parameters.   
- NEW: Added 'AllImageItems' property for global access to items (Caption, Hint, SmallPicIndex).   
  Syntax: Flowmotion1.AllImageItems[5].Caption := '...';   
  Works across all pages (updates master lists and visible screen instantly).   
     
**v 0.992**    
- new Paging property AutoScrollPageForNewAdded  
- Paging now everywhere implemented  
- added new events: TImageLoadFailedEvent, FOnImageMouseLeave, FOnImageMouseEnter  
    
**v 0.991**     
- more improvements for Paging, basically working now when u load list of pics with Addimages   
      
**v 0.990**   
- Added hints for each Imageitem (in AddImage, Addimages...)
      
**v 0.989**  
- improved caption rendering   
- some small bugfixes   
      
**v 0.988**       
- LoadPositionsFromFile bug fixed(wasnt working after new exe start)     
- MoveImage bug fixed when there was duplicate picpaths     
- New Captions with semi-transparent background     
  CaptionOnHoverOnly, ShowCaptions, CaptionFont, CaptionColor, CaptionBackground,     
  CaptionAlpha, CaptionOffsetY propertys   
- OnCaptionClick event added   
      
**v 0.987**  
- New flFreeFloat Layout added, no zoomselected to center and all pictures free draggable (like a nonstatic grid)     
- New SavePositionsToFile() / LoadPositionsFromFile() for persisting free float layouts    
- AddImagesWithPositions() allows precise image placement with saved coordinates    
- Automatic position validation ensures layouts work across different screen resolutions     
- Sample project demonstrates save/load functionality  
- added new KeepfreeAreaRect (not working fully now)  
      
**v 0.986**   
- Added new ActivationZones for the selected image if `SelectedMovable` is True   
- New `AddActivationZone(const AName: string; const ARect: TRect)` and `ClearActivationZones` methods   
  allow defining named rectangular areas on the component.   
- A new `OnSelectedImageEnterZone` event is fired when the dragged selected image   
  enters one of these zones. The event provides the image item and the name of the zone.   
- Added those functions to Sample project     
      
**v 0.985**   
- Improved Z-ordering for animated and static images.   
  The paint routine now sorts items by a combination of their hot-zoom factor    
  and actual on-screen area. This ensures that actively zooming images are    
  always drawn on top, preventing visual glitches during layout changes.    
- Introduced CPU thread affinity for better performance on multi-core systems.    
- Added basic dragging functionality for the selected image.    
  When `SelectedMovable` is True, the centered selected image can be    
  dragged around to reveal images underneath.    
- Moveimage now working    
- added more functions into Sample project    
     
**v 0.984**   
- higher hotzoomed get painted above lower hotzoomed   
  (think that way almost perfect z-order... for now)  
- fixed z-order of new animated incoming single images from Addimage  
- fixed z-order of prev selected animating back from selectnext or prev pic function    
- fixed that last flicker sometimes of just hotzoomed down, in line,     
  that moment before it gets static pic again. Now all...perfect smooth,   
  no flicker, looks really awesome)    
- pause animationthread each cycle 16ms to workdown messages, no problems with animations like smarteffects that way  
- new HotTrackWidth property   
- new incoming pics now start tiny sized -> START_SCALE  
- AddImagesAsync & AddImageAsync now working     
   
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
