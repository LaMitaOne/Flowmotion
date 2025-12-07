
{------------------------------------------------------------------------------}
{                                                                              }
{ Flowmotion v0.988                                                            }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{ larate@gmx.net                                                               }
{ https://lamita.jimdosite.com                                                 }
{                                                                              }
{------------------------------------------------------------------------------}
{
 ----Latest Changes
  v 0.988
    - LoadPositionsFromFile bug fixed(wasnt working after new exe start)
    - MoveImage bug fixed when there was duplicate picpaths
    - New Captions with semi-transparent background
      CaptionOnHoverOnly, ShowCaptions, CaptionFont, CaptionColor, CaptionBackground,
      CaptionAlpha, CaptionOffsetY, SetSelectedCaptionBackground, SetSelectedCaptionColor propertys
    - OnCaptionClick event added
    - added new KeepfreeAreaRect (not working fully now)
  v 0.987
    - New flFreeFloat Layout added, no zoomselected to center and all pictures free draggable
    - New SavePositionsToFile() / LoadPositionsFromFile() for persisting free float layouts
    - AddImagesWithPositions() allows precise image placement with saved coordinates
    - Automatic position validation ensures layouts work across different screen resolutions
    - Sample project demonstrates save/load functionality
  v 0.986
    - Added new ActivationZones for the selected image if `SelectedMovable` is True.
    - New `AddActivationZone(const AName: string; const ARect: TRect)` and `ClearActivationZones` methods
      allow defining named rectangular areas on the component.
    - A new `OnSelectedImageEnterZone` event is fired when the dragged selected image
      enters one of these zones. The event provides the image item and the name of the zone.
    - Added those functions to Sample project
  v 0.985
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
  v 0.984
    - higher hotzoomed get painted above lower hotzoomed
      (think that way almost perfect z-order... for now)
    - fixed z-order of new animated incoming single images from Addimage
    - fixed z-order of prev selected animating back from selectnext or prev pic function.
    - fixed that last flicker sometimes of just hotzoomed down, in line,
      that moment before it gets static pic again. Now all...perfect smooth,
      no flicker)
    - pause animationthread each cycle 16ms to workdown messages, no more problems with animations like smarteffects that way
    - new HotTrackWidth property
    - new inoming pics now start tiny sized START_SCALE
    - AddImagesAsync & AddImageAsync now working
  v 0.983
    - Animations now Threaded, massive performance gain like 20 times faster
  v 0.982
    - Paint routine optimized
    - fixed some wrong Z-orders of prevsel or prevhot back zooming pictures getting painted below static pics
  v 0.981
    - combined HottrackTimer into TimerAnimation (looks way better, and a LOT faster)
    - optimized TimerAnimation for less useless paints when nothing changed
    - Mousemove -> GetImageAtPoint hotzoomed now highest priority
  v 0.98
    - new TImageEntryStyle -> Flexible entry/fly-in styles for new images:
      iesFromTop and so on for moving to/from sides and new:
      iesFromCenter, // pop-in from image center
      iesFromPoint  //move to target rect
      for falling normal pics and selected different target possible
    - Clear & remove got TImageEntryStyle and FallingTargetPos too
    - sample updated with some of those functions shown
    - prev selected if hot sometimes dissapears while move to old pos - fixed
    - simplified animated clear cycle
    - some bugfixes
  v 0.97
    - hotzoom now faster zooms in, slower zooms out
    - selected image breathing effect while mouseover
    - ZoomSelectedtoCenter optional now, more grid style then
    - Images zoom down now on mousedown
    - some bugfixes
  v 0.96
    - some more Hottrackzoom improvements at edges and Prefhot...
    - some bugfixes
  v 0.95
    - ZoominSelected option at clear added
    - Zoominselected at clear to Targetpos added
    - HotTrack added
    - HotTrack Zoom added
  v 0.94
    - Animation Timer now stops always after all animations finished
    - Keep space under zoomed setting added
    - DeselectZoomedImage proc added
    - some bugfixes
}

unit UFLowmotion;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, JPEG, Math,
  Pngimage;

//, SynGdiPlus; //for test syngdiplus
//, GR32, GR32_Image, GR32_Resamplers;  //for test gr32

const
  // Animation constants
  TARGET_FPS = 30;
  MIN_FRAME_TIME = 1000 div TARGET_FPS;
  DEFAULT_ANIMATION_SPEED = 4;
  DEFAULT_ALPHA = 255;
  START_SCALE = 0.05;

  // Spacing / Effects
  MIN_CELL_SIZE = 22;
  DEFAULT_GLOW_WIDTH = 2;
  DEFAULT_HOTTRACK_WIDTH = 1;
  DEFAULT_MAX_ZOOM_SIZE = 300;
  HOT_ZOOM_MIN_FACTOR = 1.02;
  HOT_ZOOM_MAX_FACTOR = 1.3;
  HOT_ZOOM_IN_SPEED = 0.07;
  HOT_ZOOM_OUT_SPEED = 0.09;
  HOT_ZOOM_IN_PER_SEC = 2.5;
  HOT_ZOOM_OUT_PER_SEC = 3.0;
  HOT_ZOOM_EPSILON = 0.0001;
  BREATHING_AMPLITUDE = 2.0;
  BREATHING_SPEED_PER_SEC = 0.06;

  // Timeouts
  THREAD_CLEANUP_TIMEOUT = 3000;

type
{$IFDEF WIN64}
  DWORD_PTR = UInt64; // 64-bit: Pointer is 8 bytes
{$ELSE}

  DWORD_PTR = Cardinal; // 32-bit: Pointer is 4 bytes
{$ENDIF}

  TFlowLayout = (flSorted, flFreeFloat);

  TImageLoadEvent = procedure(Sender: TObject; const FileName: string; Success: Boolean) of object;

  TZoomAnimationType = (zatSlide, zatFade, zatZoom, zatBounce);

  TFlowmotionLoadMode = (lmLoadAll, lmLazy, lmLazyAndFree);

  TImagePosition = record
    FileName: string;
    Caption: string;
    Path: string;
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TImagePositions = array of TImagePosition;

  // ===================================================================
  // Flexible entry/fly-in styles for new images or fall out/remove
  // ===================================================================
  TImageEntryStyle = (iesRandom, // default = old behavior: random from 4 sides
    iesFromTop, // all drop from top
    iesFromBottom, // all rise from bottom
    iesFromLeft, // all slide in from left
    iesFromRight, // all slide in from right
    iesFromTopLeft, // diagonal from top-left corner
    iesFromTopRight, // diagonal from top-right
    iesFromBottomLeft, // diagonal from bottom-left
    iesFromBottomRight, // diagonal from bottom-right
    iesFromCenter, // pop-in from image center
    iesFromPoint // all fly in from a custom point (e.g. mouse)
  );


  {
    TImageItem - Represents a single image in the gallery with animation state
    Properties:
      - Bitmap: The actual image data
      - CurrentRect: Current position/size on screen
      - TargetRect: Target position/size for animation
      - AnimationProgress: 0.0 to 1.0 animation completion
      - Alpha: Current transparency (0-255)
  }
  TImageItem = class
  private
    FBitmap: TBitmap;
    FCaption: string;
    FPath: string;
    FCurrentRect: TRect;
    FTargetRect: TRect;
    FStartRect: TRect;
    FAnimationProgress: Double;
    FAnimating: Boolean;
    FDirection: TImageEntryStyle;
    FVisible: Boolean;
    FAlpha: Byte;
    FTargetAlpha: Byte;
    FFileName: string;
    FIsSelected: Boolean;
    FZoomProgress: Double;
    FHotZoom: Double;
    FHotZoomTarget: Double;
  public
    constructor Create;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write FBitmap;
    property CurrentRect: TRect read FCurrentRect write FCurrentRect;
    property TargetRect: TRect read FTargetRect write FTargetRect;
    property StartRect: TRect read FStartRect write FStartRect;
    property AnimationProgress: Double read FAnimationProgress write FAnimationProgress;
    property Animating: Boolean read FAnimating write FAnimating;
    property Visible: Boolean read FVisible write FVisible;
    property Caption: string read FCaption write FCaption;
    property Path: string read FPath write FPath;
    property Alpha: Byte read FAlpha write FAlpha;
    property TargetAlpha: Byte read FTargetAlpha write FTargetAlpha;
    property FileName: string read FFileName write FFileName;
    property IsSelected: Boolean read FIsSelected write FIsSelected;
    property ZoomProgress: Double read FZoomProgress write FZoomProgress;
    property Direction: TImageEntryStyle read FDirection write FDirection;
  end;

  TOnSelectedItemMouseDown = procedure(Sender: TObject; ImageItem: TImageItem; Index, X, Y: Integer; Button: TMouseButton; Shift: TShiftState) of object;

  TOnAllAnimationsFinished = procedure(Sender: TObject) of object;

  TOnSelectedImageDblClick = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TImageSelectEvent = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TOnCaptionClick = procedure(Sender: TObject; ImageItem: TImageItem; Index: Integer) of object;

  TBooleanGrid = array of array of Boolean;

  // Defines a rectangular activation zone for the selected image
  TActivationZone = record
    Name: string;
    Rect: TRect;
  end;
  // Event type fired when the selected image enters an activation zone

  TSelectedImageEnterZoneEvent = procedure(Sender: TObject; ImageItem: TImageItem; const ZoneName: string) of object;


  {
    TImageLoadThread - Background thread for loading images
    Loads images asynchronously to prevent UI blocking
  }
  TImageLoadThread = class(TThread)
  private
    FCaption: string;
    FPath: string;
    FFileName: string;
    FBitmap: TBitmap;
    FOwner: TObject;
    FSuccess: Boolean;
    procedure SyncAddImage;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFileName, ACaption, APath: string; AOwner: TObject; TargetCoreIndex: Integer);
    destructor Destroy; override;
  end;

type
  TAnimationThread = class(TThread)
  private
    FOwner: TCustomControl;
    FLastTick: Cardinal;
    FStopRequested: Boolean;
    FEvent: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TCustomControl);
    destructor Destroy; override;
    procedure Stop;
  end;

  {
    TFlowmotion - Main component for animated image gallery

    Key Features:
      - Animated grid layout with customizable spacing
      - Zoom functionality for selected images
      - Paging support for large image collections
      - Async image loading with threads
      - Various animation types (slide, fade, zoom, bounce)

    Usage:
      1. Add images via AddImage() or AddImages() or AddImageAsync() or AddImagesAsync()
      2. Navigate with SelectNextImage/SelectPreviousImage or arrow keys
      3. Use NextPage/PrevPage for paging
  }
  TFlowmotion = class(TCustomControl) // TPaintbox32
  private
    // Core image management
    FImages: TList; // Current visible images (TImageItem)
    FAllFiles: TStringList; // All image files (for paging)
    FAllCaptions: TStringList; // All captions
    FAllPaths: TStringList; // All paths

    // Threading
    FNextLoaderCoreIndex: Integer;
    FAnimationThread: TAnimationThread;
    FLoadingThreads: TList; // Active loading threads
    FLoadingCount: Integer; // Number of loading threads
    FClearing: Boolean;

    // Animation
    inPaintCycle: Boolean; //.paint cycle running atm
    FAnimationSpeed: Integer; // 1-100, higher = faster
    FAnimationEasing: Boolean; // Use easing function
    FInFallAnimation: Boolean; // Currently in fall animation
    FFallingOut: Boolean; // Page change fall animation
    FPageOutProgress: Double; // 0.0 to 1.0
    FImageEntryStyle: TImageEntryStyle;
    FEntryPoint: TPoint; // only used with iesFromPoint

    // Layout
    FFlowLayout: TFlowLayout;
    FKeepSpaceforZoomed: Boolean;
    FSpacing: Integer; // Space between images
    FKeepAspectRatio: Boolean;
    FMaxColumns: Integer; // 0 = auto
    FMaxRows: Integer; // 0 = auto
    FSorted: Boolean; // True = load order, False = size order
    FDragOffset: TPoint;
    FDraggingImage: Boolean;
    FDraggedImage: TImageItem;
    FLoadedPositions: TImagePositions;

    // Visual
    FBackgroundColor: TColor;
    FBackgroundpicture: TBitmap;
    FBackgroundCache: TBitmap;
    FTempBitmap: TBitmap;
    FBackgroundCacheValid: Boolean;
    FHotTrackColor: TColor;
    FHotTrackWidth: Integer;
    FHotTrackZoom: Boolean;
    FBreathingPhase: Double;
    FBreathingEnabled: Boolean;
    FZoomSelectedtoCenter: Boolean;
    FGlowColor: TColor;
    FGlowWidth: Integer;
    FCaptionFont: TFont;
    FCaptionColor: TColor;
    FCaptionBackground: TColor;
    FSelectedCaptionColor: TColor;
    FSelectedCaptionBackground: TColor;
    FCaptionAlpha: Byte;
    FCaptionOffsetY: Integer;
    FShowCaptions: Boolean;
    FCaptionOnHoverOnly: Boolean;
    FKeepAreaFreeRect: TRect;

    // Selection & Zoom
    FSelectedImage: TImageItem;
    FWasSelectedItem: TImageItem; // Previous selection (for animation)
    FCurrentSelectedIndex: Integer; // Index in FImages (relative to page)
    FMaxZoomSize: Integer;
    FZoomAnimationType: TZoomAnimationType;
    FSelectedMovable: Boolean;
    FDraggingSelected: Boolean;
    FDragStartPos: TPoint;
    FIsPotentialDrag: Boolean;
    FHotItem: TImageItem;
    FActivationZones: array of TActivationZone;
    FLastActivationZoneName: string;
    FOnCaptionClick: TOnCaptionClick;

    // Paging
    FPageSize: Integer; // Images per page
    FCurrentPage: Integer; // 0-based page index
    FPageChangeInProgress: Boolean;
    FLoadMode: TFlowmotionLoadMode;

    // State
    FActive: Boolean;
    FThreadPriority: TThreadPriority;

    // Events
    FOnImageLoad: TImageLoadEvent;
    FOnItemSelected: TImageSelectEvent;
    FOnSelectedItemMouseDown: TOnSelectedItemMouseDown;
    FOnAllAnimationsFinished: TOnAllAnimationsFinished;
    FOnSelectedImageDblClick: TOnSelectedImageDblClick;
    FOnSelectedImageEnterZone: TSelectedImageEnterZoneEvent;

    // Internal methods - Animation
    procedure WMUser1(var Message: TMessage); message WM_USER + 1;
    procedure WMUser2(var Message: TMessage);
      //yea it says never used, but it is, here: PostMessage(Handle, WM_USER + 2, 0, 0);
    procedure StopAnimationThread;
    procedure StartAnimationThread;
    procedure ThreadSafeFireAllAnimationsFinished;
    procedure ThreadSafeInvalidate;
    procedure PerformAnimationUpdate(DeltaMS: Cardinal);
    function AnimationsRunning: Boolean;
    function EaseInOutQuad(t: Double): Double;
    function GetEntryDirection: TImageEntryStyle;
    procedure WaitForAllAnimations;
    procedure FreeAllImagesAndClearLists;
    procedure AnimateImage(ImageItem: TImageItem; EntryStyle: TImageEntryStyle; UseSavedPosition: Boolean; SavedTargetRect: TRect);
    procedure StartZoomAnimation(ImageItem: TImageItem; ZoomIn: Boolean);
    procedure SetImageEntryStyle(Value: TImageEntryStyle);

    // Internal methods - Layout
    procedure CalculateLayout;
    procedure CalculateLayoutSorted;
    procedure CalculateLayoutFreeFloat;
    function IsAreaFree(const Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer): Boolean;
    procedure MarkAreaOccupied(var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer);
    function PlaceImage(ImageItem: TImageItem; var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer; BaseCellWidth, BaseCellHeight: Integer): Boolean;
    function GetOptimalSize(const OriginalWidth, OriginalHeight: Integer; const MaxWidth, MaxHeight: Integer): TSize;

    // Internal methods - Paging
    function GetPageCount: Integer;
    function GetPageStartIndex: Integer;
    function GetPageEndIndex: Integer;
    procedure SetPageSize(Value: Integer);
    procedure ShowPage(Page: Integer);
    procedure NextPage;
    procedure PrevPage;

    // Internal methods - Threading
    procedure ThreadFinished(Thread: TImageLoadThread);
    procedure EnsureBitmapLoaded(Item: TImageItem);
    procedure FreeInvisibleBitmaps;

    // Internal methods - Utilities
    function GetImageItem(Index: Integer): TImageItem;
    function GetImageCount: Integer;
    function GetLoadingCount: Integer;
    procedure SetSelectedImage(ImageItem: TImageItem; Index: Integer);
    procedure LoadImageFromFile(const AFileName: string; ABitmap: TBitmap);
    function GetCaptionRect(Item: TImageItem; const DrawRect: TRect): TRect;

    // Property setters
    procedure SetLoadMode(Value: TFlowmotionLoadMode);
    procedure SetActive(Value: Boolean);
    procedure SetSelectedMovable(Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetKeepSpaceforZoomed(Value: Boolean);
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure SetHotTrackColor(Value: TColor);
    procedure SetGlowColor(Value: TColor);
    procedure SetGlowWidth(Value: Integer);
    procedure SetHotTrackWidth(Value: Integer);
    procedure SetAnimationSpeed(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetKeepAspectRatio(const Value: Boolean);
    procedure SetHotTrackZoom(const Value: Boolean);
    procedure SetZoomSelectedtoCenter(Value: Boolean);
    procedure SetBreathingEnabled(Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetMaxColumns(const Value: Integer);
    procedure SetMaxRows(const Value: Integer);
    procedure SetCaptionFont(Value: TFont);
    procedure SetCaptionColor(Value: TColor);
    procedure SetCaptionBackground(Value: TColor);
    procedure SetCaptionAlpha(Value: Byte);
    procedure SetCaptionOffsetY(Value: Integer);
    procedure SetShowCaptions(Value: Boolean);
    procedure SetCaptionOnHoverOnly(Value: Boolean);
    procedure SetSelectedCaptionColor(Value: TColor);
    procedure SetSelectedCaptionBackground(Value: TColor);
    procedure SetKeepAreaFreeRect(const Value: TRect);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoImageLoad(const FileName: string; Success: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Layout
    procedure SetFlowLayout(Value: TFlowLayout);
    // Save/load positions to file
    procedure SavePositionsToFile(const FileName: string);
    procedure LoadPositionsFromFile(const FileName: string);
    procedure SavePositionsToStream(Stream: TStream);
    procedure LoadPositionsFromStream(Stream: TStream);
    // Add images with specific positions
    procedure AddImagesWithPositions(const FileNames, Captions, Paths: TStringList; const Positions: array of TRect);
    // Get current positions of all images
    function GetCurrentPositions: TImagePositions;
    // Reset all positions to default layout
    procedure ResetPositions;

    // Image management
    procedure AddImage(const FileName: string); overload;
    procedure AddImage(const FileName, ACaption, APath: string); overload;
    procedure AddImage(Bitmap: TBitmap); overload;
    procedure AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = '');
    procedure AddImages(const FileNames, Captions, Paths: TStringList);
    procedure AddImagesAsync(const FileNames, Captions, Paths: TStringList);
    procedure InsertImage(Pic: TBitmap; const XFileName, XCaption, XPath: string);
    procedure InsertImageAsync(const FileName, Caption, Path: string);
    procedure SetImage(Index: Integer; Bitmap: TBitmap);
    procedure RemoveImage(Index: Integer; animated: Boolean = True); overload;
    procedure RemoveImage(Index: Integer; Animated: Boolean; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle); overload;
    procedure Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom); overload;
    procedure Clear(animated: Boolean; ZoominSelected: Boolean = false); overload;
    procedure MoveImageToPos(RelIndexFrom, RelIndexTo: Integer);

    // Activation zone
    procedure AddActivationZone(const AName: string; const ARect: TRect);
    procedure ClearActivationZones;

    // Navigation
    procedure SelectNextImage;
    procedure SelectPreviousImage;
    procedure DeselectZoomedImage;
    procedure ScrollToIndex(Index: Integer; Animate: Boolean = True);

    // Search
    function FindImageByPath(const Path: string): TImageItem;
    function FindImageByCaption(const Caption: string): TImageItem;
    function GetImageIndex(ImageItem: TImageItem): Integer;
    function GetImageAtPoint(X, Y: Integer): TImageItem;

    // Utilities
    function GetPicture(index: Integer): TBitmap;
    procedure SetBackgroundpicture(const Path: string);
    procedure WaitForAllLoads;

    // Properties
    property MaxZoomSize: Integer read FMaxZoomSize write FMaxZoomSize default DEFAULT_MAX_ZOOM_SIZE;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor;
    property GlowColor: TColor read FGlowColor write SetGlowColor;
    property GlowWidth: Integer read FGlowWidth write SetGlowWidth;
    property HotTrackWidth: Integer read FHotTrackWidth write SetHotTrackWidth;
    property CaptionOnHoverOnly: Boolean read FCaptionOnHoverOnly write SetCaptionOnHoverOnly default True;
    property PageCount: Integer read GetPageCount;
    property CurrentSelectedIndex: Integer read FCurrentSelectedIndex;
    property ImageCount: Integer read GetImageCount;
    property LoadingCount: Integer read GetLoadingCount;
    property SelectedImage: TImageItem read FSelectedImage;
    property Items[Index: Integer]: TImageItem read GetImageItem; default;
    property Images[Index: Integer]: TImageItem read GetImageItem;

  published
    property LoadMode: TFlowmotionLoadMode read FLoadMode write SetLoadMode;
    property FlowLayout: TFlowLayout read FFlowLayout write SetFlowLayout;
    property AnimationSpeed: Integer read FAnimationSpeed write SetAnimationSpeed default DEFAULT_ANIMATION_SPEED;
    property SelectedMovable: Boolean read FSelectedMovable write SetSelectedMovable default true;
    property Sorted: Boolean read FSorted write SetSorted default True;
    property KeepSpaceforZoomed: Boolean read FKeepSpaceforZoomed write SetKeepSpaceforZoomed default false;
    property Spacing: Integer read FSpacing write SetSpacing default 0;
    property KeepAspectRatio: Boolean read FKeepAspectRatio write SetKeepAspectRatio default True;
    property HotTrackZoom: Boolean read FHotTrackZoom write SetHotTrackZoom default True;
    property BreathingEnabled: Boolean read FBreathingEnabled write SetBreathingEnabled default True;
    property ZoomSelectedtoCenter: Boolean read FZoomSelectedtoCenter write SetZoomSelectedtoCenter default True;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBlack;
    property MaxColumns: Integer read FMaxColumns write SetMaxColumns default 0;
    property MaxRows: Integer read FMaxRows write SetMaxRows default 0;
    property AnimationEasing: Boolean read FAnimationEasing write FAnimationEasing default True;
    property OnImageLoad: TImageLoadEvent read FOnImageLoad write FOnImageLoad;
    property OnItemSelected: TImageSelectEvent read FOnItemSelected write FOnItemSelected;
    property Active: Boolean read FActive write SetActive default True;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority;
    property OnSelectedItemMouseDown: TOnSelectedItemMouseDown read FOnSelectedItemMouseDown write FOnSelectedItemMouseDown;
    property OnAllAnimationsFinished: TOnAllAnimationsFinished read FOnAllAnimationsFinished write FOnAllAnimationsFinished;
    property OnSelectedImageDblClick: TOnSelectedImageDblClick read FOnSelectedImageDblClick write FOnSelectedImageDblClick;
    property ZoomAnimationType: TZoomAnimationType read FZoomAnimationType write FZoomAnimationType default zatSlide;
    property PageSize: Integer read FPageSize write SetPageSize;
    property CurrentPage: Integer read FCurrentPage;
    property ImageEntryStyle: TImageEntryStyle read FImageEntryStyle write SetImageEntryStyle default iesRandom;
    property EntryPoint: TPoint read FEntryPoint write FEntryPoint;
    property OnSelectedImageEnterZone: TSelectedImageEnterZoneEvent read FOnSelectedImageEnterZone write FOnSelectedImageEnterZone;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default True;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clWhite;
    property CaptionBackground: TColor read FCaptionBackground write SetCaptionBackground default clBlack;
    property CaptionAlpha: Byte read FCaptionAlpha write SetCaptionAlpha default 180;
    property CaptionOffsetY: Integer read FCaptionOffsetY write SetCaptionOffsetY default 8;
    property OnCaptionClick: TOnCaptionClick read FOnCaptionClick write FOnCaptionClick;
    property SelectedCaptionColor: TColor read FSelectedCaptionColor write SetSelectedCaptionColor default clYellow;
    property SelectedCaptionBackground: TColor read FSelectedCaptionBackground write SetSelectedCaptionBackground default clMaroon;
    property KeepAreaFreeRect: TRect read FKeepAreaFreeRect write SetKeepAreaFreeRect;

    // Inherited properties
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
  end;

procedure Register;

implementation

{ Registers the component in the IDE component palette }

procedure Register;
begin
  RegisterComponents('LaMita Components', [TFlowmotion]);
end;

{ TAnimationThread }
constructor TAnimationThread.Create(AOwner: TCustomControl);
var
  SystemInfo: TSystemInfo;
  AffinityMask: DWORD_PTR;
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FLastTick := GetTickCount;
  FStopRequested := False;
  FEvent := CreateEvent(nil, True, False, nil);
  Priority := (AOwner as TFlowmotion).FThreadPriority;

  // Set thread affinity to the second logical processor (index 1), if available
  GetSystemInfo(SystemInfo);
  if SystemInfo.dwNumberOfProcessors > 1 then
  begin
    // Create a mask for the second processor (bit 1 is set)
    AffinityMask := 1 shl 1; // Equivalent to 2
    SetThreadAffinityMask(GetCurrentThread, AffinityMask);
  end;
end;

destructor TAnimationThread.Destroy;
begin
  CloseHandle(FEvent);
  inherited Destroy;
end;

procedure TAnimationThread.Stop;
begin
  FStopRequested := True;
  SetEvent(FEvent);
end;

procedure TAnimationThread.Execute;
var
  NowTick, LastTick, ElapsedMS, SleepTime: Cardinal;

  // Helper procedure to process a few messages without blocking the main thread
{  procedure ProcessFewMessages;
  var
    Msg: TMsg;
    i: Integer;
  begin
      // Process only a small number of messages (e.g., 10)
      // and then release control. This keeps the main thread responsive.
    for i := 1 to 10 do
    begin
      if not PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
        Break; // No more messages in the queue, so we're done for now
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;     }
begin
  // Initialize the timer for the first frame.
  LastTick := GetTickCount;

  // The main animation loop. It runs until the thread is terminated or a stop is requested.
  while not Terminated and not FStopRequested do
  begin
    // --- 1. Execute the main animation logic ---
    // Call the owner's update method, passing the time delta since the last frame.
    // This ensures smooth animation regardless of the actual frame rate.
    (FOwner as TFlowMotion).PerformAnimationUpdate(GetTickCount - LastTick);

    // --- 2. Calculate timing for the next frame ---
    NowTick := GetTickCount;
    ElapsedMS := NowTick - LastTick; // Measure how long the work took.
    LastTick := NowTick; // Reset the timer for the next iteration.

    // --- 3. Wait until the next frame is due ---
    // We need to subtract the work duration from the total frame time to achieve our target FPS.
    SleepTime := 0;
    if ElapsedMS < MIN_FRAME_TIME then
    begin
      SleepTime := 16 + MIN_FRAME_TIME - ElapsedMS;
      //20 min..that way only we get enough free time for problematic same time running animations like from smart effects
    end;

    // The 'cooperative' waiting that gives the main thread time to breathe ---
    // When we wait here, we use the calculated sleep time. This is much more efficient
    // than a fixed Sleep() duration because it allows the thread to wake up immediately
    // if a stop is requested.
    if WaitForSingleObject(FEvent, SleepTime) = WAIT_OBJECT_0 then
      Break // Stop event was triggered, exit the loop immediately.
    else
    begin
      // During the wait time, we use the opportunity to allow the main thread
      // to process its message queue (e.g., Synchronize requests from other threads).
     // ProcessFewMessages;
      Application.ProcessMessages;
    end;
  end;
end;

{ TImageItem }

{ Creates a new image item with default values }
constructor TImageItem.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  FAnimationProgress := 0;
  FAnimating := False;
  FVisible := False;
  FAlpha := DEFAULT_ALPHA;
  FTargetAlpha := DEFAULT_ALPHA;
  FIsSelected := False;
  FZoomProgress := 0;
  FHotZoom := 1;
  FHotZoomTarget := 1;
end;

{ Frees the bitmap and destroys the image item }
destructor TImageItem.Destroy;
begin
  if assigned(FBitmap) then
    FBitmap.Free;
  inherited Destroy;
end;

{ TImageLoadThread }

{ Creates a new image loading thread }
constructor TImageLoadThread.Create(const AFileName, ACaption, APath: string; AOwner: TObject; TargetCoreIndex: Integer);
var
  SystemInfo: TSystemInfo;
  AffinityMask: DWORD_PTR;
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FFileName := AFileName;
  FCaption := ACaption;
  FPath := APath;
  FOwner := AOwner;
  FBitmap := TBitmap.Create;
  FSuccess := False;
  Priority := TFlowmotion(FOwner).FThreadPriority;

  // Set thread affinity to the specified core, if it's a valid index
  GetSystemInfo(SystemInfo);
  if (TargetCoreIndex >= 0) and (TargetCoreIndex < SystemInfo.dwNumberOfProcessors) then
  begin
    AffinityMask := 1 shl TargetCoreIndex;
    SetThreadAffinityMask(GetCurrentThread, AffinityMask);
  end;
end;

{ Frees the bitmap and destroys the thread }
destructor TImageLoadThread.Destroy;
begin
  if assigned(FBitmap) then
    FBitmap.Free;
  inherited Destroy;
end;

{ Main thread execution: loads image from file }
procedure TImageLoadThread.Execute;
begin
  FSuccess := False;
  try
    // Call the centralized loading function
    TFlowmotion(FOwner).LoadImageFromFile(FFileName, FBitmap);
    FSuccess := True;
  except
    // FSuccess remains False if an exception occurs
    on E: Exception do
      FSuccess := False;
  end;

  if (not Terminated) and FSuccess then
    Synchronize(SyncAddImage);

  // Thread cleanup
  if not Terminated then
  begin
    try
      TFlowmotion(FOwner).ThreadFinished(Self);
    except
      // Ignore exceptions during cleanup
    end;
  end;
end;

{ Synchronized method: adds loaded image to the component (called from main thread) }
procedure TImageLoadThread.SyncAddImage;
var
  NewItem: TImageItem;
begin
  try
    if Assigned(FOwner) and (FOwner is TFlowmotion) then
    begin
      if FSuccess then
      begin
        with TFlowmotion(FOwner) do
        begin
          // Create the image item directly
          NewItem := TImageItem.Create;
          NewItem.Bitmap.Assign(FBitmap);
          NewItem.Caption := FCaption;
          NewItem.Path := FPath;
          NewItem.FileName := FFileName;
          NewItem.Direction := GetEntryDirection;
          NewItem.Visible := False;
          FImages.Add(NewItem);

          CheckSynchronize(10);
          if Visible then
          begin
            // Calculate the layout for all images
            CalculateLayout;

            // Set up animation for the new image
            AnimateImage(NewItem, NewItem.Direction, false, Rect(0, 0, 0, 0));
            NewItem.Visible := True;
          end;
        end;
      end;
      TFlowmotion(FOwner).DoImageLoad(FFileName, FSuccess);
    end;
  finally
    TFlowmotion(FOwner).ThreadFinished(Self);
    TFlowmotion(FOwner).StartAnimationThread;
  end;
end;

{ TFlowmotion }

{ Creates and initializes the Flowmotion component }
constructor TFlowmotion.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Gdip := TGDIPlus.Create('');
  FCaptionFont := TFont.Create;
  FCaptionFont.Name := 'Segoe UI';
  FCaptionFont.Size := 10;
  FCaptionFont.Style := [fsBold];
  FCaptionColor := clWhite;
  FCaptionBackground := clBlack;
  FSelectedCaptionColor := clBlack;
  FSelectedCaptionBackground := clAqua;
  FCaptionAlpha := 180;
  FShowCaptions := True;
  FCaptionOnHoverOnly := True;
  FCaptionOffsetY := 8;
  FImages := TList.Create;
  SetLength(FLoadedPositions, 0);
  FDraggingImage := False;
  FDraggedImage := nil;
  FNextLoaderCoreIndex := 0;
  FClearing := False;
  FBreathingPhase := 0.0;
  FLoadingThreads := TList.Create;
  FAllFiles := TStringList.Create;
  FAllCaptions := TStringList.Create;
  FAllPaths := TStringList.Create;
  FBackgroundpicture := TBitmap.Create;
  FBackgroundCache := TBitmap.Create;
  FTempBitmap := TBitmap.Create;
  FAnimationThread := nil;
  FImageEntryStyle := iesRandom;
  FEntryPoint := Point(-1000, -1000);
  FFlowLayout := flSorted; //flSorted  flFreeFloat
  FKeepSpaceforZoomed := False;
  FLoadMode := lmLazy;
  FAnimationSpeed := DEFAULT_ANIMATION_SPEED;
  FSpacing := 0;
  FKeepAspectRatio := True;
  FBackgroundColor := clBlack;
  FHotTrackColor := clTeal;
  HotTrackZoom := true;
  FMaxColumns := 0;
  FMaxRows := 0;
  FSelectedMovable := True;
  FDraggingSelected := False;
  FDragOffset := Point(0, 0);
  FSorted := True;
  FAnimationEasing := True;
  FLoadingCount := 0;
  FGlowColor := clAqua;
  FGlowWidth := DEFAULT_GLOW_WIDTH;
  FHotTrackWidth := DEFAULT_HOTTRACK_WIDTH;
  FSelectedImage := nil;
  FWasSelectedItem := nil;
  FZoomAnimationType := zatSlide;
    // zatSlide, zatFade, zatZoom, zatBounce   not working atm
  FBackgroundCacheValid := False;
  FPageSize := 100;
  FCurrentPage := 0;
  FCurrentSelectedIndex := -1;
  FActive := True;
  FThreadPriority := tpNormal;
  DoubleBuffered := True;
  TabStop := True;
  ControlStyle := ControlStyle + [csOpaque, csDoubleClicks];
  Width := 400;
  Height := 300;
  Color := FBackgroundColor;
  FBreathingEnabled := True;
  FZoomSelectedtoCenter := True;
end;

{ Destroys the component and frees all resources }
destructor TFlowmotion.Destroy;
var
  i: Integer;
  StartTime: Cardinal;
begin
  try
    if FAnimationThread <> nil then
    begin
      FAnimationThread.Stop;
      FAnimationThread.WaitFor;
      FreeAndNil(FAnimationThread);
    end;
    // Terminate all threads
    for i := 0 to FLoadingThreads.Count - 1 do
    begin
      try
        TImageLoadThread(FLoadingThreads[i]).Terminate;
      except
        // Continue terminating other threads
      end;
    end;
    // Wait for threads to finish (with timeout)
    StartTime := GetTickCount;
    while (FLoadingThreads.Count > 0) and ((GetTickCount - StartTime) < THREAD_CLEANUP_TIMEOUT) do
    begin
      CheckSynchronize(10);
      Sleep(5);
    end;
    // Force clear remaining threads
    FLoadingThreads.Clear;
    CheckSynchronize(10);
    FLoadingThreads.Free;
    // Free all images
    for i := 0 to FImages.Count - 1 do
    begin
      try
        TImageItem(FImages[i]).Free;
      except
        // Continue freeing other images
      end;
    end;
    FImages.Free;
    // Free resources
    FBackgroundCache.Free;
    FTempBitmap.free;
    FBackgroundpicture.Free;
    FAllFiles.Free;
    FAllCaptions.Free;
    FAllPaths.Free;
    //Gdip.Free;
  except
    // Ensure destructor completes even if errors occur
  end;
  inherited Destroy;
end;

procedure TFlowmotion.StartAnimationThread;
begin
  if (FAnimationThread = nil) or FAnimationThread.Terminated then
  begin
    if FAnimationThread <> nil then
      FreeAndNil(FAnimationThread);

    FAnimationThread := TAnimationThread.Create(Self);
  end;
end;

procedure TFlowmotion.EnsureBitmapLoaded(Item: TImageItem);
begin
  if (FLoadMode = lmLazyAndFree) and (Item.FBitmap = nil) and (Item.FFileName <> '') and FileExists(Item.FFileName) then
  begin
    Item.FBitmap := TBitmap.Create;
    try
      LoadImageFromFile(Item.FFileName, Item.FBitmap);
    except
      Item.FBitmap.Free;
      Item.FBitmap := nil;
    end;
  end;
end;

procedure TFlowmotion.StopAnimationThread;
begin
  if FAnimationThread <> nil then
  begin
    FAnimationThread.Stop;
    FAnimationThread.WaitFor;
    FreeAndNil(FAnimationThread);
  end;
end;

{ Deselects the currently zoomed/selected image }
procedure TFlowmotion.DeselectZoomedImage;
var
  i: Integer;
begin
  //force all to end hotzoom
  for i := 0 to FImages.Count - 1 do
  begin
    TImageItem(FImages[i]).FHotZoomTarget := 1.0;
  end;

  // In free float mode, directly deselect without animation
  if FFlowLayout = flFreeFloat then
  begin
    // Clear selection directly
    if FSelectedImage <> nil then
    begin
      FSelectedImage.IsSelected := False;
      FSelectedImage := nil;
      FCurrentSelectedIndex := -1;
      FHotItem := nil;
    end;
    Invalidate;
  end
  else
    SetSelectedImage(nil, -1);
end;

{ Returns the total number of pages based on page size and total files }
function TFlowmotion.GetPageCount: Integer;
begin
  if FPageSize > 0 then
    Result := (FAllFiles.Count + FPageSize - 1) div FPageSize
  else
    Result := 1;
end;

{ Sets the image loading mode and refreshes the display }
procedure TFlowmotion.SetLoadMode(Value: TFlowmotionLoadMode);
begin
  if FLoadMode <> Value then
  begin
    FLoadMode := Value;
    ShowPage(0);
  end;
end;

{ SetFlowLayout - Sets the flow layout algorithm and recalculates layout }
procedure TFlowmotion.SetFlowLayout(Value: TFlowLayout);
begin
  if not visible then
    Exit;

  if FFlowLayout <> Value then
  begin
    DeselectZoomedImage;

    FFlowLayout := Value;

    // When switching to free float mode, ensure all images have positions
    if FFlowLayout = flFreeFloat then
    begin

      CalculateLayoutFreeFloat;
    end
    else
    begin
      // For other layout modes, recalculate the layout
      CalculateLayout;
    end;

    Invalidate;
  end;
end;


{ Sets the ImageEntryStyle for new images }
procedure TFlowmotion.SetImageEntryStyle(Value: TImageEntryStyle);
begin
  if FImageEntryStyle <> Value then
  begin
    FImageEntryStyle := Value;
    // no Invalidate needed – only affects newly added images
  end;
end;

{ Sets the color for hot-track border }
procedure TFlowmotion.SetHotTrackColor(Value: TColor);
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    Invalidate;
  end;
end;

{ Sets the color for selected image glow border }
procedure TFlowmotion.SetGlowColor(Value: TColor);
begin
  if FGlowColor <> Value then
  begin
    FGlowColor := Value;
    Invalidate;
  end;
end;

{ Sets the width of the glow border }
procedure TFlowmotion.SetGlowWidth(Value: Integer);
begin
  if FGlowWidth <> Value then
  begin
    FGlowWidth := Value;
    Invalidate;
  end;
end;

{ Sets the width of the hot border }
procedure TFlowmotion.SetHotTrackWidth(Value: Integer);
begin
  if FHotTrackWidth <> Value then
  begin
    FHotTrackWidth := Value;
    Invalidate;
  end;
end;

{ Enables or disables animations }
procedure TFlowmotion.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
  end;
end;

{ Adds a new activation zone }
procedure TFlowmotion.AddActivationZone(const AName: string; const ARect: TRect);
begin
  SetLength(FActivationZones, Length(FActivationZones) + 1);
  FActivationZones[High(FActivationZones)].Name := AName;
  FActivationZones[High(FActivationZones)].Rect := ARect;
end;
{ Clears all activation zones and resets the last zone name }

procedure TFlowmotion.ClearActivationZones;
begin
  SetLength(FActivationZones, 0);
  FLastActivationZoneName := '';
end;

{ Sets the priority for image loading threads }
procedure TFlowmotion.SetThreadPriority(Value: TThreadPriority);
begin
  FThreadPriority := Value;
  if Assigned(FAnimationThread) then
    FAnimationThread.Priority := Value;
end;


// Save positions to a file
procedure TFlowmotion.SavePositionsToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SavePositionsToStream(Stream);
  finally
    Stream.Free;
  end;
end;

// Save positions to a stream
procedure TFlowmotion.SavePositionsToStream(Stream: TStream);
var
  Writer: TWriter;
  i: Integer;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    // Number of images
    Writer.WriteInteger(FImages.Count);

    for i := 0 to FImages.Count - 1 do
      with TImageItem(FImages[i]) do
      begin
        Writer.WriteString(FileName);
        Writer.WriteString(Caption);
        Writer.WriteString(Path);

        Writer.WriteInteger(TargetRect.Left);
        Writer.WriteInteger(TargetRect.Top);
        Writer.WriteInteger(TargetRect.Right - TargetRect.Left);
        Writer.WriteInteger(TargetRect.Bottom - TargetRect.Top);
      end;
  finally
    Writer.Free;
  end;
end;

// Load positions from a file
procedure TFlowmotion.LoadPositionsFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  if not FileExists(FileName) then
    Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadPositionsFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

// Load positions from a stream with validation
procedure TFlowmotion.LoadPositionsFromStream(Stream: TStream);
var
  Reader: TReader;
  Count, i: Integer;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    Count := Reader.ReadInteger;
    // Add a sanity check for the count to prevent memory issues
    if (Count < 0) or (Count > 10000) then
      Exit; // Invalid data, abort loading

    SetLength(FLoadedPositions, Count);

    for i := 0 to Count - 1 do
    begin
      FLoadedPositions[i].FileName := Reader.ReadString;
      FLoadedPositions[i].Caption := Reader.ReadString;
      FLoadedPositions[i].Path := Reader.ReadString;

      FLoadedPositions[i].Left := Reader.ReadInteger;
      FLoadedPositions[i].Top := Reader.ReadInteger;
      FLoadedPositions[i].Width := Reader.ReadInteger;
      FLoadedPositions[i].Height := Reader.ReadInteger;
    end;
  finally
    Reader.Free;
  end;
end;


// Get current positions of all images
function TFlowmotion.GetCurrentPositions: TImagePositions;
var
  i: Integer;
begin
  if Length(FLoadedPositions) > 0 then
    Result := Copy(FLoadedPositions) // Return copy of loaded positions
  else
  begin
    // Generate positions from current images
    SetLength(Result, FImages.Count);

    for i := 0 to FImages.Count - 1 do
    begin
      with TImageItem(FImages[i]) do
      begin
        Result[i].FileName := FileName;
        Result[i].Caption := Caption;
        Result[i].Path := Path;
        Result[i].Left := TargetRect.Left;
        Result[i].Top := TargetRect.Top;
        Result[i].Width := TargetRect.Right - TargetRect.Left;
        Result[i].Height := TargetRect.Bottom - TargetRect.Top;
      end;
    end;
  end;
end;


// Reset all positions to default layout
procedure TFlowmotion.ResetPositions;
var
  i: Integer;
  ImageItem: TImageItem;
begin
  if FImages.Count = 0 then
    Exit;

  // Simply recalculate the layout
  CalculateLayout;

  // Update all image positions
  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    ImageItem.StartRect := ImageItem.CurrentRect;
    ImageItem.AnimationProgress := 0;
    ImageItem.Animating := True;
  end;

  Invalidate;
end;


{ Inserts a picture from TPicture with caption and path }
procedure TFlowmotion.InsertImage(Pic: TBitmap; const XFileName, XCaption, XPath: string);
begin
  if Pic = nil then
    Exit;
  try
    AddImage(Pic);
    if FImages.Count > 0 then
      with TImageItem(FImages[FImages.Count - 1]) do
      begin
        FileName := FileName;
        Caption := XCaption;
        Path := XPath;
      end;
  finally

  end;
end;

{ Centralized function to load an image file (JPG, PNG, BMP) into a TBitmap }
procedure TFlowmotion.LoadImageFromFile(const AFileName: string; ABitmap: TBitmap);
var
  PNG: TPNGObject;
  JPEGImage: TJPEGImage;
  Ext: string;
begin
  if not FileExists(AFileName) or (ABitmap = nil) then
    raise Exception.CreateFmt('File not found or target bitmap is nil: %s', [AFileName]);

  Ext := LowerCase(ExtractFileExt(AFileName));

  try
    if (Ext = '.jpg') or (Ext = '.jpeg') then
    begin
      JPEGImage := TJPEGImage.Create;
      try
        JPEGImage.LoadFromFile(AFileName);
        ABitmap.Assign(JPEGImage);
      finally
        JPEGImage.Free;
      end;
    end
    else if Ext = '.png' then
    begin
      PNG := TPNGObject.Create;
      try
        PNG.LoadFromFile(AFileName);
        ABitmap.Assign(PNG);
      finally
        PNG.Free;
      end;
    end
    else
    begin
      ABitmap.LoadFromFile(AFileName);
    end;
  except
    on E: Exception do
      // Re-raise with more context or handle silently if preferred
    //  raise Exception.CreateFmt('Failed to load image "%s". Error: %s', [AFileName, E.Message]);




  end;
end;

procedure TFlowmotion.PerformAnimationUpdate(DeltaMS: Cardinal);
var
  i: Integer;
  DeltaTime: Double;
  ImageItem: TImageItem;
  Progress, Eased, Speed: Double;
  AnyAnimating, AnyAnimatingAfter, AllFinishedAtStart, NeedRepaint: Boolean;
  TempRect: TRect;
  TempZoom, TempAlpha: Double;
  TargetZoom: Double;

  // -----------------------------------------------------------------------
  // Helper: compare two TRects for equality (pixel-exact)
  // -----------------------------------------------------------------------

  function RectsEqual(const A, B: TRect): Boolean;
  begin
    Result := (A.Left = B.Left) and (A.Top = B.Top) and (A.Right = B.Right) and (A.Bottom = B.Bottom);
  end;

  // -----------------------------------------------------------------------
  // Returns true when an image item has no more animation work left
  // (position, zoom, hot-zoom, breathing)
  // -----------------------------------------------------------------------
  function ItemFinished(const It: TImageItem): Boolean;
  begin
    Result := (It.AnimationProgress >= 1.0) and ((It.ZoomProgress <= 0.0001) or (It.ZoomProgress >= 0.9999)) and RectsEqual(It.CurrentRect, It.TargetRect) and (Abs(It.FHotZoom - It.FHotZoomTarget) <= 0.006);

    // Breathing is an endless animation ? never "finished" while active
    if FBreathingEnabled and (It = FSelectedImage) and (It = FHotItem) then
      Result := False;
  end;

begin
  // -----------------------------------------------------------------------
  // Early exit conditions – same as original timer
  // -----------------------------------------------------------------------
  if FInFallAnimation or FClearing or (not Visible) then
    Exit;

  // -----------------------------------------------------------------------
  // Convert milliseconds from thread to seconds
  // -----------------------------------------------------------------------
  DeltaTime := DeltaMS / 1000.0;
  if DeltaTime <= 0 then
    DeltaTime := 0.016; // fallback ~60 fps

  // -----------------------------------------------------------------------
  // Determine initial animation state
  // -----------------------------------------------------------------------
  AnyAnimating := FFallingOut;
  AllFinishedAtStart := not AnyAnimating;

  for i := 0 to FImages.Count - 1 do
    if not ItemFinished(TImageItem(FImages[i])) then
    begin
      AllFinishedAtStart := False;
      Break;
    end;

  NeedRepaint := False;

  try
    // =====================================================================
    // PHASE 1: Page fall-out animation (when changing pages)
    // =====================================================================
    if FFallingOut then
    begin
      FPageOutProgress := FPageOutProgress + (FAnimationSpeed / 100);
      if FPageOutProgress >= 1.0 then
      begin
        FPageOutProgress := 1.0;
        FFallingOut := False;
        FInFallAnimation := False;
      end;

      Eased := EaseInOutQuad(FPageOutProgress);

      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);

        TempRect := Rect(Round(ImageItem.StartRect.Left + (ImageItem.TargetRect.Left - ImageItem.StartRect.Left) * Eased), Round(ImageItem.StartRect.Top + (ImageItem.TargetRect.Top - ImageItem.StartRect.Top) * Eased), Round(ImageItem.StartRect.Right + (ImageItem.TargetRect.Right - ImageItem.StartRect.Right) * Eased), Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Eased));

        if not RectsEqual(ImageItem.CurrentRect, TempRect) then
        begin
          ImageItem.CurrentRect := TempRect;
          NeedRepaint := True;
        end;

        if ImageItem.Alpha <> 255 then
        begin
          ImageItem.Alpha := 255;
          NeedRepaint := True;
        end;
      end;
    end
    else
    begin
      // ===================================================================
      // PHASE 2: Normal item animations (move + zoom in/out)
      // ===================================================================
      for i := FImages.Count - 1 downto 0 do
      begin
        ImageItem := TImageItem(FImages[i]);

        // ----- Alpha (currently always 255, but kept for consistency) -----
        TempAlpha := 255;
        if Abs(ImageItem.Alpha - TempAlpha) > 0.5 then
        begin
          ImageItem.Alpha := Round(TempAlpha);
          NeedRepaint := True;
        end;

        // ----- Main position/scale animation progress -----
        if ImageItem.AnimationProgress < 1.0 then
        begin
          TempZoom := Min(1.0, ImageItem.AnimationProgress + FAnimationSpeed / 100);
          if Abs(ImageItem.AnimationProgress - TempZoom) > 0.001 then
          begin
            ImageItem.AnimationProgress := TempZoom;
            NeedRepaint := True;
          end;
        end;

        // ----- Selection zoom (big zoom when selected) -----
        if ImageItem.IsSelected then
          TempZoom := Min(1.0, ImageItem.ZoomProgress + FAnimationSpeed / 100)
        else if ImageItem.ZoomProgress > 0.0 then
          TempZoom := Max(0.0, ImageItem.ZoomProgress - FAnimationSpeed / 100)
        else
          TempZoom := ImageItem.ZoomProgress;

        if Abs(ImageItem.ZoomProgress - TempZoom) > 0.001 then
        begin
          ImageItem.ZoomProgress := TempZoom;
          NeedRepaint := True;
        end;

        // ----- Combined progress for position interpolation -----
        Progress := Max(ImageItem.AnimationProgress, ImageItem.ZoomProgress);
        if FAnimationEasing then
          Progress := EaseInOutQuad(Progress);

        TempRect := Rect(Round(ImageItem.StartRect.Left + (ImageItem.TargetRect.Left - ImageItem.StartRect.Left) * Progress), Round(ImageItem.StartRect.Top + (ImageItem.TargetRect.Top - ImageItem.StartRect.Top) * Progress), Round(ImageItem.StartRect.Right + (ImageItem.TargetRect.Right - ImageItem.StartRect.Right) * Progress), Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Progress));

        if not RectsEqual(ImageItem.CurrentRect, TempRect) then
        begin
          ImageItem.CurrentRect := TempRect;
          NeedRepaint := True;
        end;

        // Update per-item animating flag
        ImageItem.Animating := not ItemFinished(ImageItem);

        // Precise clearing of the previous selection to prevent flicker ---
        // We only clear the FWasSelectedItem reference when the zoom-out animation
        // is definitively finished. This prevents the item from losing its
        // "special" status mid-animation, which causes a one-frame flicker.
        if (ImageItem = FWasSelectedItem) and (ImageItem.ZoomProgress <= 0.0001) and // Must be fully zoomed out
          RectsEqual(ImageItem.CurrentRect, ImageItem.TargetRect) then
            // Must be at its final position
        begin
          FWasSelectedItem := nil;
        end;
      end;
    end;

    // ----- Alpha (for Fade effects) -----
    if ImageItem.Alpha <> ImageItem.TargetAlpha then
    begin
      TempAlpha := ImageItem.Alpha;
      if ImageItem.IsSelected then
        TempAlpha := Min(255, ImageItem.Alpha + FAnimationSpeed)
      else if ImageItem.Alpha > 0 then
        TempAlpha := Max(0, ImageItem.Alpha - FAnimationSpeed);

      if Abs(ImageItem.Alpha - TempAlpha) > 0.5 then
      begin
        ImageItem.Alpha := Round(TempAlpha);
        NeedRepaint := True;
      end;
    end;

    // ===================================================================
    // PHASE 2.5: Hot-track zoom + Breathing animation
    // ===================================================================
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);

      if (not FHotTrackZoom) and (ImageItem <> FSelectedImage) or (FDraggingSelected and (ImageItem = FSelectedImage)) then
      begin
        if ImageItem.FHotZoom <> 1.0 then
        begin
          if not (FDraggingSelected and (ImageItem = FSelectedImage)) then
            ImageItem.FHotZoom := 1.0;
          ImageItem.FHotZoomTarget := 1.0;
          NeedRepaint := True;
        end;
      // Skip the rest of the hot-zoom logic for this item.
        Continue;
      end;

      if not (ImageItem.Visible and HotTrackZoom) then
        Continue;

      // Breathing when selected AND hovered
      if FBreathingEnabled and (ImageItem = FSelectedImage) and (ImageItem = FHotItem) and (not FDraggingSelected) then
        TargetZoom := 1.0 + BREATHING_AMPLITUDE * 0.2 * (Sin(FBreathingPhase * 2 * Pi) + 1.0)

        // Normal hot-zoom when only hovered
      else if (ImageItem = FHotItem) and HotTrackZoom and (not FDraggingSelected) then
        TargetZoom := HOT_ZOOM_MAX_FACTOR
      else
        TargetZoom := 1.0;

      // Choose speed (in = faster when breathing)
      if ImageItem.FHotZoom < TargetZoom then
        Speed := HOT_ZOOM_IN_PER_SEC
      else
        Speed := HOT_ZOOM_OUT_PER_SEC;

      // Smooth approach
      ImageItem.FHotZoom := ImageItem.FHotZoom + (TargetZoom - ImageItem.FHotZoom) * Speed * DeltaTime;
      if HotTrackZoom then
        ImageItem.FHotZoomTarget := TargetZoom // for ItemFinished check
      else
        ImageItem.FHotZoomTarget := 1.0;

      // Clamp non-breathing hotzoom
      if (ImageItem <> FSelectedImage) and (ImageItem.FHotZoom > HOT_ZOOM_MAX_FACTOR) then
        ImageItem.FHotZoom := HOT_ZOOM_MAX_FACTOR;
      if ImageItem.FHotZoom < 1.0 then
        ImageItem.FHotZoom := 1.0;

      NeedRepaint := True;
    end;

    // Advance breathing phase only when selected item is hovered
    if FBreathingEnabled and (FHotItem <> nil) and (FHotItem = FSelectedImage) then
      FBreathingPhase := Frac(FBreathingPhase + BREATHING_SPEED_PER_SEC * DeltaTime);

    // ===================================================================
    // PHASE 3: Final decision – repaint + stop condition (Delphi 7 safe)
    // ===================================================================
    AnyAnimatingAfter := FFallingOut;
    for i := 0 to FImages.Count - 1 do
    begin
      if not ItemFinished(TImageItem(FImages[i])) then
      begin
        AnyAnimatingAfter := True;
        Break;
      end;
    end;

    // At the end of the method, ensure proper synchronization
    if NeedRepaint or AnyAnimatingAfter then
      ThreadSafeInvalidate;

    if not AnyAnimatingAfter and not AllFinishedAtStart and Assigned(FOnAllAnimationsFinished) then
      ThreadSafeFireAllAnimationsFinished;

  except
    on E: Exception do
    begin
      // Swallow exceptions – we never want the background thread to die
      // (same behavior as original timer)
    end;
  end;
end;

{ not remove, needed for animationthread }
procedure TFlowmotion.ThreadSafeInvalidate;
begin
  if not FClearing then
  begin
    if GetCurrentThreadId = MainThreadId then
      Invalidate // We're already in the main thread
    else
      PostMessage(Handle, WM_USER + 1, 0, 0); // Send a message to invalidate
  end;
end;

{ not remove, needed for animationthread }
procedure TFlowmotion.ThreadSafeFireAllAnimationsFinished;
begin
  if Assigned(FOnAllAnimationsFinished) then
  begin
    if GetCurrentThreadId = MainThreadId then
      FOnAllAnimationsFinished(Self)
    else
      PostMessage(Handle, WM_USER + 2, 0, 0); // Send a message to fire the event
  end;
end;

{ not remove, needed for animationthread ^}
procedure TFlowmotion.WMUser1(var Message: TMessage);
begin
  if not FClearing then
    Invalidate;
end;

{ not remove, needed for animationthread ^}
procedure TFlowmotion.WMUser2(var Message: TMessage);
begin
  if Assigned(FOnAllAnimationsFinished) then
    FOnAllAnimationsFinished(Self);
end;

{ Returns the bitmap at the specified index, or nil if index is invalid }
function TFlowmotion.GetPicture(index: Integer): TBitmap;
begin
  if (index >= 0) and (index < FImages.Count) then
    Result := TImageItem(FImages[index]).Bitmap
  else
    Result := nil;
end;

{ Creates window parameters with clipping enabled }
procedure TFlowmotion.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CLIPCHILDREN;
end;

{ Returns the number of currently visible images }
function TFlowmotion.GetImageCount: Integer;
begin
  Result := FImages.Count;
end;

{ Returns the number of currently loading images }
function TFlowmotion.GetLoadingCount: Integer;
begin
  Result := FLoadingCount;
end;

{ Called when an image loading thread finishes (thread-safe cleanup) }
procedure TFlowmotion.ThreadFinished(Thread: TImageLoadThread);
begin
  // Check if component is being destroyed
  if csDestroying in ComponentState then
    Exit;
  // Thread-safe removal (already in main thread via Synchronize)
  try
    if FLoadingThreads.IndexOf(Thread) >= 0 then
    begin
      FLoadingThreads.Remove(Thread);
      if FLoadingCount > 0 then
        InterlockedDecrement(FLoadingCount);
    end;
  except
    // Ignore errors during thread cleanup
  end;
end;


// ---------------------------------------------------------------------
// FREE BITMAPS OF NON-VISIBLE IMAGES (only in LazyAndFree mode)
// Called from ShowPage after old images are removed from FImages
// ---------------------------------------------------------------------
procedure TFlowmotion.FreeInvisibleBitmaps;
var
  i: Integer;
  Item: TImageItem;
  AbsIndex: Integer;
  PageStart, PageEnd: Integer;
begin
  if FLoadMode <> lmLazyAndFree then
    Exit;

  PageStart := GetPageStartIndex;
  PageEnd := PageStart + FPageSize - 1;

  // Scan ALL loaded images (they may still exist in the old page cache)
  for i := 0 to FImages.Count - 1 do
  begin
    Item := TImageItem(FImages[i]);
    AbsIndex := PageStart + i;

    // If this image is NOT on the current page ? free its bitmap
    if (AbsIndex < PageStart) or (AbsIndex > PageEnd) then
    begin
      if Assigned(Item.FBitmap) then
      begin
        Item.FBitmap.Free;
        Item.FBitmap := nil;  // Mark as unloaded
      end;
    end;
  end;
end;

{ Fires the OnImageLoad event }
procedure TFlowmotion.DoImageLoad(const FileName: string; Success: Boolean);
begin
  if Assigned(FOnImageLoad) then
  begin
    try
      FOnImageLoad(Self, FileName, Success);
    except
      // Prevent callback exceptions from crashing the component
    end;
  end;
end;

{ Enable or disable breathing of selected image on mouseover }
procedure TFlowmotion.SetBreathingEnabled(Value: Boolean);
begin
  if FBreathingEnabled = Value then
    Exit;
  FBreathingEnabled := Value;
  if not FBreathingEnabled and (FSelectedImage <> nil) and (FSelectedImage = FHotItem) then
  begin
    FSelectedImage.FHotZoomTarget := 1.0;
    FBreathingPhase := 0.0;
  end;
  Invalidate;
end;

{ Sets up animation for a new image entering the gallery }
procedure TFlowmotion.AnimateImage(ImageItem: TImageItem; EntryStyle: TImageEntryStyle; UseSavedPosition: Boolean; SavedTargetRect: TRect);
var
  StartX, StartY, W, H: Integer;
  Target: TRect;
  EffectiveStyle: TImageEntryStyle;
  CenterX, CenterY: Integer;
begin
  if ImageItem = nil then
    Exit;

  // Use saved target position if requested, otherwise use item's current target
  if UseSavedPosition then
    Target := SavedTargetRect
  else
    Target := ImageItem.TargetRect;

  W := Target.Right - Target.Left;
  H := Target.Bottom - Target.Top;

  // Resolve random direction once
  EffectiveStyle := EntryStyle;
  if EffectiveStyle = iesRandom then
    EffectiveStyle := TImageEntryStyle(Random(8) + 1);
      // 1..8 = the eight side directions

  // --------------------------------------------------------------
  // Calculate starting position depending on entry style
  // --------------------------------------------------------------
  case EffectiveStyle of
    iesFromLeft:
      begin
        StartX := -W - 100; // far outside left
        StartY := Target.Top + (Target.Bottom - Target.Top - H) div 2;
      end;
    iesFromRight:
      begin
        StartX := Width + 100; // far outside right
        StartY := Target.Top + (Target.Bottom - Target.Top - H) div 2;
      end;
    iesFromTop:
      begin
        StartX := Target.Left + (Target.Right - Target.Left - W) div 2;
        StartY := -H - 100; // far above
      end;
    iesFromBottom:
      begin
        StartX := Target.Left + (Target.Right - Target.Left - W) div 2;
        StartY := Height + 100; // far below
      end;
    iesFromTopLeft:
      begin
        StartX := -W - 100;
        StartY := -H - 100;
      end;
    iesFromTopRight:
      begin
        StartX := Width + 100;
        StartY := -H - 100;
      end;
    iesFromBottomLeft:
      begin
        StartX := -W - 100;
        StartY := Height + 100;
      end;
    iesFromBottomRight:
      begin
        StartX := Width + 100;
        StartY := Height + 100;
      end;
    iesFromCenter:
      begin
        CenterX := Target.Left + (Target.Right - Target.Left) div 2;
        CenterY := Target.Top + (Target.Bottom - Target.Top) div 2;
        StartX := CenterX;
        StartY := CenterY;
      end;
    iesFromPoint:
      begin
        CenterX := FEntryPoint.X;
        CenterY := FEntryPoint.Y;
        StartX := CenterX;
        StartY := CenterY;
      end;
  else
    StartX := Target.Left;
    StartY := Target.Top;
  end;

  // --------------------------------------------------------------
  // All new images start tiny and grow to full size
  // --------------------------------------------------------------
  // For center/point we start literally as a dot
  if (EffectiveStyle in [iesFromCenter, iesFromPoint]) then
  begin
    ImageItem.StartRect := Rect(StartX, StartY, StartX, StartY); // single pixel
    ImageItem.CurrentRect := ImageItem.StartRect;
  end
  else
  begin
    // Start tiny but with correct aspect ratio
    ImageItem.StartRect := Rect(StartX + Round(W * (1 - START_SCALE) / 2), StartY + Round(H * (1 - START_SCALE) / 2), StartX + Round(W * START_SCALE) + Round(W * (1 - START_SCALE) / 2), StartY + Round(H * START_SCALE) + Round(H * (1 - START_SCALE) / 2));
    ImageItem.CurrentRect := ImageItem.StartRect;
  end;

  // Force hot-zoom animation from tiny to normal
  ImageItem.FHotZoom := START_SCALE; // start tiny
  ImageItem.FHotZoomTarget := 1.0; // grow to normal size

  // Set the target rect (either saved or current)
  ImageItem.TargetRect := Target;
  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;
  ImageItem.Alpha := 255;
  ImageItem.TargetAlpha := 255;
end;

{ Enable or disable breathing of selected image on mouseover }
procedure TFlowmotion.SetZoomSelectedtoCenter(Value: Boolean);
begin
  if FZoomSelectedtoCenter = Value then
    Exit;
  FZoomSelectedtoCenter := Value;
  CalculateLayout;
  Invalidate;
end;

{ Sets animation speed (1-100, higher = faster) }
procedure TFlowmotion.SetAnimationSpeed(const Value: Integer);
begin
  if (Value > 0) and (Value <= 100) then
    FAnimationSpeed := Value;
end;

{ Sets spacing between images and recalculates layout }
procedure TFlowmotion.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    CalculateLayout;
    Invalidate;
  end;
end;

{ Loads and sets a background picture from file (supports JPG, PNG, BMP) }
procedure TFlowmotion.SetBackgroundpicture(const Path: string);
var
  TempBitmap: TBitmap;
begin
  try
    if FileExists(Path) then
    begin
      TempBitmap := TBitmap.Create;
      try
        // Call the centralized loading function
        LoadImageFromFile(Path, TempBitmap);
        FBackgroundpicture.Assign(TempBitmap);
      finally
        TempBitmap.Free;
      end;
    end
    else
      FBackgroundpicture.Assign(nil);
  except
    on E: Exception do
      FBackgroundpicture.Assign(nil);
  end;
  FBackgroundCacheValid := False;
  Invalidate;
end;

{ Enables or disables hot-track zoom effect }
procedure TFlowmotion.SetHotTrackZoom(const Value: Boolean);
var
  i: Integer;
begin
  if FHotTrackZoom <> Value then
  begin
    FHotTrackZoom := Value;
    // When HotTrackZoom is disabled, all active hot-zooms must be
    // immediately reset to prevent a "glow-after-effect".
    if not FHotTrackZoom then
    begin
      // Reset FHotItem so it's no longer considered "hot".
      if FHotItem <> nil then
      begin
        // Only reset if it's not the selected image, which might have
        // its own animation (e.g., breathing).
        if FHotItem <> FSelectedImage then
        begin
          FHotItem.FHotZoom := 1.0;
          FHotItem.FHotZoomTarget := 1.0;
        end;
        FHotItem := nil;
      end;
      // Reset all other images that might still be animating.
      for i := 0 to FImages.Count - 1 do
      begin
        // The selected image is excluded here as it might have the
        // "breathing" effect, which is independent of hot-tracking.
        if (TImageItem(FImages[i]).FHotZoom > 1.0) and (TImageItem(FImages[i]) <> FSelectedImage) then
        begin
          TImageItem(FImages[i]).FHotZoom := 1.0;
          TImageItem(FImages[i]).FHotZoomTarget := 1.0;
        end;
      end;
    end;
    Invalidate;
  end;
end;

{ Sets whether images should maintain aspect ratio when resized }
procedure TFlowmotion.SetKeepAspectRatio(const Value: Boolean);
begin
  if FKeepAspectRatio <> Value then
  begin
    FKeepAspectRatio := Value;
    CalculateLayout;
    Invalidate;
  end;
end;

{ Sets the background color }
procedure TFlowmotion.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundCacheValid := False;
    FBackgroundColor := Value;
    Color := Value;
    Invalidate;
  end;
end;

{ Sets maximum number of columns in grid (0 = auto) }
procedure TFlowmotion.SetMaxColumns(const Value: Integer);
begin
  if FMaxColumns <> Value then
  begin
    FMaxColumns := Value;
    CalculateLayout;
    Invalidate;
  end;
end;

{ Sets maximum number of rows in grid (0 = auto) }
procedure TFlowmotion.SetMaxRows(const Value: Integer);
begin
  if FMaxRows <> Value then
  begin
    FMaxRows := Value;
    CalculateLayout;
    Invalidate;
  end;
end;

{ Easing function for smooth animations (quadratic ease-in-out) }
function TFlowmotion.EaseInOutQuad(t: Double): Double;
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

{ Returns selected animation direction }
function TFlowmotion.GetEntryDirection: TImageEntryStyle;
begin
  Result := TImageEntryStyle(Ord(FImageEntryStyle));
end;

{
  Moves an image from one position to another within the current page.
  The indices are relative to the FImages list (0-based).
  The change is permanent and updates all internal lists.
}
// ---------------------------------------------------------------------
// Moves an image within the current page (relative indices)
// Updates master lists ? change is permanent across pages and restarts
// Fully safe with async loading, paging, selection, and saved positions
// ---------------------------------------------------------------------
procedure TFlowmotion.MoveImageToPos(RelIndexFrom, RelIndexTo: Integer);
var
  PageStart: Integer;
  AbsIndexFrom, AbsIndexTo: Integer;
  Item: TImageItem;
  FileName, Caption, Path: string;
  OldSelectedIndex: Integer;
begin
  // --- 1. Safety: never move during animation or page change ---
  if FInFallAnimation or FPageChangeInProgress or AnimationsRunning then
    Exit;

  // --- 2. Validate relative indices ---
  if (RelIndexFrom < 0) or (RelIndexFrom >= FImages.Count) or (RelIndexTo < 0) or (RelIndexTo >= FImages.Count) or (RelIndexFrom = RelIndexTo) then
    Exit;

  // --- 3. Move in visible list (FImages) ---
  Item := TImageItem(FImages[RelIndexFrom]);
  FImages.Delete(RelIndexFrom);
  FImages.Insert(RelIndexTo, Item);

  // --- 4. Calculate absolute indices in master lists ---
  PageStart := GetPageStartIndex;
  AbsIndexFrom := PageStart + RelIndexFrom;
  AbsIndexTo := PageStart + RelIndexTo;

  // --- 5. Move in ALL master lists (permanent change) ---
  // FAllFiles
  FileName := FAllFiles[AbsIndexFrom];
  FAllFiles.Delete(AbsIndexFrom);
  FAllFiles.Insert(AbsIndexTo, FileName);

  // FAllCaptions
  Caption := FAllCaptions[AbsIndexFrom];
  FAllCaptions.Delete(AbsIndexFrom);
  FAllCaptions.Insert(AbsIndexTo, Caption);

  // FAllPaths
  Path := FAllPaths[AbsIndexFrom];
  FAllPaths.Delete(AbsIndexFrom);
  FAllPaths.Insert(AbsIndexTo, Path);

  // --- 6. Fix selected index (critical!) ---
  OldSelectedIndex := FCurrentSelectedIndex;

  if OldSelectedIndex = RelIndexFrom then
    FCurrentSelectedIndex := RelIndexTo
  else if (OldSelectedIndex > RelIndexFrom) and (OldSelectedIndex <= RelIndexTo) then
    Dec(FCurrentSelectedIndex)
  else if (OldSelectedIndex >= RelIndexTo) and (OldSelectedIndex < RelIndexFrom) then
    Inc(FCurrentSelectedIndex);

  // --- 7. Refresh layout and repaint ---
  CalculateLayout;
  Invalidate;
end;

{ Adds an image from file (uses filename as caption and path) }
procedure TFlowmotion.AddImage(const FileName: string);
begin
  AddImage(FileName, ExtractFileName(FileName), FileName);
end;

{ Adds an image from file with caption and path }
procedure TFlowmotion.AddImage(const FileName, ACaption, APath: string);
var
  Bitmap: TBitmap;
  IsLastPage: Boolean;
  IsSpaceOnPage: Boolean;
  WasEmpty: Boolean;
  NewItem: TImageItem;
  DefaultWidth, DefaultHeight, ColCount, RowCount: Integer;
  NewX, NewY: Integer;
  MaxAttempts: Integer;
  FoundPosition: Boolean;
  ExistingRect: TRect;
  i, j: Integer;
  TempRect: TRect;
begin
  WasEmpty := (FAllFiles.Count = 0);
  // Add to master list if not already present
  FAllFiles.Add(FileName);
  FAllCaptions.Add(ACaption);
  FAllPaths.Add(APath);
  if WasEmpty or (FLoadMode = lmLoadAll) then
  begin
    ShowPage(FCurrentPage);
    Exit;
  end;
  IsLastPage := (FCurrentPage = GetPageCount - 1);
  IsSpaceOnPage := (FImages.Count < FPageSize);
  // Only load image if on last page and there's space
  if IsLastPage and IsSpaceOnPage then
  begin
    if not FileExists(FileName) then
    begin
      DoImageLoad(FileName, False);
      Exit;
    end;
    Bitmap := TBitmap.Create;
    try
      LoadImageFromFile(FileName, Bitmap);
      NewItem := TImageItem.Create;
      NewItem.Bitmap.Assign(Bitmap);
      NewItem.Caption := ACaption;
      NewItem.Path := APath;
      NewItem.FileName := FileName;
      NewItem.Direction := GetEntryDirection;
      NewItem.Visible := False;
      FImages.Add(NewItem);
      CheckSynchronize(10);
      if Visible then
      begin
        // In FreeFloat-Modus only calc pic pos, not full layout
        if FFlowLayout = flFreeFloat then
        begin
          // std size for new pic
          DefaultWidth := Max(150, Width div 6);
          DefaultHeight := Max(150, Height div 6);
          ColCount := Max(1, Width div (DefaultWidth + 20));
          RowCount := Max(1, Height div (DefaultHeight + 20));
          // Try to find a valid position for the new pic
          MaxAttempts := 100;
          FoundPosition := False;
          // First try the default grid position
          NewX := 20 + ((FImages.Count - 1) mod ColCount) * (DefaultWidth + 20);
          NewY := 20 + ((FImages.Count - 1) div ColCount) * (DefaultHeight + 20);
          // Check if this position is within bounds
          if (NewX + DefaultWidth <= Width - 20) and (NewY + DefaultHeight <= Height - 20) then
          begin
            // Check for overlap with existing images
            FoundPosition := True;
            NewItem.TargetRect := Rect(NewX, NewY, NewX + DefaultWidth, NewY + DefaultHeight);
            for i := 0 to FImages.Count - 2 do // -2 because we haven't added the current image yet
            begin
              ExistingRect := TImageItem(FImages[i]).TargetRect;
              if IntersectRect(ExistingRect, ExistingRect, NewItem.TargetRect) then
              begin
                FoundPosition := False;
                Break;
              end;
            end;
          end;
          // If default position doesn't work, try random positions
          if not FoundPosition then
          begin
            for i := 1 to MaxAttempts do
            begin
              NewX := 20 + Random(Width - DefaultWidth - 40);
              NewY := 20 + Random(Height - DefaultHeight - 40);
              NewItem.TargetRect := Rect(NewX, NewY, NewX + DefaultWidth, NewY + DefaultHeight);

            // Check if the placement overlaps with the user-defined free area
              TempRect := NewItem.TargetRect;
              if not IsRectEmpty(FKeepAreaFreeRect) and IntersectRect(TempRect, NewItem.TargetRect, FKeepAreaFreeRect) then
              begin
              // Overlap found. Try a new position in the next iteration of the random loop.
                Continue;
              end;

            // Check for overlap with existing images
              FoundPosition := True;
              for j := 0 to FImages.Count - 2 do
              begin
                ExistingRect := TImageItem(FImages[j]).TargetRect;
                if IntersectRect(ExistingRect, ExistingRect, NewItem.TargetRect) then
                begin
                  FoundPosition := False;
                  Break;
                end;
              end;

              if FoundPosition then
                Break; // Found a valid position, exit the random attempt loop
            // If we still couldn't find a good position, just place it at a random position
            end;

            if not FoundPosition then
            begin
              NewX := 20 + Random(Width - DefaultWidth - 40);
              NewY := 20 + Random(Height - DefaultHeight - 40);
              NewItem.TargetRect := Rect(NewX, NewY, NewX + DefaultWidth, NewY + DefaultHeight);
            end;

          end;

          // Animation for new pic
          AnimateImage(NewItem, NewItem.Direction, false, Rect(0, 0, 0, 0));
        end
        else
        begin
          CalculateLayout;
          AnimateImage(NewItem, NewItem.Direction, false, Rect(0, 0, 0, 0));
        end;
        NewItem.Visible := True;
      end;
      StartAnimationThread;
    finally
      Bitmap.Free;
    end;
  end
  else
  begin
    // only add to Masterliste, lazyload later
    DoImageLoad(FileName, True);
  end;
end;

{ Handles keyboard input for navigation }
procedure TFlowmotion.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if AnimationsRunning then
    Exit;
  case Key of
    VK_LEFT, VK_UP:
      SelectPreviousImage;
    VK_RIGHT, VK_DOWN:
      SelectNextImage;
    VK_RETURN:
      if (FSelectedImage <> nil) then
      begin
        if Assigned(FOnSelectedImageDblClick) then
          FOnSelectedImageDblClick(Self, FSelectedImage, FCurrentSelectedIndex);
        inherited DblClick;
      end;
    VK_ESCAPE:
      SetSelectedImage(nil, -1);
    VK_HOME:
      if FImages.Count > 0 then
        SetSelectedImage(TImageItem(FImages[0]), 0);
    VK_END:
      if FImages.Count > 0 then
        SetSelectedImage(TImageItem(FImages[FImages.Count - 1]), FImages.Count - 1);
    VK_PRIOR:
      SelectPreviousImage; // Page Up
    VK_NEXT:
      SelectNextImage; // Page Down
  end;
  inherited KeyDown(Key, Shift);
end;

{ Adds a bitmap directly to the gallery }
procedure TFlowmotion.AddImage(Bitmap: TBitmap);
var
  ImageItem: TImageItem;
begin
  if (Bitmap = nil) or Bitmap.Empty then
    Exit;
  ImageItem := TImageItem.Create;
  ImageItem.Bitmap.Assign(Bitmap);
  ImageItem.Direction := GetEntryDirection; // Set entry direction
  FImages.Add(ImageItem);
  if Visible then
  begin
    CalculateLayout;

    // Set up the entry animation AFTER layout calculation
    AnimateImage(ImageItem, ImageItem.Direction, false, Rect(0, 0, 0, 0));

    ImageItem.Visible := True;
  end;
  StartAnimationThread;
end;

{ Scrolls to and selects an image by absolute index (switches page if needed) }
procedure TFlowmotion.ScrollToIndex(Index: Integer; Animate: Boolean = True);
var
  TargetPage, RelativeIndex: Integer;
  Item: TImageItem;
begin
  if (Index < 0) or (Index >= FAllFiles.Count) then
    Exit;

  TargetPage := Index div FPageSize;
  RelativeIndex := Index mod FPageSize;
  // If index is not on current page, switch to the correct page
  if TargetPage <> FCurrentPage then
  begin
    ShowPage(TargetPage);
    while FPageChangeInProgress do
    begin
      Application.ProcessMessages;
      Sleep(4);
    end;
  end;
  // Select the image on current page
  if (RelativeIndex < FImages.Count) then
  begin
    Item := TImageItem(FImages[RelativeIndex]);
    if Animate then
      SetSelectedImage(Item, RelativeIndex)
    else
    begin
      FCurrentSelectedIndex := RelativeIndex;
      FSelectedImage := Item;
      if Item <> nil then
        Item.IsSelected := True;
      Invalidate;
    end;
  end;
end;

{ Adds an image asynchronously using a background thread }
procedure TFlowmotion.AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = '');
var
  LoadThread: TImageLoadThread;
  SystemInfo: TSystemInfo;
  CoreToUse: Integer;
begin
  FAllFiles.Add(FileName);
  FAllCaptions.Add(ACaption);
  FAllPaths.Add(APath);

  GetSystemInfo(SystemInfo);
  // Use Round-Robin to distribute across all available cores
  CoreToUse := FNextLoaderCoreIndex mod SystemInfo.dwNumberOfProcessors;
  Inc(FNextLoaderCoreIndex);

  LoadThread := TImageLoadThread.Create(FileName, ACaption, APath, Self, CoreToUse);
  LoadThread.Priority := FThreadPriority;
  FLoadingThreads.Add(LoadThread);
  Inc(FLoadingCount);
end;

{ Adds multiple images synchronously (waits for all to load) }
procedure TFlowmotion.AddImages(const FileNames, Captions, Paths: TStringList);
var
  i: Integer;
  WasEmpty: Boolean;
begin
  WasEmpty := (FAllFiles.Count = 0);
  WaitForAllLoads;
  FLoadingCount := 0;
  // Add all files to master list
  for i := 0 to FileNames.Count - 1 do
  begin
    FAllFiles.Add(FileNames[i]);
    FAllCaptions.Add(Captions[i]);
    FAllPaths.Add(Paths[i]);
  end;
  if WasEmpty then
    ShowPage(FCurrentPage); //else  to do
end;

{ Adds multiple images asynchronously (does not wait) }
procedure TFlowmotion.AddImagesAsync(const FileNames, Captions, Paths: TStringList);
var
  i: Integer;
  Thread: TImageLoadThread;
  SystemInfo: TSystemInfo;
  CoreToUse: Integer;
begin
  FLoadingCount := 0;
  GetSystemInfo(SystemInfo);
  // Add all files to master list
  for i := 0 to FileNames.Count - 1 do
  begin
    FAllFiles.Add(FileNames[i]);
    FAllCaptions.Add(Captions[i]);
    FAllPaths.Add(Paths[i]);
  end;
  // Start loading threads
  for i := 0 to FileNames.Count - 1 do
  begin
    CoreToUse := FNextLoaderCoreIndex mod SystemInfo.dwNumberOfProcessors;
    Inc(FNextLoaderCoreIndex);
    Thread := TImageLoadThread.Create(FileNames[i], Captions[i], Paths[i], Self, CoreToUse);
    FLoadingThreads.Add(Thread);
    Inc(FLoadingCount);
  end;
end;

{ Sets whether images should be sorted by size (False) or keep load order (True) }
procedure TFlowmotion.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    CalculateLayout;
    Invalidate;
  end;
end;

{ Sets whether to keep center space free for zoomed images }
procedure TFlowmotion.SetKeepSpaceforZoomed(Value: Boolean);
begin
  if FKeepSpaceforZoomed <> Value then
  begin
    FKeepSpaceforZoomed := Value;
    CalculateLayout;
    Invalidate;
  end;
end;

{ Sets whether selected image can be dragged (feature not fully implemented) }
procedure TFlowmotion.SetSelectedMovable(Value: Boolean);
var
  CurrentSelectedItem: TImageItem;
  CurrentSelectedIndex: Integer;
begin
  // If dragging is being disabled, and an item is currently selected...
  if (not Value) and FSelectedMovable and (FSelectedImage <> nil) then
  begin
    // ...store the current selection
    CurrentSelectedItem := FSelectedImage;
    CurrentSelectedIndex := FCurrentSelectedIndex;

    // ...deselect it. This will stop any ongoing dragging state.
    SetSelectedImage(nil, -1);

    // ...immediately re-select it. This forces it to snap back to its centered position.
    SetSelectedImage(CurrentSelectedItem, CurrentSelectedIndex);
  end;

  // Finally, set the new value for the property.
  if FSelectedMovable <> Value then
  begin
    FSelectedMovable := Value;
    Invalidate;
  end;
end;

{ Clears all images with optional animation and zoom effect }
procedure TFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean = false);
begin
  try
    Clear(animated, ZoominSelected, Rect(0, 0, 0, 0), Rect(0, 0, 0, 0), iesFromBottom);
  except

  end;
end;

/// <summary>
///   Clears all images with optional animation.
///   Non-selected images fly out according to FallingStyle.
///   If ZoominSelected = True, the selected image flies and shrinks to SelectedTargetPos.
/// </summary>
/// <param name="animated">True = show fly-out animation</param>
/// <param name="ZoominSelected">True = selected image gets special animation to SelectedTargetPos</param>
/// <param name="SelectedTargetPos">Target rectangle for selected image</param>
/// <param name="FallingTargetPos">Target point for iesFromPoint style</param>
/// <param name="FallingStyle">Fly-out direction for normal images</param>
procedure TFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom);
var
  i: Integer;
  StartTick: DWORD;
  ImageItem: TImageItem;
  AllOut: Boolean;
  ShrinkFactor: Real;
  R: TRect;
  AnimSpeed, NewW, NewH, CurCX, CurCY, CurW, CurH, TargetCX, TargetCY, MoveX, MoveY: Integer;
  SelectedItem: TImageItem;
begin
  if (FImages.Count = 0) or FClearing or FInFallAnimation then
    Exit;

  AnimSpeed := 12;

  WaitForAllLoads;

  StopAnimationThread;
  FInFallAnimation := True;

  // Stop all loading threads
  for i := 0 to FLoadingThreads.Count - 1 do
  begin
    try
      TImageLoadThread(FLoadingThreads[i]).Terminate;
    except
    end;
  end;
  FLoadingThreads.Clear;
  FLoadingCount := 0;

  // Find selected item
  SelectedItem := nil;
  if ZoominSelected then
    for i := 0 to FImages.Count - 1 do
      if TImageItem(FImages[i]).IsSelected then
      begin
        SelectedItem := TImageItem(FImages[i]);
        Break;
      end;

  if not animated then
  begin
    FreeAllImagesAndClearLists;
    Exit;
  end;

  // ==============================================================
  // ANIMATION LOOP – handles normal images AND selected image
  // ==============================================================
  StartTick := GetTickCount;
  repeat
    AllOut := True;

    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if not ImageItem.Visible then
        Continue;

      R := ImageItem.CurrentRect;

      // --------------------------------------------------------------
      // Normal images OR selected image without special target
      // --------------------------------------------------------------
      if (ImageItem <> SelectedItem) or (not ZoominSelected) or IsRectEmpty(SelectedTargetPos) then
      begin
        case FallingStyle of
          iesFromTop:
            OffsetRect(R, 0, -Trunc(AnimSpeed * AnimSpeed));
          iesFromBottom:
            OffsetRect(R, 0, Trunc(AnimSpeed * AnimSpeed));
          iesFromLeft:
            OffsetRect(R, -Trunc(AnimSpeed * AnimSpeed), 0);
          iesFromRight:
            OffsetRect(R, Trunc(AnimSpeed * AnimSpeed), 0);
          iesFromTopLeft:
            OffsetRect(R, -Trunc(AnimSpeed * AnimSpeed), -Trunc(AnimSpeed * AnimSpeed));
          iesFromTopRight:
            OffsetRect(R, Trunc(AnimSpeed * AnimSpeed), -Trunc(AnimSpeed * AnimSpeed));
          iesFromBottomLeft:
            OffsetRect(R, -Trunc(AnimSpeed * AnimSpeed), Trunc(AnimSpeed * AnimSpeed));
          iesFromBottomRight:
            OffsetRect(R, Trunc(AnimSpeed * AnimSpeed), Trunc(AnimSpeed * AnimSpeed));
          iesRandom:
            case Random(8) of
              0:
                OffsetRect(R, 0, -Trunc(AnimSpeed * AnimSpeed));
              1:
                OffsetRect(R, 0, Trunc(AnimSpeed * AnimSpeed));
              2:
                OffsetRect(R, -Trunc(AnimSpeed * AnimSpeed), 0);
              3:
                OffsetRect(R, Trunc(AnimSpeed * AnimSpeed), 0);
              4:
                OffsetRect(R, -Trunc(AnimSpeed * AnimSpeed), -Trunc(AnimSpeed * AnimSpeed));
              5:
                OffsetRect(R, Trunc(AnimSpeed * AnimSpeed), -Trunc(AnimSpeed * AnimSpeed));
              6:
                OffsetRect(R, -Trunc(AnimSpeed * AnimSpeed), Trunc(AnimSpeed * AnimSpeed));
              7:
                OffsetRect(R, Trunc(AnimSpeed * AnimSpeed), Trunc(AnimSpeed * AnimSpeed));
            end;
          iesFromCenter:
            begin
              CurCX := Width div 2;
              CurCY := Height div 2;
              MoveX := Trunc((CurCX - (R.Left + R.Right) div 2) * 0.18);
              MoveY := Trunc((CurCY - (R.Top + R.Bottom) div 2) * 0.18);
              OffsetRect(R, MoveX, MoveY);
            end;
          iesFromPoint:
            if not IsRectEmpty(FallingTargetPos) then
            begin
              TargetCX := (FallingTargetPos.Left + FallingTargetPos.Right) div 2;
              TargetCY := (FallingTargetPos.Top + FallingTargetPos.Bottom) div 2;
              MoveX := (TargetCX - (R.Left + R.Right) div 2) div Max(1, Trunc(AnimSpeed * 0.6));
              MoveY := (TargetCY - (R.Top + R.Bottom) div 2) div Max(1, Trunc(AnimSpeed * 0.6));
              if Abs(MoveX) < 3 then
                MoveX := Sign(MoveX) * Max(3, AnimSpeed);
              if Abs(MoveY) < 3 then
                MoveY := Sign(MoveY) * Max(3, AnimSpeed);
              if (R.Right - R.Left > 20) and (R.Bottom - R.Top > 20) then
              begin
                CurW := R.Right - R.Left;
                CurH := R.Bottom - R.Top;
                ShrinkFactor := 0.92 + (AnimSpeed * 0.001);
                NewW := Trunc(CurW * ShrinkFactor);
                NewH := Trunc(CurH * ShrinkFactor);
                CurCX := (R.Left + R.Right) div 2;
                CurCY := (R.Top + R.Bottom) div 2;
                R.Left := CurCX - NewW div 2;
                R.Top := CurCY - NewH div 2;
                R.Right := CurCX + NewW div 2;
                R.Bottom := CurCY + NewH div 2;
              end;
              OffsetRect(R, MoveX, MoveY);
            end
            else
            begin
              FallingStyle := iesFromBottom;
              OffsetRect(R, 0, Trunc(AnimSpeed * AnimSpeed));
            end;
        end;

        // Hide conditions
        if (FallingStyle in [iesFromTop, iesFromBottom, iesFromLeft, iesFromRight, iesFromTopLeft, iesFromTopRight, iesFromBottomLeft, iesFromBottomRight, iesRandom]) then
        begin
          if (R.Bottom < -100) or (R.Top > Height + 100) or (R.Right < -100) or (R.Left > Width + 100) then
            ImageItem.Visible := False
          else
            AllOut := False;
        end
        else if FallingStyle = iesFromCenter then
        begin
          CurCX := (R.Left + R.Right) div 2;
          CurCY := (R.Top + R.Bottom) div 2;
          if (Abs(CurCX - Width div 2) < 80) and (Abs(CurCY - Height div 2) < 80) then
            ImageItem.Visible := False
          else
            AllOut := False;
        end
        else if FallingStyle = iesFromPoint then
        begin
          if not IsRectEmpty(FallingTargetPos) then
          begin
            if (Abs(MoveX) <= 20) and (Abs(MoveY) <= 20) or ((R.Bottom - R.Top) < 30) then
              ImageItem.Visible := False
            else
              AllOut := False;
          end
          else
          begin
            if (R.Bottom < -100) or (R.Top > Height + 100) or (R.Right < -100) or (R.Left > Width + 100) then
              ImageItem.Visible := False
            else
              AllOut := False;
          end;
        end;
      end
        // --------------------------------------------------------------
        // Selected image with ZoominSelected and valid SelectedTargetPos
        // --------------------------------------------------------------
      else
      begin

        TargetCX := (SelectedTargetPos.Left + SelectedTargetPos.Right) div 2;
        TargetCY := (SelectedTargetPos.Top + SelectedTargetPos.Bottom) div 2;
        MoveX := (TargetCX - (R.Left + R.Right) div 2) div Max(1, Trunc(AnimSpeed * 0.6));
        MoveY := (TargetCY - (R.Top + R.Bottom) div 2) div Max(1, Trunc(AnimSpeed * 0.6));

        // Shrink
        if (R.Right - R.Left > 20) and (R.Bottom - R.Top > 20) then
        begin
          CurW := R.Right - R.Left;
          CurH := R.Bottom - R.Top;
          ShrinkFactor := 0.93 + (AnimSpeed * 0.001);
          NewW := Trunc(CurW * ShrinkFactor);
          NewH := Trunc(CurH * ShrinkFactor);
          CurCX := (R.Left + R.Right) div 2;
          CurCY := (R.Top + R.Bottom) div 2;
          R.Left := CurCX - NewW div 2;
          R.Top := CurCY - NewH div 2;
          R.Right := CurCX + NewW div 2;
          R.Bottom := CurCY + NewH div 2;
        end;

        OffsetRect(R, MoveX, MoveY);

        if (Abs(MoveX) <= 20) and (Abs(MoveY) <= 20) or ((R.Bottom - R.Top) < 30) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end;

      // Update rect if still visible
      if ImageItem.Visible then
      begin
        ImageItem.CurrentRect := R;
        AllOut := False;
      end;
    end;

    Invalidate;
    if GetTickCount - StartTick > 50 then
    begin
      Application.ProcessMessages;
      if FClearing and (csDestroying in ComponentState) then
        Break;
      if GetAsyncKeyState(VK_ESCAPE) < 0 then
        Break;
    end;
    Sleep(AnimSpeed);

    if (GetTickCount - StartTick) > THREAD_CLEANUP_TIMEOUT then
      AllOut := True;

  until AllOut;

  // ==============================================================
  // Final cleanup
  // ==============================================================
  FreeAllImagesAndClearLists;
end;

/// <summary>
///   Frees all image items and clears all lists.
/// </summary>
procedure TFlowmotion.FreeAllImagesAndClearLists;
var
  i: Integer;
begin
  FClearing := True;
  try
    for i := 0 to FImages.Count - 1 do
    begin
      try
        if Assigned(TImageItem(FImages[i])) then
          TImageItem(FImages[i]).Free;
      except
      end;
    end;
    FImages.Clear;
    FAllFiles.Clear;
    FAllCaptions.Clear;
    FAllPaths.Clear;
    FHotItem := nil;
    FSelectedImage := nil;
    FWasSelectedItem := nil;
    FCurrentSelectedIndex := -1;
    FCurrentPage := 0;
    FBreathingPhase := 0.0;
  finally
    FClearing := False;
    FInFallAnimation := False;
    StopAnimationThread;
    Repaint;
  end;
end;

{ Removes an image by index, optionally with fall animation }
procedure TFlowmotion.RemoveImage(Index: Integer; Animated: Boolean = True);
begin
  RemoveImage(Index, Animated, Rect(0, 0, 0, 0), iesFromBottom);
end;

{ Removes an image by index, optionally with extended fall animation like on clear }
procedure TFlowmotion.RemoveImage(Index: Integer; Animated: Boolean; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle);
var
  StartTick: DWORD;
  CurCX, CurCY, TargetCX, TargetCY, MoveX, MoveY: Integer;
  PageStart, PageEnd: Integer;
  ImageItem: TImageItem;
  R: TRect;
  AllOut: Boolean;
  Speed: Integer;
begin
  try
    PageStart := GetPageStartIndex;
    PageEnd := GetPageEndIndex;
    if (Index < PageStart) or (Index > PageEnd) then
      Exit;

    if not Animated then
    begin
      if TImageItem(FImages[Index]) = FSelectedImage then
      begin
        FSelectedImage := nil;
        FCurrentSelectedIndex := -1;
      end
      else if Index < FCurrentSelectedIndex then
        Dec(FCurrentSelectedIndex);

      TImageItem(FImages[Index]).Free;
      FImages.Delete(Index);
      if Visible then
      begin
        CalculateLayout;
        Invalidate;
      end;
      Exit;
    end;

    FInFallAnimation := True;
    StopAnimationThread;
    ImageItem := TImageItem(FImages[Index]);
    ImageItem.Animating := True;

    repeat
      AllOut := True;

      Speed := Max(1, FAnimationSpeed * 2 - 8);

      R := ImageItem.CurrentRect;

      // direction like on addimage
      case FallingStyle of
        iesFromTop:
          OffsetRect(R, 0, -Trunc(FAnimationSpeed * Speed));
        iesFromBottom:
          OffsetRect(R, 0, Trunc(FAnimationSpeed * Speed));
        iesFromLeft:
          OffsetRect(R, -Trunc(FAnimationSpeed * Speed), 0);
        iesFromRight:
          OffsetRect(R, Trunc(FAnimationSpeed * Speed), 0);
        iesFromTopLeft:
          OffsetRect(R, -Trunc(FAnimationSpeed * Speed), -Trunc(FAnimationSpeed * Speed));
        iesFromTopRight:
          OffsetRect(R, Trunc(FAnimationSpeed * Speed), -Trunc(FAnimationSpeed * Speed));
        iesFromBottomLeft:
          OffsetRect(R, -Trunc(FAnimationSpeed * Speed), Trunc(FAnimationSpeed * Speed));
        iesFromBottomRight:
          OffsetRect(R, Trunc(FAnimationSpeed * Speed), Trunc(FAnimationSpeed * Speed));

        iesRandom:
          case Random(8) of
            0:
              OffsetRect(R, 0, -Trunc(FAnimationSpeed * Speed));
            1:
              OffsetRect(R, 0, Trunc(FAnimationSpeed * Speed));
            2:
              OffsetRect(R, -Trunc(FAnimationSpeed * Speed), 0);
            3:
              OffsetRect(R, Trunc(FAnimationSpeed * Speed), 0);
            4:
              OffsetRect(R, -Trunc(FAnimationSpeed * Speed), -Trunc(FAnimationSpeed * Speed));
            5:
              OffsetRect(R, Trunc(FAnimationSpeed * Speed), -Trunc(FAnimationSpeed * Speed));
            6:
              OffsetRect(R, -Trunc(FAnimationSpeed * Speed), Trunc(FAnimationSpeed * Speed));
            7:
              OffsetRect(R, Trunc(FAnimationSpeed * Speed), Trunc(FAnimationSpeed * Speed));
          end;

        iesFromCenter:
          begin
            CurCX := Width div 2;
            CurCY := Height div 2;
            MoveX := Trunc((CurCX - (R.Left + R.Right) div 2) * 0.18);
            MoveY := Trunc((CurCY - (R.Top + R.Bottom) div 2) * 0.18);
            OffsetRect(R, MoveX, MoveY);
          end;

        iesFromPoint:
          if not IsRectEmpty(FallingTargetPos) then
          begin
            TargetCX := (FallingTargetPos.Left + FallingTargetPos.Right) div 2;
            TargetCY := (FallingTargetPos.Top + FallingTargetPos.Bottom) div 2;
            MoveX := Trunc((TargetCX - (R.Left + R.Right) div 2) * 0.20);
            MoveY := Trunc((TargetCY - (R.Top + R.Bottom) div 2) * 0.20);
            if Abs(MoveX) < 3 then
              MoveX := Sign(MoveX) * Max(3, Speed);
            if Abs(MoveY) < 3 then
              MoveY := Sign(MoveY) * Max(3, Speed);
            OffsetRect(R, MoveX, MoveY);
          end
          else
          begin
            //fallback if no targetrect for falling
            FallingStyle := iesFromBottom;
            OffsetRect(R, 0, Trunc(FAnimationSpeed * Speed));
          end;
      end;

      ImageItem.CurrentRect := R;

      // Hide image when it's outside the window or has reached the target
      if (FallingStyle in [iesFromTop, iesFromBottom, iesFromLeft, iesFromRight, iesFromTopLeft, iesFromTopRight, iesFromBottomLeft, iesFromBottomRight, iesRandom]) then
      begin
        // Normal flying out
        if (R.Bottom < -100) or (R.Top > Height + 100) or (R.Right < -100) or (R.Left > Width + 100) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end
      else if FallingStyle = iesFromCenter then
      begin
        // Flying to center
        CurCX := (R.Left + R.Right) div 2;
        CurCY := (R.Top + R.Bottom) div 2;
        if (Abs(CurCX - Width div 2) < 80) and (Abs(CurCY - Height div 2) < 80) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end
      else if FallingStyle = iesFromPoint then
      begin
        // Flying to FallingTargetPos
        if not IsRectEmpty(FallingTargetPos) then
        begin
          TargetCX := (FallingTargetPos.Left + FallingTargetPos.Right) div 2;
          TargetCY := (FallingTargetPos.Top + FallingTargetPos.Bottom) div 2;
          CurCX := (R.Left + R.Right) div 2;
          CurCY := (R.Top + R.Bottom) div 2;

          if (Abs(CurCX - TargetCX) < 200) and (Abs(CurCY - TargetCY) < 200) then
            ImageItem.Visible := False
          else
            AllOut := False;
        end
        else
          {// Fallback if no target} if (R.Bottom < -100) or (R.Top > Height + 100) or (R.Right < -100) or (R.Left > Width + 100) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end;

      //to be safe if i calculate something wrong :P
      if (GetTickCount - StartTick) > THREAD_CLEANUP_TIMEOUT then
        AllOut := True;

      Invalidate;
      Application.ProcessMessages;
      Sleep(FAnimationSpeed);

    until AllOut;

    if TImageItem(FImages[Index]) = FSelectedImage then
    begin
      FSelectedImage := nil;
      FCurrentSelectedIndex := -1;
    end
    else if Index < FCurrentSelectedIndex then
      Dec(FCurrentSelectedIndex);

    ImageItem.Free;
    FImages.Delete(Index);
    FInFallAnimation := False;
    Invalidate;

    if (FImages.Count = 0) and (FCurrentPage > 0) then
      PrevPage;
  except
  end;
end;

{ Inserts an image asynchronously using a background thread }
procedure TFlowmotion.InsertImageAsync(const FileName, Caption, Path: string);
var
  LoadThread: TImageLoadThread;
  SystemInfo: TSystemInfo;
  CoreToUse: Integer;
begin
  FAllFiles.Add(FileName);
  FAllCaptions.Add(Caption);
  FAllPaths.Add(Path);

  GetSystemInfo(SystemInfo);
  // Use Round-Robin to distribute across all available cores
  CoreToUse := FNextLoaderCoreIndex mod SystemInfo.dwNumberOfProcessors;
  Inc(FNextLoaderCoreIndex);

  LoadThread := TImageLoadThread.Create(FileName, Caption, Path, Self, CoreToUse);
  LoadThread.Priority := FThreadPriority;
  FLoadingThreads.Add(LoadThread);
  Inc(FLoadingCount);
end;

{ Replaces the bitmap of an existing image }
procedure TFlowmotion.SetImage(Index: Integer; Bitmap: TBitmap);
begin
  if (Index >= 0) and (Index < FImages.Count) then
  begin
    TImageItem(FImages[Index]).Bitmap.Assign(Bitmap);
    CalculateLayout;
    Invalidate;
  end;
end;

{ Calculates optimal size for an image to fit within max dimensions while preserving aspect ratio }
function TFlowmotion.GetOptimalSize(const OriginalWidth, OriginalHeight: Integer; const MaxWidth, MaxHeight: Integer): TSize;
var
  ScaleX, ScaleY, Scale: Double;
begin
  if FKeepAspectRatio then
  begin
    if (OriginalWidth = 0) or (OriginalHeight = 0) then
    begin
      Result.cx := MaxWidth;
      Result.cy := MaxHeight;
      Exit;
    end;
    ScaleX := MaxWidth / OriginalWidth;
    ScaleY := MaxHeight / OriginalHeight;
    Scale := Min(ScaleX, ScaleY);
    Result.cx := Round(OriginalWidth * Scale);
    Result.cy := Round(OriginalHeight * Scale);
  end
  else
  begin
    Result.cx := MaxWidth;
    Result.cy := MaxHeight;
  end;
end;


// Helper function to calculate caption rect
function TFlowmotion.GetCaptionRect(Item: TImageItem; const DrawRect: TRect): TRect;
var
  CaptionW, CaptionH: Integer;
begin
  if not FShowCaptions or (Item.Caption = '') then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;

  Canvas.Font.Assign(FCaptionFont);
  CaptionH := Canvas.TextHeight('Hg') + 12;
  CaptionW := Canvas.TextWidth(Item.Caption) + 24;

  Result.Left := DrawRect.Left + (DrawRect.Right - DrawRect.Left - CaptionW) div 2;
  Result.Top := DrawRect.Bottom - CaptionH - FCaptionOffsetY;
  Result.Right := Result.Left + CaptionW;
  Result.Bottom := Result.Top + CaptionH;

  // Simple clipping
  if Result.Bottom > ClientHeight then
    Dec(Result.Top, Result.Bottom - ClientHeight);
  if Result.Top < 0 then
    Result.Top := 0;
  Result.Bottom := Result.Top + CaptionH;
end;


{ Returns the absolute index of the first image on the current page }
function TFlowmotion.GetPageStartIndex: Integer;
begin
  Result := FCurrentPage * FPageSize;
end;

{ Returns the absolute index of the last image on the current page }
function TFlowmotion.GetPageEndIndex: Integer;
begin
  Result := Min((FCurrentPage + 1) * FPageSize, FAllFiles.Count) - 1;
end;

{ CalculateLayout - Calculates the layout for all images based on current FlowLayout setting }
procedure TFlowmotion.CalculateLayout;
begin
  case FFlowLayout of
    flSorted:
      CalculateLayoutSorted;
    flFreeFloat:
      CalculateLayoutFreeFloat;
  end;
end;


{ CalculateLayoutFreeFloat - Implements free float positioning for images }
procedure TFlowmotion.CalculateLayoutFreeFloat;
var
  Cols, Rows, i, c, r, BestCols, BestRows: Integer;
  ImageItem: TImageItem;
  BaseCellWidth, BaseCellHeight: Integer;
  VCount, Row, Col, AddforZoomed: Integer;
  Grid: TBooleanGrid;
  SpanCols, SpanRows: Integer;
  Placed: Boolean;
  MaxCellWidth: Double;
  ImageAspectRatio: Double;
  VisibleImages: TList;
  SortList: TList;
  MaxCellArea: Integer;
  MinCols, MaxColsToTry: Integer;
  PotentialCellWidth, PotentialCellHeight, CellArea: Double;
  TotalCellEstimate: Integer;
  X, Y: Integer;
  ImageSize: TSize;
  CellWidth, CellHeight: Integer;

  function CompareImageSize(A, B: Pointer): Integer;
  var
    AreaA, AreaB: Int64;
  begin
    with TImageItem(A).Bitmap do
      AreaA := Int64(Width) * Height;
    with TImageItem(B).Bitmap do
      AreaB := Int64(Width) * Height;
    if AreaA > AreaB then
      Result := -1
    else if AreaA < AreaB then
      Result := 1
    else
      Result := 0;
  end;

begin
  if FImages.Count = 0 then
    Exit;
  if FInFallAnimation then
    Exit;
  AddforZoomed := 0;
  if FKeepSpaceforZoomed then
    if FSelectedImage <> nil then
      AddforZoomed := 2;
  // Collect visible images
  VisibleImages := TList.Create;
  try
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if (not ImageItem.IsSelected) or (not FZoomSelectedtoCenter) then
        VisibleImages.Add(ImageItem);
    end;
    if VisibleImages.Count = 0 then
      Exit;
    // Sort only when Sorted = False (by size)
    if not FSorted then
    begin
      SortList := TList.Create;
      try
        SortList.Assign(VisibleImages);
        SortList.Sort(@CompareImageSize);
        VisibleImages.Assign(SortList);
      finally
        SortList.Free;
      end;
    end;
    if FZoomSelectedtoCenter and (FSelectedImage <> nil) then
      VCount := VisibleImages.Count + 1
    else
      VCount := VisibleImages.Count;
    // =================================================================
    // First, estimate the total number of cells needed
    // =================================================================
    TotalCellEstimate := 0;
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if (ImageItem.Bitmap.Width = 0) or (ImageItem.Bitmap.Height = 0) then
        Continue;
      ImageAspectRatio := ImageItem.Bitmap.Width / ImageItem.Bitmap.Height;
      if ImageAspectRatio > 1.4 then
        Inc(TotalCellEstimate, 2) // Wide image takes 2 cells
      else if ImageAspectRatio < 0.75 then
        Inc(TotalCellEstimate, 2) // Tall image takes 2 cells
      else
        Inc(TotalCellEstimate, 1); // Square image takes 1 cell
    end;
    // Ensure the grid is at least large enough for the number of images
    if TotalCellEstimate < VCount then
      TotalCellEstimate := VCount;
    // =================================================================
    // Find the optimal number of columns for the best layout
    // =================================================================
    MaxCellWidth := 0;
    BestCols := 0;
    BestRows := 0;
    MinCols := Max(3, Trunc(Sqrt(TotalCellEstimate)));
    MaxColsToTry := TotalCellEstimate;
    for c := MinCols to MaxColsToTry do
    begin
      r := Ceil(TotalCellEstimate / c);
      if r < 3 then
        r := 3;
      PotentialCellWidth := (Width - FSpacing * (c + 1)) / c;
      PotentialCellHeight := (Height - FSpacing * (r + 1)) / r;
        // If cell size is too small, no point in trying this configuration
      if (PotentialCellWidth < MIN_CELL_SIZE) or (PotentialCellHeight < MIN_CELL_SIZE) then
        Continue;
        // Prioritize cell width over area.
        // This ensures the layout uses the full horizontal space.
      if PotentialCellWidth > MaxCellWidth then
      begin
        MaxCellWidth := PotentialCellWidth;
        BestCols := c;
        BestRows := r;
      end;
    end;
      // Fallback if the loop didn't find a good layout
    if BestCols = 0 then
    begin
      BestCols := Max(3, Ceil(Sqrt(TotalCellEstimate)));
      BestRows := Max(3, Ceil(TotalCellEstimate / BestCols));
    end;
    Cols := BestCols;
    Rows := BestRows;
    // =================================================================
    // END OF NEW SECTION
    // =================================================================
    BaseCellWidth := Max(MIN_CELL_SIZE, (Width - FSpacing * (Cols + 1)) div Cols);
    BaseCellHeight := Max(MIN_CELL_SIZE, (Height - FSpacing * (Rows + 1)) div Rows);
    SetLength(Grid, Rows, Cols);
    for Row := 0 to Rows - 1 do
      for Col := 0 to Cols - 1 do
        Grid[Row, Col] := False;
    // The robust placement logic from the previous answer is correct and should be used here.
    // It will now work because the grid is guaranteed to be large enough.
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if (ImageItem.Bitmap.Width = 0) or (ImageItem.Bitmap.Height = 0) then
        Continue;
      ImageAspectRatio := ImageItem.Bitmap.Width / ImageItem.Bitmap.Height;
      if ImageAspectRatio > 1.4 then
      begin
        SpanCols := 2;
        SpanRows := 1;
      end
      else if ImageAspectRatio < 0.75 then
      begin
        SpanCols := 1;
        SpanRows := 2;
      end
      else
      begin
        SpanCols := 1;
        SpanRows := 1;
      end;
      Placed := False;
      // Exhaustive search for a free spot
      for r := 0 to Rows - SpanRows do
      begin
        for c := 0 to Cols - SpanCols do
        begin
          if IsAreaFree(Grid, r, c, SpanRows, SpanCols) then
          begin
            // Calculate cell position and size
            X := FSpacing + c * BaseCellWidth + c * FSpacing;
            Y := r * (BaseCellHeight + FSpacing);
            // Available size for the image
            CellWidth := (SpanCols * BaseCellWidth) + ((SpanCols - 1) * FSpacing);
            CellHeight := (SpanRows * BaseCellHeight) + ((SpanRows - 1) * FSpacing);
            // Optimal image size preserving aspect ratio
            ImageSize := GetOptimalSize(ImageItem.Bitmap.Width, ImageItem.Bitmap.Height, CellWidth, CellHeight);
            // Center image in cell
            X := X + (CellWidth - ImageSize.cx) div 2;
            Y := Y + (CellHeight - ImageSize.cy) div 2;
            // Ensure the image stays within component bounds
            if X < 0 then
              X := 0;
            if Y < 0 then
              Y := 0;
            if X + ImageSize.cx > Width then
              X := Width - ImageSize.cx;
            if Y + ImageSize.cy > Height then
              Y := Height - ImageSize.cy;
            ImageItem.TargetRect := Rect(X, Y, X + ImageSize.cx, Y + ImageSize.cy);
            ImageItem.StartRect := ImageItem.CurrentRect;
            ImageItem.AnimationProgress := 0;
            ImageItem.Animating := True;
            MarkAreaOccupied(Grid, r, c, SpanRows, SpanCols);
            Placed := True;
            Break;
          end;
        end;
        if Placed then
          Break;
      end;
      // Fallback: If the image didn't fit with its span, try to force it as 1x1
      if not Placed then
      begin
        for r := 0 to Rows - 1 do
        begin
          for c := 0 to Cols - 1 do
          begin
            if IsAreaFree(Grid, r, c, 1, 1) then
            begin
              // Calculate cell position and size
              X := FSpacing + c * BaseCellWidth + c * FSpacing;
              Y := r * (BaseCellHeight + FSpacing);
              // Available size for the image
              CellWidth := BaseCellWidth;
              CellHeight := BaseCellHeight;
              // Optimal image size preserving aspect ratio
              ImageSize := GetOptimalSize(ImageItem.Bitmap.Width, ImageItem.Bitmap.Height, CellWidth, CellHeight);
              // Center image in cell
              X := X + (CellWidth - ImageSize.cx) div 2;
              Y := Y + (CellHeight - ImageSize.cy) div 2;
              // Ensure the image stays within component bounds
              if X < 0 then
                X := 0;
              if Y < 0 then
                Y := 0;
              if X + ImageSize.cx > Width then
                X := Width - ImageSize.cx;
              if Y + ImageSize.cy > Height then
                Y := Height - ImageSize.cy;
              ImageItem.TargetRect := Rect(X, Y, X + ImageSize.cx, Y + ImageSize.cy);
              ImageItem.StartRect := ImageItem.CurrentRect;
              ImageItem.AnimationProgress := 0;
              ImageItem.Animating := True;
              MarkAreaOccupied(Grid, r, c, 1, 1);
              Placed := True;
              Break;
            end;
          end;
          if Placed then
            Break;
        end;
      end;
    end;
  finally
    VisibleImages.Free;
  end;
end;


{ Calculates layout using sorted algorithm: places images in grid based on aspect ratio }
procedure TFlowmotion.CalculateLayoutSorted;
var
  Cols, Rows, i, c, r, BestCols, BestRows: Integer;
  ImageItem: TImageItem;
  BaseCellWidth, BaseCellHeight: Integer;
  VCount, Row, Col, AddforZoomed: Integer;
  Grid: TBooleanGrid;
  SpanCols, SpanRows: Integer;
  Placed: Boolean;
  MaxCellWidth: Double;
  ImageAspectRatio: Double;
  VisibleImages: TList;
  SortList: TList;
  MaxCellArea: Integer;
  MinCols, MaxColsToTry: Integer;
  PotentialCellWidth, PotentialCellHeight, CellArea: Double;
  TotalCellEstimate: Integer;

  function CompareImageSize(A, B: Pointer): Integer;
  var
    AreaA, AreaB: Int64;
  begin
    with TImageItem(A).Bitmap do
      AreaA := Int64(Width) * Height;
    with TImageItem(B).Bitmap do
      AreaB := Int64(Width) * Height;
    if AreaA > AreaB then
      Result := -1
    else if AreaA < AreaB then
      Result := 1
    else
      Result := 0;
  end;

begin
  if FImages.Count = 0 then
    Exit;
  if FInFallAnimation then
    Exit;

  AddforZoomed := 0;
  if FKeepSpaceforZoomed then
    if FSelectedImage <> nil then
      AddforZoomed := 2;

  // Collect visible images
  VisibleImages := TList.Create;
  try
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if (not ImageItem.IsSelected) or (not FZoomSelectedtoCenter) then
        VisibleImages.Add(ImageItem);
    end;
    if VisibleImages.Count = 0 then
      Exit;

    // Sort only when Sorted = False (by size)
    if not FSorted then
    begin
      SortList := TList.Create;
      try
        SortList.Assign(VisibleImages);
        SortList.Sort(@CompareImageSize);
        VisibleImages.Assign(SortList);
      finally
        SortList.Free;
      end;
    end;

    if FZoomSelectedtoCenter and (FSelectedImage <> nil) then
      VCount := VisibleImages.Count + 1
    else
      VCount := VisibleImages.Count;

    // =================================================================
    // First, estimate the total number of cells needed
    // =================================================================
    TotalCellEstimate := 0;
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if (ImageItem.Bitmap.Width = 0) or (ImageItem.Bitmap.Height = 0) then
        Continue;
      ImageAspectRatio := ImageItem.Bitmap.Width / ImageItem.Bitmap.Height;

      if ImageAspectRatio > 1.4 then
        Inc(TotalCellEstimate, 2) // Wide image takes 2 cells
      else if ImageAspectRatio < 0.75 then
        Inc(TotalCellEstimate, 2) // Tall image takes 2 cells
      else
        Inc(TotalCellEstimate, 1); // Square image takes 1 cell
    end;

    // Ensure the grid is at least large enough for the number of images
    if TotalCellEstimate < VCount then
      TotalCellEstimate := VCount;

    // =================================================================
    // NEW: Find the optimal number of columns for the best layout
    // =================================================================
    MaxCellWidth := 0;
    BestCols := 0;
    BestRows := 0;
    MinCols := Max(3, Trunc(Sqrt(TotalCellEstimate)));
    MaxColsToTry := TotalCellEstimate;

    for c := MinCols to MaxColsToTry do
    begin
      r := Ceil(TotalCellEstimate / c);
      if r < 3 then
        r := 3;

      PotentialCellWidth := (Width - FSpacing * (c + 1)) / c;
      PotentialCellHeight := (Height - FSpacing * (r + 1)) / r;

        // If cell size is too small, no point in trying this configuration
      if (PotentialCellWidth < MIN_CELL_SIZE) or (PotentialCellHeight < MIN_CELL_SIZE) then
        Continue;

        // Prioritize cell width over area.
        // This ensures the layout uses the full horizontal space.
      if PotentialCellWidth > MaxCellWidth then
      begin
        MaxCellWidth := PotentialCellWidth;
        BestCols := c;
        BestRows := r;
      end;
    end;

      // Fallback if the loop didn't find a good layout
    if BestCols = 0 then
    begin
      BestCols := Max(3, Ceil(Sqrt(TotalCellEstimate)));
      BestRows := Max(3, Ceil(TotalCellEstimate / BestCols));
    end;

    Cols := BestCols;
    Rows := BestRows;

    // =================================================================
    // END OF NEW SECTION
    // =================================================================

    BaseCellWidth := Max(MIN_CELL_SIZE, (Width - FSpacing * (Cols + 1)) div Cols);
    BaseCellHeight := Max(MIN_CELL_SIZE, (Height - FSpacing * (Rows + 1)) div Rows);

    SetLength(Grid, Rows, Cols);
    for Row := 0 to Rows - 1 do
      for Col := 0 to Cols - 1 do
        Grid[Row, Col] := False;

    // The robust placement logic from the previous answer is correct and should be used here.
    // It will now work because the grid is guaranteed to be large enough.
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if (ImageItem.Bitmap.Width = 0) or (ImageItem.Bitmap.Height = 0) then
        Continue;

      ImageAspectRatio := ImageItem.Bitmap.Width / ImageItem.Bitmap.Height;

      if ImageAspectRatio > 1.4 then
      begin
        SpanCols := 2;
        SpanRows := 1;
      end
      else if ImageAspectRatio < 0.75 then
      begin
        SpanCols := 1;
        SpanRows := 2;
      end
      else
      begin
        SpanCols := 1;
        SpanRows := 1;
      end;

      Placed := False;

      // Exhaustive search for a free spot
      for r := 0 to Rows - SpanRows do
      begin
        for c := 0 to Cols - SpanCols do
        begin
          if IsAreaFree(Grid, r, c, SpanRows, SpanCols) then
          begin
            if PlaceImage(ImageItem, Grid, r, c, SpanRows, SpanCols, BaseCellWidth, BaseCellHeight) then
            begin
              Placed := True;
              Break;
            end;
          end;
        end;
        if Placed then
          Break;
      end;

      // Fallback: If the image didn't fit with its span, try to force it as 1x1
      if not Placed then
      begin
        for r := 0 to Rows - 1 do
        begin
          for c := 0 to Cols - 1 do
          begin
            if IsAreaFree(Grid, r, c, 1, 1) then
            begin
              PlaceImage(ImageItem, Grid, r, c, 1, 1, BaseCellWidth, BaseCellHeight);
              Placed := True;
              Break;
            end;
          end;
          if Placed then
            Break;
        end;
      end;
    end;
  finally
    VisibleImages.Free;
  end;
end;

// Checks if an area in the grid is free for placing an image
function TFlowmotion.IsAreaFree(const Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer): Boolean;
var
  r, c: Integer;
  CenterRow, CenterCol, ProtectedSize: Integer;
  PotentialRect: TRect;
  DummyRect: TRect; // *** FIX: Dummy rect for the IntersectRect out parameter ***
  BaseCellWidth, BaseCellHeight: Integer;
  X, Y, CellW, CellH: Integer;
begin
  // Bounds check
  if (Row < 0) or (Col < 0) or (Row + SpanRows > Length(Grid)) or (Col + SpanCols > Length(Grid[0])) then
  begin
    Result := False;
    Exit;
  end;

  // Keep center area free for zoomed image
  if FKeepSpaceforZoomed then
    if (FSelectedImage <> nil) then
    begin
      CenterRow := (Length(Grid) div 2) - 1;
      CenterCol := (Length(Grid[0]) div 2) - 1;
      ProtectedSize := 2;
      if (Row < CenterRow + ProtectedSize) and (Row + SpanRows > CenterRow) and (Col < CenterCol + ProtectedSize) and (Col + SpanCols > CenterCol) then
      begin
        Result := False;
        Exit;
      end;
    end;

  // Normal grid check: verify all cells in the area are free
  for r := Row to Row + SpanRows - 1 do
    for c := Col to Col + SpanCols - 1 do
      if Grid[r, c] then
      begin
        Result := False;
        Exit;
      end;

  // We calculate the potential screen rectangle for this grid area.
  BaseCellWidth := Max(MIN_CELL_SIZE, (Width - FSpacing * (Length(Grid[0]) + 1)) div Length(Grid[0]));
  BaseCellHeight := Max(MIN_CELL_SIZE, (Height - FSpacing * (Length(Grid) + 1)) div Length(Grid));

  X := FSpacing + Col * BaseCellWidth + Col * FSpacing;
  Y := Row * (BaseCellHeight + FSpacing);

  CellW := (SpanCols * BaseCellWidth) + ((SpanCols - 1) * FSpacing);
  CellH := (SpanRows * BaseCellHeight) + ((SpanRows - 1) * FSpacing);

  PotentialRect := Rect(X, Y, X + CellW, Y + CellH);

  // Now check for overlap with the user-defined free area.
  // We use a dummy rectangle for the 'out' parameter.
  if not IsRectEmpty(FKeepAreaFreeRect) and IntersectRect(DummyRect, PotentialRect, FKeepAreaFreeRect) then
  begin
    Result := False;
    Exit;
  end;

  // If all checks passed, the area is free.
  Result := True;
end;

{ Marks an area in the grid as occupied }
procedure TFlowmotion.MarkAreaOccupied(var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer);
var
  r, c: Integer;
begin
  for r := Row to Row + SpanRows - 1 do
    for c := Col to Col + SpanCols - 1 do
      if (r >= 0) and (r < Length(Grid)) and (c >= 0) and (c < Length(Grid[0])) then
        Grid[r, c] := True;
end;

// Places an image in the grid at the specified position and calculates its target rectangle
function TFlowmotion.PlaceImage(ImageItem: TImageItem; var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer; BaseCellWidth, BaseCellHeight: Integer): Boolean;
var
  X, Y: Integer;
  CellWidth, CellHeight: Integer;
  ImageSize: TSize;
  TempRect: TRect;
begin
  // Calculate cell position and size
  X := FSpacing + Col * BaseCellWidth + Col * FSpacing;
  Y := Row * (BaseCellHeight + FSpacing);

  // Available size for the image
  CellWidth := (SpanCols * BaseCellWidth) + ((SpanCols - 1) * FSpacing);
  CellHeight := (SpanRows * BaseCellHeight) + ((SpanRows - 1) * FSpacing);

  // Optimal image size preserving aspect ratio
  ImageSize := GetOptimalSize(ImageItem.Bitmap.Width, ImageItem.Bitmap.Height, CellWidth, CellHeight);

  // Center image in cell
  X := X + (CellWidth - ImageSize.cx) div 2;
  Y := Y + (CellHeight - ImageSize.cy) div 2;

  // Ensure the image stays within component bounds
  if X < 0 then
    X := 0;
  if Y < 0 then
    Y := 0;
  if X + ImageSize.cx > Width then
    X := Width - ImageSize.cx;
  if Y + ImageSize.cy > Height then
    Y := Height - ImageSize.cy;

  // Set the final target rectangle first.
  ImageItem.TargetRect := Rect(X, Y, X + ImageSize.cx, Y + ImageSize.cy);

  // Now check if this FINAL rectangle overlaps with the user-defined free area.
  TempRect := ImageItem.TargetRect;
  if not IsRectEmpty(FKeepAreaFreeRect) and IntersectRect(TempRect, ImageItem.TargetRect, FKeepAreaFreeRect) then
  begin
    // It overlaps. Signal failure.
    Result := False;
    Exit;
  end;

  // If we get here, the placement is valid.
  ImageItem.StartRect := ImageItem.CurrentRect;
  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;

  MarkAreaOccupied(Grid, Row, Col, SpanRows, SpanCols);
  Result := True; // Signal success
end;


{ Returns the image item at the specified index, or nil if index is invalid }
function TFlowmotion.GetImageItem(Index: Integer): TImageItem;
begin
  if (Index >= 0) and (Index < FImages.Count) then
    Result := TImageItem(FImages[Index])
  else
    Result := nil;
end;

{ Finds an image by its path (switches page if needed) }
function TFlowmotion.FindImageByPath(const Path: string): TImageItem;
var
  i: Integer;
  AbsoluteIndex: Integer;
begin
  Result := nil;
  for i := 0 to FAllPaths.Count - 1 do
  begin
    if SameText(FAllPaths[i], Path) then
    begin
      AbsoluteIndex := i;

      if (AbsoluteIndex >= GetPageStartIndex) and (AbsoluteIndex <= GetPageEndIndex) then
        Result := TImageItem(FImages[AbsoluteIndex - GetPageStartIndex])
      else
      begin
        FCurrentPage := AbsoluteIndex div FPageSize;
        if (FCurrentPage >= 0) and (FCurrentPage < PageCount) then
          ShowPage(FCurrentPage);
        Result := FindImageByPath(Path);
      end;
      Break;
    end;
  end;
end;

{ Finds an image by its caption (switches page if needed) }
function TFlowmotion.FindImageByCaption(const Caption: string): TImageItem;
var
  i, AbsoluteIndex, RelativeIndex, TargetPage: Integer;
begin
  Result := nil;
  for i := 0 to FAllCaptions.Count - 1 do
  begin
    if SameText(FAllCaptions[i], Caption) then
    begin
      AbsoluteIndex := i;

      if (AbsoluteIndex >= GetPageStartIndex) and (AbsoluteIndex <= GetPageEndIndex) then
      begin
        RelativeIndex := AbsoluteIndex - GetPageStartIndex;
        if (RelativeIndex >= 0) and (RelativeIndex < FImages.Count) then
          Result := TImageItem(FImages[RelativeIndex]);
      end
      else
      begin
        TargetPage := AbsoluteIndex div FPageSize;
        if (TargetPage >= 0) and (TargetPage < PageCount) and (TargetPage <> FCurrentPage) then
        begin
          ShowPage(TargetPage);
          RelativeIndex := AbsoluteIndex - GetPageStartIndex;
          if (RelativeIndex >= 0) and (RelativeIndex < FImages.Count) then
            Result := TImageItem(FImages[RelativeIndex]);
        end;
      end;
      Break;
    end;
  end;
end;

{ Returns the index of an image item in the current page, or -1 if not found }
function TFlowmotion.GetImageIndex(ImageItem: TImageItem): Integer;
begin
  Result := FImages.IndexOf(ImageItem);
end;

{ Returns the image item at the specified screen coordinates, or nil if none.
  This function's Z-order logic is synchronized with the Paint method to ensure
  that the item returned is the one visually on top at the given point. }
function TFlowmotion.GetImageAtPoint(X, Y: Integer): TImageItem;
var
  i: Integer;
  ImageItem: TImageItem;
  P: TPoint;
  CenterX, CenterY, BaseW, BaseH, DrawW, DrawH: Integer;
  DrawRect: TRect;
  ZoomFactor: Double;
  BestCandidate: TImageItem;
  BestCandidateZoom: Double;
  BestCandidateArea: Integer;
  CurrentZoom: Double;
  CurrentArea: Integer;
  CurrentRect: TRect;
  BorderSize: Integer;
begin
  P := Point(X, Y);
  Result := nil;

  // ==================================================================
  // 1. SELECTED IMAGE HAS ABSOLUTE PRIORITY
  // ==================================================================
  if (FSelectedImage <> nil) and FSelectedImage.Visible then
  begin
    if (FSelectedImage = FHotItem) or (FSelectedImage.FHotZoom > 1.01) then
      ZoomFactor := FSelectedImage.FHotZoom
    else
      ZoomFactor := 1.0;

    if (FSelectedImage.CurrentRect.Right - FSelectedImage.CurrentRect.Left <= 0) or (FSelectedImage.CurrentRect.Bottom - FSelectedImage.CurrentRect.Top <= 0) then
    begin
      CenterX := (FSelectedImage.TargetRect.Left + FSelectedImage.TargetRect.Right) div 2;
      CenterY := (FSelectedImage.TargetRect.Top + FSelectedImage.TargetRect.Bottom) div 2;
      BaseW := FSelectedImage.TargetRect.Right - FSelectedImage.TargetRect.Left;
      BaseH := FSelectedImage.TargetRect.Bottom - FSelectedImage.TargetRect.Top;
    end
    else
    begin
      CenterX := (FSelectedImage.CurrentRect.Left + FSelectedImage.CurrentRect.Right) div 2;
      CenterY := (FSelectedImage.CurrentRect.Top + FSelectedImage.CurrentRect.Bottom) div 2;
      BaseW := FSelectedImage.CurrentRect.Right - FSelectedImage.CurrentRect.Left;
      BaseH := FSelectedImage.CurrentRect.Bottom - FSelectedImage.CurrentRect.Top;
    end;

    DrawW := Round(BaseW * ZoomFactor);
    DrawH := Round(BaseH * ZoomFactor);
    DrawRect.Left := CenterX - DrawW div 2;
    DrawRect.Top := CenterY - DrawH div 2;
    DrawRect.Right := DrawRect.Left + DrawW;
    DrawRect.Bottom := DrawRect.Top + DrawH;

    // IMPORTANT: Inflate by the LARGER of the two borders to ensure the hot-track
    // area of a selected image is also correctly detected.
    // This prevents FHotItem from being incorrectly reset, which causes
    // drawing inconsistencies and the "frame-only" flicker.
    BorderSize := Max(FGlowWidth, FHotTrackWidth);
    InflateRect(DrawRect, BorderSize, BorderSize);

    if PtInRect(DrawRect, P) then
    begin
      Result := FSelectedImage;
      Exit;
    end;
  end;

  // ==================================================================
  // 2. ALL OTHER IMAGES (find the one with highest Z-order)
  // ==================================================================
  BestCandidate := nil;
  BestCandidateZoom := -1.0; // Start with a very low value
  BestCandidateArea := 0;

  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if not ImageItem.Visible or (ImageItem = FSelectedImage) then
      Continue;

    // Calculate the effective drawing rectangle for this item
    if (ImageItem.FHotZoom > 1.01) then
    begin
      ZoomFactor := ImageItem.FHotZoom;
      CenterX := (ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) div 2;
      CenterY := (ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) div 2;
      BaseW := ImageItem.CurrentRect.Right - ImageItem.CurrentRect.Left;
      BaseH := ImageItem.CurrentRect.Bottom - ImageItem.CurrentRect.Top;

      if (BaseW <= 0) or (BaseH <= 0) then
      begin
        BaseW := ImageItem.TargetRect.Right - ImageItem.TargetRect.Left;
        BaseH := ImageItem.TargetRect.Bottom - ImageItem.TargetRect.Top;
        CenterX := (ImageItem.TargetRect.Left + ImageItem.TargetRect.Right) div 2;
        CenterY := (ImageItem.TargetRect.Top + ImageItem.TargetRect.Bottom) div 2;
      end;

      DrawW := Round(BaseW * ZoomFactor);
      DrawH := Round(BaseH * ZoomFactor);
      DrawRect.Left := CenterX - DrawW div 2;
      DrawRect.Top := CenterY - DrawH div 2;
      DrawRect.Right := DrawRect.Left + DrawW;
      DrawRect.Bottom := DrawRect.Top + DrawH;

      InflateRect(DrawRect, FHotTrackWidth, FHotTrackWidth);
    end
    else
    begin
      DrawRect := ImageItem.CurrentRect;
      InflateRect(DrawRect, FHotTrackWidth, FHotTrackWidth);
    end;

    // Quick check: Is the point even over this item's potential area?
    if not PtInRect(DrawRect, P) then
      Continue; // No, skip to the next item.

    // If we are here, the mouse is over this item. Now calculate its priority.
    // This logic mirrors the CompareHotZoom function used in the Paint method.
    CurrentZoom := ImageItem.FHotZoom;
    if CurrentZoom < 1.0 then
      CurrentZoom := 1.0; // Normalize non-hotzoomed items

    // Calculate area (using TargetRect if CurrentRect is empty)
    CurrentRect := ImageItem.CurrentRect;
    if IsRectEmpty(CurrentRect) then
      CurrentRect := ImageItem.TargetRect;
    CurrentArea := (CurrentRect.Right - CurrentRect.Left) * (CurrentRect.Bottom - CurrentRect.Top);

    // Compare with the best candidate found so far.
    // The item with the higher zoom factor should be drawn last (on top).
    // If zoom is equal, the item with the larger area should be on top.
    if (CurrentZoom > BestCandidateZoom) or ((CurrentZoom = BestCandidateZoom) and (CurrentArea > BestCandidateArea)) then
    begin
      BestCandidate := ImageItem;
      BestCandidateZoom := CurrentZoom;
      BestCandidateArea := CurrentArea;
    end;
  end;

  Result := BestCandidate;
end;

{ MouseUp - Handles mouse button release events }
procedure TFlowmotion.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ImageUnderCursor: TImageItem;
begin
  if FClearing or FInFallAnimation then
    Exit;

  if (Button = mbLeft) then
  begin
    // Handle dragging any image in free float mode
    if FDraggingImage then
    begin
      FDraggingImage := False;

      // Reset zoom when dragging ends
      if FDraggedImage <> nil then
      begin
        FDraggedImage.FHotZoomTarget := 1.0;
        FDraggedImage := nil;
      end;

      MouseCapture := False;

      if FlowLayout = flFreeFloat then
        FBreathingPhase := FBreathingPhase - 0.4;

      // Check if the mouse cursor is still over the image after release
      ImageUnderCursor := GetImageAtPoint(X, Y);
      if (ImageUnderCursor <> nil) then
      begin
        FHotItem := ImageUnderCursor;
        StartAnimationThread;
      end;
    end;

    // Handle dragging of selected image
    if FDraggingSelected or FIsPotentialDrag then
    begin
      FDraggingSelected := False;
      FIsPotentialDrag := False;

      // Reset zoom when dragging ends
      if FSelectedImage <> nil then
        FSelectedImage.FHotZoomTarget := 1.0;

      MouseCapture := False;

      ImageUnderCursor := GetImageAtPoint(X, Y);
      if (ImageUnderCursor = FSelectedImage) and (FSelectedImage <> nil) then
      begin
        FHotItem := FSelectedImage;
        StartAnimationThread;
      end;
    end;
  end;

  inherited MouseUp(Button, Shift, X, Y);
end;

{ MouseMove - Handles mouse movement for hot-tracking and dragging }
procedure TFlowmotion.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHot: TImageItem;
  NewCenterX, NewCenterY: Integer;
  ImageCenter: TPoint;
  CurrentZoneName: string;
  i: Integer;
  ScaleFactor, NearestZoneDistance, Distance: Double;
  ZoneRect: TRect;
  ZoneCenterX, ZoneCenterY: Integer;
  BorderWidth, OffsetX, OffsetY: Integer;
  R: TRect;
  DraggedItem: TImageItem;
const
  DRAG_THRESHOLD = 10;
  MIN_SCALE_WHILE_DRAGGING = 0.92;
  SCALE_DISTANCE = 180;
begin
  if FClearing or FInFallAnimation then
    Exit;

  // ------------------------------------------------------------------
  // 1. DRAGGING – Selected image (Sorted layout) or any image (FreeFloat)
  // ------------------------------------------------------------------
  DraggedItem := nil;
  if FDraggingSelected and (FSelectedImage <> nil) then
    DraggedItem := FSelectedImage
  else if (FFlowLayout = flFreeFloat) and FDraggingImage and (FDraggedImage <> nil) then
    DraggedItem := FDraggedImage;

  if DraggedItem <> nil then
  begin
    // Update target rectangle based on mouse position
    NewCenterX := X - FDragOffset.X;
    NewCenterY := Y - FDragOffset.Y;
    with DraggedItem.TargetRect do
      DraggedItem.TargetRect := Rect(NewCenterX - (Right - Left) div 2, NewCenterY - (Bottom - Top) div 2, NewCenterX + (Right - Left) div 2, NewCenterY + (Bottom - Top) div 2);

    // Keep inside component bounds when no activation zones are used
    if Length(FActivationZones) = 0 then
    begin
      BorderWidth := Max(FGlowWidth, FHotTrackWidth) + 10;
      R := DraggedItem.TargetRect;
      OffsetX := 0;
      OffsetY := 0;
      if R.Left < BorderWidth then
        OffsetX := BorderWidth - R.Left;
      if R.Right > Width - BorderWidth then
        OffsetX := Width - BorderWidth - R.Right;
      if R.Top < BorderWidth then
        OffsetY := BorderWidth - R.Top;
      if R.Bottom > Height - BorderWidth then
        OffsetY := Height - BorderWidth - R.Bottom;
      OffsetRect(R, OffsetX, OffsetY);
      DraggedItem.TargetRect := R;
    end;

    // Slight scale-down while dragging (visual feedback)
    DraggedItem.FHotZoom := MIN_SCALE_WHILE_DRAGGING;
    DraggedItem.FHotZoomTarget := MIN_SCALE_WHILE_DRAGGING;

    // Activation zone scaling effect
    if Length(FActivationZones) > 0 then
    begin
      ImageCenter := Point((DraggedItem.TargetRect.Left + DraggedItem.TargetRect.Right) div 2, (DraggedItem.TargetRect.Top + DraggedItem.TargetRect.Bottom) div 2);

      NearestZoneDistance := MaxDouble;
      for i := Low(FActivationZones) to High(FActivationZones) do
      begin
        ZoneRect := FActivationZones[i].Rect;
        ZoneCenterX := (ZoneRect.Left + ZoneRect.Right) div 2;
        ZoneCenterY := (ZoneRect.Top + ZoneRect.Bottom) div 2;
        Distance := Hypot(ImageCenter.X - ZoneCenterX, ImageCenter.Y - ZoneCenterY);
        if Distance < NearestZoneDistance then
          NearestZoneDistance := Distance;
      end;

      if NearestZoneDistance < SCALE_DISTANCE then
      begin
        ScaleFactor := MIN_SCALE_WHILE_DRAGGING + (1.0 - MIN_SCALE_WHILE_DRAGGING) * (1.0 - NearestZoneDistance / SCALE_DISTANCE);
        DraggedItem.FHotZoom := ScaleFactor;
        DraggedItem.FHotZoomTarget := ScaleFactor;
      end;
    end;

    // Activation zone detection
    if Length(FActivationZones) > 0 then
    begin
      ImageCenter := Point((DraggedItem.TargetRect.Left + DraggedItem.TargetRect.Right) div 2, (DraggedItem.TargetRect.Top + DraggedItem.TargetRect.Bottom) div 2);

      CurrentZoneName := '';
      for i := Low(FActivationZones) to High(FActivationZones) do
      begin
        if PtInRect(FActivationZones[i].Rect, ImageCenter) then
        begin
          CurrentZoneName := FActivationZones[i].Name;
          Break;
        end;
      end;

      if (CurrentZoneName <> '') and (CurrentZoneName <> FLastActivationZoneName) then
      begin
        FLastActivationZoneName := CurrentZoneName;
        if Assigned(FOnSelectedImageEnterZone) then
          FOnSelectedImageEnterZone(Self, DraggedItem, CurrentZoneName);
      end
      else if CurrentZoneName = '' then
        FLastActivationZoneName := '';
    end;

    // In FreeFloat: immediate update (no animation lag)
    if FFlowLayout = flFreeFloat then
      DraggedItem.CurrentRect := DraggedItem.TargetRect;

    StartAnimationThread;
    Exit; // Dragging has priority – skip hot-tracking
  end;

  // ------------------------------------------------------------------
  // 2. Potential drag ? real drag (threshold check for SelectedMovable)
  // ------------------------------------------------------------------
  if FIsPotentialDrag and (FSelectedImage <> nil) then
  begin
    if (Abs(X - FDragStartPos.X) > DRAG_THRESHOLD) or (Abs(Y - FDragStartPos.Y) > DRAG_THRESHOLD) then
    begin
      FDraggingSelected := True;
      FIsPotentialDrag := False;
    end;
  end;

  // ------------------------------------------------------------------
  // 3. Normal hot-tracking (when nothing is being dragged)
  // ------------------------------------------------------------------
  NewHot := GetImageAtPoint(X, Y);

  if not FHotTrackZoom then
  begin
    if FHotItem <> nil then
    begin
      FHotItem.FHotZoomTarget := 1.0;
      FHotItem := nil;
      Invalidate;
    end;
  end
  else
  begin
    if (NewHot = nil) and (FHotItem <> nil) then
    begin
      FHotItem.FHotZoomTarget := 1.0;
      FHotItem := nil;
      StartAnimationThread;
      if not inPaintCycle then
        Invalidate;
    end
    else if (NewHot <> FHotItem) and (NewHot <> nil) then
    begin
      if (FHotItem <> nil) and (FHotItem <> NewHot) then
        FHotItem.FHotZoomTarget := 1.0;
      FHotItem := NewHot;
      StartAnimationThread;
      if not inPaintCycle then
        Invalidate;
    end;
  end;

  // Update cursor
  if FHotItem <> nil then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

{ MouseDown - Handles mouse button press events for selection, double-click and dragging }
procedure TFlowmotion.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ImageItem: TImageItem;
  Index: Integer;
  CaptionRect: TRect;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (FImages.Count = 0) or FClearing or FInFallAnimation then
    Exit;


  // check if click is on a caption
  if Button = mbLeft then
  begin
    for Index := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[Index]);
      CaptionRect := GetCaptionRect(ImageItem, ImageItem.CurrentRect);

      if PtInRect(CaptionRect, Point(X, Y)) then
      begin
        if Assigned(FOnCaptionClick) then
          FOnCaptionClick(Self, ImageItem, Index);
        Exit;
      end;
    end;
  end;

  ImageItem := GetImageAtPoint(X, Y);
  Index := FImages.IndexOf(ImageItem);

  // Click on background - deselect
  if (Button = mbLeft) and (ImageItem = nil) then
  begin
    if FSelectedImage <> nil then
    begin
      DeselectZoomedImage;
      Invalidate;
    end;
    Exit;
  end;


  // Handle double-click
  if (Button = mbLeft) and (ssDouble in Shift) then
  begin
    if ImageItem <> nil then
    begin
      if FSelectedImage <> ImageItem then
        SetSelectedImage(ImageItem, Index);

      if Assigned(FOnSelectedImageDblClick) then
        FOnSelectedImageDblClick(Self, ImageItem, Index);
    end;
    Exit;
  end;


  // Left click on an image
  if Button = mbLeft then
  begin
    // External handler
    if Assigned(FOnSelectedItemMouseDown) then
      FOnSelectedItemMouseDown(Self, ImageItem, Index, X, Y, Button, Shift);

    // 1. FREEFLOAT MODE — EVERY IMAGE IS DRAGGABLE
    if FFlowLayout = flFreeFloat then
    begin
      FDraggingImage := True;
      FDraggedImage := ImageItem;
      FDragOffset.X := X - ((ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) div 2);
      FDragOffset.Y := Y - ((ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) div 2);
      MouseCapture := True;

      // Selection handling in FreeFloat
      if FSelectedImage <> ImageItem then
      begin
        if FSelectedImage <> nil then
        begin
          FSelectedImage.IsSelected := False;
          if FSelectedImage = FWasSelectedItem then
            FWasSelectedItem := nil;
        end;
        FSelectedImage := ImageItem;
        FCurrentSelectedIndex := Index;
        ImageItem.IsSelected := True;
        FHotItem := ImageItem;
        if Assigned(FOnItemSelected) then
          FOnItemSelected(Self, ImageItem, Index);
      end;
      Exit;
    end
    else
    begin
      // 2. SORTED LAYOUT — SELECT IMAGE FIRST, THEN ENABLE DRAGGING
      if FSelectedImage <> ImageItem then
      begin
        // Small tactile "dip" when clicking a hot-tracked (zoomed) image
        if ImageItem.FHotZoom >= 1.1 then
          ImageItem.FHotZoom := ImageItem.FHotZoom - 0.1;

        SetSelectedImage(ImageItem, Index);
      end
      else
        FBreathingPhase := FBreathingPhase - 0.4;
    end;

    // 3. ENABLE DRAGGING FOR SELECTED IMAGE IN SORTED LAYOUT
    if FSelectedMovable and (ImageItem = FSelectedImage) then
    begin
      FDraggingSelected := True;
      FDragOffset.X := X - ((ImageItem.CurrentRect.Left + ImageItem.CurrentRect.Right) div 2);
      FDragOffset.Y := Y - ((ImageItem.CurrentRect.Top + ImageItem.CurrentRect.Bottom) div 2);
      MouseCapture := True;
    end
    else if FSelectedImage = ImageItem then
    begin
      // Already selected ? small breathing push for tactile feedback
      if (not FDraggingImage) and (FlowLayout <> flFreeFloat) then
        FBreathingPhase := FBreathingPhase - 0.4;
    end;
  end;
end;



 { adds list of pictures with loaded positions }
procedure TFlowmotion.AddImagesWithPositions(const FileNames, Captions, Paths: TStringList; const Positions: array of TRect);
var
  i: Integer;
  Bitmap: TBitmap;
  NewItem: TImageItem;
begin
  if (FileNames = nil) or (Captions = nil) or (Paths = nil) or (FileNames.Count <> Captions.Count) or (FileNames.Count <> Paths.Count) or (FileNames.Count <> Length(Positions)) then
    Exit;

  for i := 0 to FileNames.Count - 1 do
  begin
    if not FileExists(FileNames[i]) then
    begin
      DoImageLoad(FileNames[i], False);
      Continue;
    end;

    Bitmap := TBitmap.Create;
    try
      LoadImageFromFile(FileNames[i], Bitmap);

      NewItem := TImageItem.Create;
      NewItem.Bitmap.Assign(Bitmap);
      NewItem.Caption := Captions[i];
      NewItem.Path := Paths[i];
      NewItem.FileName := FileNames[i];
      NewItem.Direction := GetEntryDirection;
      NewItem.Visible := False;

      FImages.Add(NewItem);
      FAllFiles.Add(FileNames[i]);
      FAllCaptions.Add(Captions[i]);
      FAllPaths.Add(Paths[i]);

      if Visible then
      begin
        if FFlowLayout = flFreeFloat then
        begin
          // Set the target position directly
          NewItem.TargetRect := Positions[i];
          NewItem.CurrentRect := Positions[i];

          // Animate with entry animation to saved target position
          AnimateImage(NewItem, NewItem.Direction, True, Positions[i]); // True = UseSavedPosition
        end
        else
        begin
          CalculateLayout;
          AnimateImage(NewItem, NewItem.Direction, False, Rect(0, 0, 0, 0)); // False = UseSavedPosition
        end;

        NewItem.Visible := True;
      end;
    finally
      Bitmap.Free;
    end;
  end;

  StartAnimationThread;
end;

{ Starts zoom animation for an image (zoom in or zoom out) }
procedure TFlowmotion.StartZoomAnimation(ImageItem: TImageItem; ZoomIn: Boolean);
var
  CenterX, CenterY: Integer;
  ImageSize: TSize;
begin
  if ImageItem = nil then
    Exit;
  ImageItem.ZoomProgress := 0;
  ImageItem.Animating := True;
  ImageItem.StartRect := ImageItem.CurrentRect;

  case FZoomAnimationType of
    zatSlide:
      ImageItem.StartRect := ImageItem.CurrentRect;
    zatFade:
      begin
        if ZoomIn then
        begin
          ImageItem.StartRect := ImageItem.CurrentRect;
          ImageItem.Alpha := 0;
          ImageItem.TargetAlpha := 255;
        end
        else
        begin
          ImageItem.Alpha := 255;
          ImageItem.TargetAlpha := 0;
        end;
      end;
    zatZoom:
      begin
        if ZoomIn then
        begin
          CenterX := ImageItem.CurrentRect.Left + (ImageItem.CurrentRect.Right - ImageItem.CurrentRect.Left) div 2;
          CenterY := ImageItem.CurrentRect.Top + (ImageItem.CurrentRect.Bottom - ImageItem.CurrentRect.Top) div 2;
          // StartRect is a point in the center, but keep CurrentRect at current position
          // so the image stays visible until TimerAnimation starts interpolating
          ImageItem.StartRect := Rect(CenterX, CenterY, CenterX, CenterY);
          // Don't change CurrentRect here - it will be interpolated in TimerAnimation
          // This keeps the image visible during the first paint
          ImageItem.Alpha := 255;
        end
        else
          ImageItem.StartRect := ImageItem.CurrentRect;
      end;
    zatBounce:
      begin
        ImageItem.StartRect := ImageItem.CurrentRect;
        ImageItem.Alpha := 255;
        ImageItem.TargetAlpha := 255;
      end;
  end;

  if ZoomIn then
  begin
    ImageSize := GetOptimalSize(ImageItem.Bitmap.Width, ImageItem.Bitmap.Height, Min(FMaxZoomSize, Self.Width div 2), Min(FMaxZoomSize, Self.Height div 2));
    CenterX := (Self.Width - ImageSize.cx) div 2;
    CenterY := (Self.Height - ImageSize.cy) div 2;
    ImageItem.TargetRect := Rect(CenterX, CenterY, CenterX + ImageSize.cx, CenterY + ImageSize.cy);
    CalculateLayout;
  end;
end;

{ SetSelectedImage - Sets the selected image and starts zoom animations }
procedure TFlowmotion.SetSelectedImage(ImageItem: TImageItem; Index: Integer);
var
  OldSelected: TImageItem;
begin
  if ImageItem = nil then
    FHotItem := nil;

  if (ImageItem = nil) and (FSelectedImage <> nil) and (FSelectedImage.ZoomProgress > 0.1) and (FSelectedImage.ZoomProgress < 1) then
    Exit;

  if FSelectedImage = ImageItem then
    Exit;

  OldSelected := FSelectedImage;

  // Handle old selection
  if OldSelected <> nil then
  begin
    OldSelected.IsSelected := False;
    OldSelected.ZoomProgress := 0;
    if OldSelected.FHotZoom >= 1 then
      OldSelected.FHotZoom := 1.1;
  end;

  // Set new selection
  FWasSelectedItem := FSelectedImage;
  if FWasSelectedItem <> nil then
  begin
    FWasSelectedItem.FAnimating := True;
    // If the old selected item was also the hot item, reset its hot-zoom
    if FWasSelectedItem = FHotItem then
      FWasSelectedItem.FHotZoomTarget := 1.0;
  end;

  FSelectedImage := ImageItem;
  FCurrentSelectedIndex := Index;

  if ImageItem <> nil then
  begin
    ImageItem.IsSelected := True;
    ImageItem.ZoomProgress := 0;
    if ImageItem.FHotZoom < 1 then
      ImageItem.FHotZoom := 1;

    // If breathing is enabled, immediately set the new image as the hot item.
    // This synchronizes the states and prevents the target from being reset to 1.0,
    // which would cause a flicker when breathing kicks in.
    if FBreathingEnabled then
      FHotItem := ImageItem
    else
      ImageItem.FHotZoomTarget := 1.0; // Only reset if breathing is OFF

    FCurrentSelectedIndex := Index;
    ImageItem.AnimationProgress := 0;
    ImageItem.Animating := True;
  end
  else
  begin
    FCurrentSelectedIndex := -1;
    // If we are deselecting, clear the hot item too
    FHotItem := nil;
  end;

  // Only calculate layout if NOT in free float mode
  if (FFlowLayout <> flFreeFloat) then
    CalculateLayout;

  // Only zoom to center if NOT in free float mode
  if (FZoomSelectedtoCenter and (FFlowLayout <> flFreeFloat)) then
  begin
    if OldSelected <> nil then
      StartZoomAnimation(OldSelected, False);

    // Start zoom animation for the new selected image
    if ImageItem <> nil then
      StartZoomAnimation(ImageItem, True);
  end;

  StartAnimationThread;

  Invalidate;

  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self, ImageItem, Index);
end;



{ Main paint procedure: draws background and all images in (almost always) correct z-order }{ Main paint procedure: draws background and all images in (almost always) correct z-order }
procedure TFlowmotion.Paint;
var
  i: Integer;
  ImageItem: TImageItem;
  DrawRect: TRect;
  AnimatingItems: TList;
  EnteringItems: TList;
  // --------------------------------------------------------------
  // Compare function for correct Z-ordering during animations.
  // It uses a two-level sort:
  // 1. Primary: Sort by HotZoom factor (descending). More zoomed items have priority.
  // 2. Secondary: If HotZoom is equal, sort by on-screen area (descending).
  // This ensures actively zooming items appear on top of static ones.
  // --------------------------------------------------------------
  // The CompareHotZoom function can be simplified or reverted to the original,
  // as it will no longer handle entering images.

  function CompareHotZoom(Item1, Item2: Pointer): Integer;
  var
    Zoom1, Zoom2: Double;
    Area1, Area2: Integer;
    Rect1, Rect2: TRect;
  begin
    Zoom1 := TImageItem(Item1).FHotZoom;
    Zoom2 := TImageItem(Item2).FHotZoom;
    if Zoom1 > Zoom2 then
      Result := 1
    else if Zoom1 < Zoom2 then
      Result := -1
    else
    begin
      Rect1 := TImageItem(Item1).CurrentRect;
      Rect2 := TImageItem(Item2).CurrentRect;
      if IsRectEmpty(Rect1) then
        Rect1 := TImageItem(Item1).TargetRect;
      if IsRectEmpty(Rect2) then
        Rect2 := TImageItem(Item2).TargetRect;
      Area1 := (Rect1.Right - Rect1.Left) * (Rect1.Bottom - Rect1.Top);
      Area2 := (Rect2.Right - Rect2.Left) * (Rect2.Bottom - Rect2.Top);
      if Area1 > Area2 then
        Result := 1
      else if Area1 < Area2 then
        Result := -1
      else
        Result := 0;
    end;
  end;

  {        //gr32 test too slow like that:
 procedure GR32Stretch(DestCanvas: TCanvas; const DestRect: TRect; SrcBitmap: TBitmap);
 var
   View: TCustomImgView32;
   Temp32: TBitmap32;
   TargetW, TargetH: Integer;
 begin
   if SrcBitmap.Empty then Exit;

   TargetW := DestRect.Right - DestRect.Left;
   TargetH := DestRect.Bottom - DestRect.Top;
   if (TargetW <= 0) or (TargetH <= 0) then Exit;

   View := TCustomImgView32.Create(nil);
   Temp32 := TBitmap32.Create;
   try
     Temp32.SetSize(TargetW, TargetH);

     View.ScaleMode := smResize;
     View.Bitmap.Resampler := TKernelResampler.Create(View.Bitmap);
     TKernelResampler(View.Bitmap.Resampler).Kernel := TLanczosKernel.Create;
     // oder TMitchellKernel.Create;

     View.Bitmap.Assign(SrcBitmap);

     View.PaintTo(Temp32, Rect(0, 0, TargetW, TargetH));

     Temp32.DrawTo(DestCanvas.Handle, DestRect.Left, DestRect.Top);
   finally
     Temp32.Free;
     View.Free;
   end;
 end;
       }
{               /7syngdiplus test not worked
procedure SmartStretchDraw(DestCanvas: TCanvas; const DestRect: TRect; SrcBitmap: TBitmap; Alpha: Byte = 255);
var
 SynPic: TSynPicture;
 TempBitmap: TBitmap;
begin
 if not Assigned(DestCanvas) or not Assigned(SrcBitmap) or SrcBitmap.Empty then
   Exit;

 if not Gdip.Exists then
 begin
   Exit;
 end;

 SynPic := TSynPicture.Create;
 TempBitmap := TBitmap.Create;
 try
   TempBitmap.Width := SrcBitmap.Width;
   TempBitmap.Height := SrcBitmap.Height;
   TempBitmap.PixelFormat := pf32bit;
   TempBitmap.Canvas.Draw(0, 0, SrcBitmap);
   SynPic.Assign(TempBitmap);
   SynPic.Draw(DestCanvas, DestRect);
 finally
   SynPic.Free;
   TempBitmap.Free;
 end;
end; }

// HALFTONE test
{  procedure SmartStretchDraw(DestCanvas: TCanvas; const DestRect: TRect; SrcBitmap: TBitmap; Alpha: Byte = 255);
 var
   OldStretchMode: Integer;
   BlendFunction: TBlendFunction;
   W, H: Integer;
 begin
   if not Assigned(DestCanvas) or not Assigned(SrcBitmap) or SrcBitmap.Empty then
     Exit;

   W := DestRect.Right - DestRect.Left;
   H := DestRect.Bottom - DestRect.Top;

   // --- Case 1: No transparency needed -> Fast HALFTONE method ---
   // This is the fast path. It uses the highly optimized HALFTONE StretchBlt.
   // We check for Alpha, the bitmap's Transparent property, and pixel format.
   if (Alpha >= 255) and (not SrcBitmap.Transparent) and (SrcBitmap.PixelFormat <> pf32bit) then
   begin
     OldStretchMode := GetStretchBltMode(DestCanvas.Handle);
     try
       SetStretchBltMode(DestCanvas.Handle, HALFTONE);
       // Reset brush origin after setting the mode
       SetBrushOrgEx(DestCanvas.Handle, 0, 0, nil);
       StretchBlt(DestCanvas.Handle, DestRect.Left, DestRect.Top, W, H,
                  SrcBitmap.Canvas.Handle, 0, 0, SrcBitmap.Width, SrcBitmap.Height, SRCCOPY);
     finally
       // Always restore the original mode to avoid side effects
       SetStretchBltMode(DestCanvas.Handle, OldStretchMode);
     end;
   end
   // --- Case 2: Transparency is needed -> Slower, but correct Alpha method ---
   else
   begin
     // Here we use your logic from DrawNormalItem.
     // We need a temporary bitmap to scale first and then blend.
     // FTempBitmap must be a TBitmap created elsewhere in your class.
     FTempBitmap.Width := W;
     FTempBitmap.Height := H;

     // We can also use HALFTONE for scaling into the temp bitmap for better quality!
     OldStretchMode := GetStretchBltMode(FTempBitmap.Canvas.Handle);
     try
       SetStretchBltMode(FTempBitmap.Canvas.Handle, HALFTONE);
       SetBrushOrgEx(FTempBitmap.Canvas.Handle, 0, 0, nil);
       StretchBlt(FTempBitmap.Canvas.Handle, 0, 0, W, H,
                  SrcBitmap.Canvas.Handle, 0, 0, SrcBitmap.Width, SrcBitmap.Height, SRCCOPY);
     finally
       SetStretchBltMode(FTempBitmap.Canvas.Handle, OldStretchMode);
     end;

     // Now draw the result with Alpha onto the destination canvas
     BlendFunction.BlendOp := AC_SRC_OVER;
     BlendFunction.BlendFlags := 0;
     BlendFunction.SourceConstantAlpha := Alpha;
     // If SrcBitmap has a per-pixel alpha channel, you would set AC_SRC_ALPHA here
     // and ensure the bitmap has the correct format (pf32bit).
     // For simple global transparency, 0 is correct.
     BlendFunction.AlphaFormat := 0;

     AlphaBlend(DestCanvas.Handle, DestRect.Left, DestRect.Top, W, H,
                FTempBitmap.Canvas.Handle, 0, 0, W, H, BlendFunction);
   end;
 end;          }


  // Renders caption with semi-transparent background — used by both draw methods
  procedure DrawCaption(Item: TImageItem; const DrawRect: TRect);
  var
    TextRect: TRect;
    CaptionW, CaptionH: Integer;
    BlendFunction: TBlendFunction;
    Lines: TStringList;
    i, LineHeight: Integer;
    MaxCaptionWidth: Integer; // Max width for word wrapping (can be full image width)
    MaxCaptionHeight: Integer; // The hard limit: half of the image height
    CurrentLine: string;
    LineWidth: Integer;
    WordStart, WordEnd: Integer;
    Word: string;
    LineTextWidth: Integer;
    TextX: Integer;
    MaxLinesToShow: Integer; // Number of lines to display after truncation
    MaxLineWidth: Integer;    // Width of the widest line for a snug box
    ActualLinesToShow: Integer; // Helper variable to fix loop bounds
    CurrentCaptionColor: TColor;       // *** MODIFICATION: Holds the color to use for this item ***
    CurrentCaptionBackground: TColor; // *** MODIFICATION: Holds the background to use for this item ***
  begin
    if not FShowCaptions or (Item.Caption = '') then
      Exit;
    if FCaptionOnHoverOnly and (Item <> FHotItem) and (Item <> FSelectedImage) then
      Exit;

    Canvas.Font.Assign(FCaptionFont);
    Canvas.Font.Color := FCaptionColor;

    // --- Step 1: Define constraints ---
    // The height is hard constraint. The caption box cannot be taller than this.
    MaxCaptionHeight := (DrawRect.Bottom - DrawRect.Top) div 2;
    // The width is flexible. We use the full image width for wrapping to minimize height.
    MaxCaptionWidth := (DrawRect.Right - DrawRect.Left) - 20;

    // --- Step 2: ALWAYS perform word wrapping ---
    // This simplifies logic. If the text is short, Lines will just contain one entry.
    Lines := TStringList.Create;
    Lines.Capacity := 2;   //limit lines to 2
    try
      CurrentLine := '';
      i := 1;
      while i <= Length(Item.Caption) do
      begin
        // Skip leading spaces
        while (i <= Length(Item.Caption)) and (Item.Caption[i] = ' ') do
          Inc(i);
        if i > Length(Item.Caption) then
          Break;
        // Find word boundaries
        WordStart := i;
        while (i <= Length(Item.Caption)) and (Item.Caption[i] <> ' ') do
          Inc(i);
        WordEnd := i - 1;
        // Extract the word
        Word := Copy(Item.Caption, WordStart, WordEnd - WordStart + 1);
        // Calculate width if we add this word
        if CurrentLine = '' then
          LineWidth := Canvas.TextWidth(Word)
        else
          LineWidth := Canvas.TextWidth(CurrentLine + ' ' + Word);
        // If adding this word would exceed max width, start a new line
        if LineWidth > MaxCaptionWidth then
        begin
          if CurrentLine <> '' then
            Lines.Add(CurrentLine);
          CurrentLine := Word;
        end
        else
        begin
          if CurrentLine = '' then
            CurrentLine := Word
          else
            CurrentLine := CurrentLine + ' ' + Word;
        end;
      end;
      if CurrentLine <> '' then
        Lines.Add(CurrentLine);

      // --- Step 3: Check if the resulting height is too high and truncate lines ---
      LineHeight := Canvas.TextHeight('Hg') + 2;

      //max 2 lines height for background too
      MaxCaptionHeight := LineHeight*2;

      // Calculate how many lines can fit into the max height.
      MaxLinesToShow := 2; // (MaxCaptionHeight - 6) div LineHeight;
      if MaxLinesToShow < 1 then
        MaxLinesToShow := 1; // Always show at least one line.

      // Determine the actual number of lines we will draw.
      // This is the crucial fix to prevent "List index out of bounds".
      ActualLinesToShow := Min(MaxLinesToShow, Lines.Count);

      // If we have more lines than space, truncate the list's height.
      if Lines.Count > MaxLinesToShow then
      begin
        // The final height will be our maximum allowed height.
        CaptionH := MaxCaptionHeight;
      end
      else
      begin
        // It fits, so the height is based on the actual number of lines.
        CaptionH := Lines.Count * LineHeight + 12;
      end;

      // --- Step 4: Calculate the final width of the caption box ---
      // Find the width of the widest line that will actually be displayed.
      MaxLineWidth := 0;
      // BUG FIX: Loop only up to the number of lines that actually exist
      for i := 0 to ActualLinesToShow - 1 do
      begin
        LineTextWidth := Canvas.TextWidth(Lines[i]);
        if LineTextWidth > MaxLineWidth then
          MaxLineWidth := LineTextWidth;
      end;
      // Add padding to the widest line to get the final box width.
      CaptionW := MaxLineWidth + 24;

      // --- Step 5: Position and draw the bottom-aligned caption box ---
      TextRect.Left := DrawRect.Left + (DrawRect.Right - DrawRect.Left - CaptionW) div 2;
      TextRect.Top := DrawRect.Bottom - CaptionH - FCaptionOffsetY; // Bottom-aligned
      TextRect.Right := TextRect.Left + CaptionW;
      TextRect.Bottom := TextRect.Top + CaptionH;

      // Simple clipping to keep it on screen
      if TextRect.Bottom > ClientHeight then
        Dec(TextRect.Top, TextRect.Bottom - ClientHeight);
      if TextRect.Top < 0 then
        TextRect.Top := 0;
      TextRect.Bottom := TextRect.Top + CaptionH;

      FTempBitmap.Width := CaptionW;
      FTempBitmap.Height := CaptionH;

      // *** MODIFICATION: Choose colors based on selection state ***
      if Item.IsSelected then
      begin
        CurrentCaptionColor := FSelectedCaptionColor;
        CurrentCaptionBackground := FSelectedCaptionBackground;
      end
      else
      begin
        CurrentCaptionColor := FCaptionColor;
        CurrentCaptionBackground := FCaptionBackground;
      end;

      FTempBitmap.Canvas.Brush.Color := CurrentCaptionBackground;
      FTempBitmap.Canvas.FillRect(Rect(0, 0, CaptionW, CaptionH));

      FTempBitmap.Canvas.Font.Assign(FCaptionFont);
      FTempBitmap.Canvas.Font.Color := CurrentCaptionColor;
      FTempBitmap.Canvas.Brush.Style := bsClear;

      // Draw only the lines that fit, centered in the box.
      for i := 0 to ActualLinesToShow - 1 do
      begin
        LineTextWidth := Canvas.TextWidth(Lines[i]);
        TextX := (CaptionW - LineTextWidth) div 2; // Center each line
        FTempBitmap.Canvas.TextOut(TextX, 6 + i * LineHeight, Lines[i]);
      end;

      // Draw with alpha
      BlendFunction.BlendOp := AC_SRC_OVER;
      BlendFunction.BlendFlags := 0;
      BlendFunction.SourceConstantAlpha := FCaptionAlpha;
      BlendFunction.AlphaFormat := 0;

      AlphaBlend(Canvas.Handle, TextRect.Left, TextRect.Top, CaptionW, CaptionH, FTempBitmap.Canvas.Handle, 0, 0, CaptionW, CaptionH, BlendFunction);
    finally
      Lines.Free;
    end;
  end;


// --------------------------------------------------------------
// Draw static item (no zoom, no animation)
// --------------------------------------------------------------
  procedure DrawNormalItem(Item: TImageItem);
  var
    BlendFunction: TBlendFunction;
    W, H: Integer;
  begin
    if not Item.Visible or Item.Bitmap.Empty or (Item.Alpha <= 0) then
      Exit;

    W := Item.CurrentRect.Right - Item.CurrentRect.Left;
    H := Item.CurrentRect.Bottom - Item.CurrentRect.Top;

    if Item.Alpha < 255 then
    begin
      FTempBitmap.Width := W;
      FTempBitmap.Height := H;
      //GR32Stretch(FTempBitmap.Canvas, Rect(0,0,W,H), Item.Bitmap);
      FTempBitmap.Canvas.StretchDraw(Rect(0, 0, W, H), Item.Bitmap);
      //SmartStretchDraw(FTempBitmap.Canvas, Rect(0,0,W,H), Item.Bitmap, Item.FAlpha);

      BlendFunction.BlendOp := AC_SRC_OVER;
      BlendFunction.BlendFlags := 0;
      BlendFunction.SourceConstantAlpha := Item.Alpha;
      BlendFunction.AlphaFormat := 0;

      AlphaBlend(Canvas.Handle, Item.CurrentRect.Left, Item.CurrentRect.Top, W, H, FTempBitmap.Canvas.Handle, 0, 0, W, H, BlendFunction);
    end
    else //SmartStretchDraw(Canvas, Item.CurrentRect, Item.Bitmap, Item.FAlpha);//GR32Stretch(Canvas, Item.CurrentRect, Item.Bitmap);
      Canvas.StretchDraw(Item.CurrentRect, Item.Bitmap);
    DrawCaption(Item, Item.CurrentRect);
  end;

  // --------------------------------------------------------------
  // Draw zoomed + alpha + glow item
  // --------------------------------------------------------------
  procedure DrawHotZoomedItem(Item: TImageItem; IsCurrentHot: Boolean);
  var
    CenterX, CenterY, BaseW, BaseH, NewW, NewH: Integer;
    ZoomFactor: Double;
    R: TRect;
    OffsetX, OffsetY: Integer;
    BorderWidth: Integer;
  begin
    if not Item.Visible or Item.Bitmap.Empty then
      Exit;
    if (not FHotTrackZoom) and (not Item.IsSelected) then
    begin
      DrawNormalItem(Item);
      if IsCurrentHot then
      begin
        R := Item.CurrentRect;
        InflateRect(R, FHotTrackWidth, FHotTrackWidth);
        Canvas.Pen.Width := FHotTrackWidth;
        Canvas.Pen.Color := FHotTrackColor;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;
      Exit;
    end;
  // ===================================================================
  // Base size and center
    if (Item.CurrentRect.Right > Item.CurrentRect.Left) and (Item.CurrentRect.Bottom > Item.CurrentRect.Top) then
    begin
      CenterX := Item.CurrentRect.Left + (Item.CurrentRect.Right - Item.CurrentRect.Left) div 2;
      CenterY := Item.CurrentRect.Top + (Item.CurrentRect.Bottom - Item.CurrentRect.Top) div 2;
      BaseW := Item.CurrentRect.Right - Item.CurrentRect.Left;
      BaseH := Item.CurrentRect.Bottom - Item.CurrentRect.Top;
    end
    else
    begin
      CenterX := Item.TargetRect.Left + (Item.TargetRect.Right - Item.TargetRect.Left) div 2;
      CenterY := Item.TargetRect.Top + (Item.TargetRect.Bottom - Item.TargetRect.Top) div 2;
      BaseW := Item.TargetRect.Right - Item.TargetRect.Left;
      BaseH := Item.TargetRect.Bottom - Item.TargetRect.Top;
    end;
    ZoomFactor := Item.FHotZoom;
    NewW := Round(BaseW * ZoomFactor);
    NewH := Round(BaseH * ZoomFactor);
    R := Rect(CenterX - NewW div 2, CenterY - NewH div 2, CenterX + NewW div 2, CenterY + NewH div 2);
  // Keep inside control (glow or hottrack margin)
    BorderWidth := Max(FGlowWidth, FHotTrackWidth);
    OffsetX := 0;
    OffsetY := 0;
    if R.Left < 0 then
      OffsetX := -R.Left + BorderWidth;
    if R.Right > Width then
      OffsetX := Width - R.Right - BorderWidth;
    if R.Top < 0 then
      OffsetY := -R.Top + BorderWidth;
    if R.Bottom > Height then
      OffsetY := Height - R.Bottom - BorderWidth;
    OffsetRect(R, OffsetX, OffsetY);
    Canvas.StretchDraw(R, Item.Bitmap);
  // Glow / hot border
    if IsCurrentHot or Item.IsSelected then
    begin
      if Item.IsSelected then
        InflateRect(R, FGlowWidth, FGlowWidth)
      else
        InflateRect(R, FHotTrackWidth, FHotTrackWidth);
      Canvas.Pen.Width := ifThen(Item.IsSelected, FGlowWidth, FHotTrackWidth);
      Canvas.Pen.Color := ifThen(Item.IsSelected, FGlowColor, FHotTrackColor);
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;
    DrawCaption(Item, R);
  end;

begin
  if inPaintCycle or FClearing then
    Exit;
  inPaintCycle := True;
  Canvas.Lock;
  try
    // 1. Background
    if not FBackgroundpicture.Empty then
    begin
      if not FBackgroundCacheValid or (FBackgroundCache.Width <> Width) or (FBackgroundCache.Height <> Height) then
      begin
        FBackgroundCache.Width := Width;
        FBackgroundCache.Height := Height;
        FBackgroundCache.Canvas.StretchDraw(Rect(0, 0, Width, Height), FBackgroundpicture);
        FBackgroundCacheValid := True;
      end;
      Canvas.CopyRect(ClientRect, FBackgroundCache.Canvas, ClientRect);
    end
    else
    begin
      Canvas.Brush.Color := FBackgroundColor;
      Canvas.FillRect(ClientRect);
    end;
    AnimatingItems := TList.Create;
    EnteringItems := TList.Create;
    try
      // 2. Collect animating and entering items
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);

        //check if they need to get loaded for lazy
        EnsureBitmapLoaded(ImageItem);

        // A new image is "entering" if its animation progress is very low.
        // These get top priority and are added to their own special list.
        if (ImageItem.AnimationProgress < 0.99) and (ImageItem.FHotZoom < 0.99) and (ImageItem <> FSelectedImage) then
        begin
          EnteringItems.Add(ImageItem);
        end
        // Other animating items (like hot-zoomed ones) go into the regular list.
        // We exclude items already in the EnteringItems list.
        else if (ImageItem <> FSelectedImage) and ((ImageItem.ZoomProgress > 0) or (Abs(ImageItem.FHotZoom - ImageItem.FHotZoomTarget) > HOT_ZOOM_EPSILON) or (ImageItem = FWasSelectedItem)) then
        begin
          AnimatingItems.Add(ImageItem);
        end;
      end;
      // 3. Draw completely static items
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        if (AnimatingItems.IndexOf(ImageItem) >= 0) or (EnteringItems.IndexOf(ImageItem) >= 0) then
          Continue;
        DrawNormalItem(ImageItem);
      end;
      // 4. Draw all other animating items (sorted by zoom)
      if AnimatingItems.Count > 0 then
      begin
        AnimatingItems.Sort(@CompareHotZoom);
        for i := 0 to AnimatingItems.Count - 1 do
        begin
          // If HotTrackZoom is disabled, draw animating items as normal items.
          if FHotTrackZoom then
            DrawHotZoomedItem(TImageItem(AnimatingItems[i]), TImageItem(AnimatingItems[i]) = FHotItem)
          else
            DrawNormalItem(TImageItem(AnimatingItems[i]));
        end;
      end;
      // 5. Current hovered item on top (unless it's selected or entering)
      if (FHotItem <> nil) and (FHotItem <> FSelectedImage) and (EnteringItems.IndexOf(FHotItem) = -1) then
      begin
        // If HotTrackZoom is disabled, draw the hovered item as a normal item.
        if FHotTrackZoom then
          DrawHotZoomedItem(FHotItem, True)
        else
          DrawNormalItem(FHotItem);
      end;
      // 6. Selected item (always on top of non-entering items)
      if FSelectedImage <> nil then
      begin
        // The selected image has its own zoom logic (e.g., ZoomSelectedtoCenter),
        // so we always use DrawHotZoomedItem to respect that, regardless of the HotTrackZoom setting.
        if FSelectedImage.FHotZoom > 1.0 then
          DrawHotZoomedItem(FSelectedImage, FSelectedImage = FHotItem)
        else
        begin
          DrawNormalItem(FSelectedImage);
          DrawRect := FSelectedImage.CurrentRect;
          if IsRectEmpty(DrawRect) then
            DrawRect := FSelectedImage.TargetRect;
          InflateRect(DrawRect, FGlowWidth, FGlowWidth);
          Canvas.Pen.Color := FGlowColor;
          Canvas.Pen.Width := FGlowWidth;
          Canvas.Brush.Style := bsClear;
          Canvas.Rectangle(DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom);
        end;
      end;
      // 7. Draw entering items on the very top
      for i := 0 to EnteringItems.Count - 1 do
      begin
        // The pop-in animation for new images is a hot-zoom animation.
        // It should only be drawn if the HotTrackZoom feature is enabled.
        // If disabled, new images will just appear at their target size without the pop-in effect.
        if FHotTrackZoom then
          DrawHotZoomedItem(TImageItem(EnteringItems[i]), TImageItem(EnteringItems[i]) = FHotItem)
        else
          DrawNormalItem(TImageItem(EnteringItems[i]));
      end;
    finally
      AnimatingItems.Free;
      EnteringItems.Free;
    end;
  finally
    Canvas.Unlock;
    inPaintCycle := False;
  end;
end;


{ Resize - Handles component resize: invalidates background cache and recalculates layout }
procedure TFlowmotion.Resize;
var
  NewSize: TSize;
  L, T: Integer;
begin
  inherited Resize;

  FBackgroundCacheValid := False;
  // Only center selected image if NOT in free float mode
  if FZoomSelectedtoCenter and (FSelectedImage <> nil) and (FFlowLayout <> flFreeFloat) then
  begin
    NewSize := GetOptimalSize(FSelectedImage.Bitmap.Width, FSelectedImage.Bitmap.Height, Min(FMaxZoomSize, Width div 2), Min(FMaxZoomSize, Height div 2));
    L := (Width - NewSize.cx) div 2;
    T := (Height - NewSize.cy) div 2;
    FSelectedImage.TargetRect := Rect(L, T, L + NewSize.cx, T + NewSize.cy);
  end;

  CalculateLayout;
  Invalidate;
end;

{ Sets the number of images per page and refreshes display }
procedure TFlowmotion.SetPageSize(Value: Integer);
begin
  if (Value > 0) and (Value <> FPageSize) then
  begin
    FPageSize := Value;
    ShowPage(0);
  end;
end;

{ Shows a specific page: intelligently loads and displays images for that page }
procedure TFlowmotion.ShowPage(Page: Integer);
var
  ItemIndex, i, StartIdx, EndIdx: Integer;
  Bitmap: TBitmap;
  FileName: string;
  ImageItem: TImageItem;
  LoadedItemsMap: TStringList; // Maps filename to TImageItem for fast lookup
  NewItems: TList; // Temporary list to hold items added in this call
  HasLoadedPositions: Boolean;
  FoundPosition: Boolean;
  j: Integer;
  SavedRect: TRect;
begin
  if not visible then
    Exit;
  if FClearing or FPageChangeInProgress then
    Exit;

  FPageChangeInProgress := True;
  NewItems := TList.Create; // Create the temporary list for new items
  try
    // Wait for any ongoing operations before we start modifying the list
    WaitForAllLoads;
    WaitForAllAnimations;

    // Stop all pending loading threads that are now obsolete
    for i := 0 to FLoadingThreads.Count - 1 do
      TImageLoadThread(FLoadingThreads[i]).Terminate;
    FLoadingThreads.Clear;
    FLoadingCount := 0;

    StartIdx := Page * FPageSize;
    EndIdx := Min(StartIdx + FPageSize, FAllFiles.Count) - 1;

    LoadedItemsMap := TStringList.Create;
    try
      LoadedItemsMap.Sorted := True; // Enable fast binary search lookups
      LoadedItemsMap.Duplicates := dupIgnore;

      // 1. Move all currently loaded items into the map for quick lookup.
      // This effectively removes them from the active FImages list.
      for i := FImages.Count - 1 downto 0 do
      begin
        ImageItem := TImageItem(FImages[i]);
        LoadedItemsMap.AddObject(ImageItem.FileName, ImageItem);
        FImages.Delete(i);
      end;

      // 2. Populate FImages with items for the new page, loading only what's necessary.
      for i := StartIdx to EndIdx do
      begin
        FileName := FAllFiles[i];
        ItemIndex := LoadedItemsMap.IndexOf(FileName);

        sleep(10);
        CheckSynchronize(10);

        if ItemIndex <> -1 then
        begin
          // Item is already loaded. Get it from the map and add it to our active list.
          ImageItem := TImageItem(LoadedItemsMap.Objects[ItemIndex]);
          FImages.Add(ImageItem);
          // Remove from map so it's not freed later. It's now "active" again.
          LoadedItemsMap.Delete(ItemIndex);
        end
        else
        begin
          // Item is not loaded. Load it from file.
          if not FileExists(FileName) then
            Continue;

          Bitmap := TBitmap.Create;
          try
            LoadImageFromFile(FileName, Bitmap);

            ImageItem := TImageItem.Create;
            ImageItem.Bitmap.Assign(Bitmap);
            ImageItem.Caption := FAllCaptions[i];
            ImageItem.Path := FAllPaths[i];
            ImageItem.FileName := FileName;
            ImageItem.Direction := GetEntryDirection;
            ImageItem.Visible := False; // Initially set new items to be invisible
            FImages.Add(ImageItem);
            NewItems.Add(ImageItem); // Add the new item to our temporary list
          finally
            Bitmap.Free;
          end;
        end;
      end;

      // 3. Free any items left in the map (they were from the old page and are no longer needed).
      for i := 0 to LoadedItemsMap.Count - 1 do
      begin
        TImageItem(LoadedItemsMap.Objects[i]).Free;
      end;

    finally
      LoadedItemsMap.Free;
    end;

    FCurrentPage := Page;

    // Check if we have loaded positions
    HasLoadedPositions := (Length(FLoadedPositions) > 0);

    if HasLoadedPositions then
    begin
      // Use loaded positions for new items
      for j := 0 to Length(FLoadedPositions) - 1 do
      begin
        if FLoadedPositions[j].Path = ImageItem.Path then
        begin
            // Use saved position
          SavedRect := Rect(FLoadedPositions[j].Left, FLoadedPositions[j].Top, FLoadedPositions[j].Left + FLoadedPositions[j].Width, FLoadedPositions[j].Top + FLoadedPositions[j].Height);

            // Validate and clamp the saved rectangle
            // 1. Check for invalid dimensions (e.g., width or height is zero or negative)
          if (SavedRect.Right <= SavedRect.Left) or (SavedRect.Bottom <= SavedRect.Top) then
          begin
            FoundPosition := False; // Discard this invalid position
            Break;
          end;

            // 2. Clamp the rectangle to be within the component's bounds
          if SavedRect.Left < 0 then
            SavedRect.Left := 0;
          if SavedRect.Top < 0 then
            SavedRect.Top := 0;
          if SavedRect.Right > Self.Width then
            SavedRect.Right := Self.Width;
          if SavedRect.Bottom > Self.Height then
            SavedRect.Bottom := Self.Height;

            // 3. Check if the clamped rectangle is still a reasonable size
          if (SavedRect.Right - SavedRect.Left < 10) or (SavedRect.Bottom - SavedRect.Top < 10) then
          begin
            FoundPosition := False; // Discard, too small after clamping
            Break;
          end;

            // If we passed all checks, use the position
          AnimateImage(ImageItem, ImageItem.Direction, True, SavedRect);
          FoundPosition := True;
          Break;
        end;

        // If no saved position found, use default animation
        if not FoundPosition then
          AnimateImage(ImageItem, ImageItem.Direction, False, Rect(0, 0, 0, 0));
      end;

      // Clear all positions after using them
      SetLength(FLoadedPositions, 0);
    end
    else
    begin
      // --- Prepare animations for all new items while they are still invisible ---
      CalculateLayout; // Calculate the final destination (TargetRect) for all images
      for i := 0 to NewItems.Count - 1 do
      begin
        ImageItem := TImageItem(NewItems[i]);
        if Visible then
          AnimateImage(ImageItem, ImageItem.Direction, False, Rect(0, 0, 0, 0)); // Default animation
      end;
    end;

    // --- Now that all items are prepared, make them visible ---
    for i := 0 to NewItems.Count - 1 do
    begin
      TImageItem(NewItems[i]).Visible := True;
    end;

    if FLoadMode = lmLazyAndFree then
      FreeInvisibleBitmaps;

    // Request a repaint. The Paint method will now see items with valid TargetRect and CurrentRect.
    Invalidate;
    StartAnimationThread;

  finally
    NewItems.Free; // Free the temporary list
    FPageChangeInProgress := False;
  end;
end;




{ Navigates to the next page }
procedure TFlowmotion.NextPage;
begin
  if FCurrentPage < GetPageCount - 1 then
    ShowPage(FCurrentPage + 1);
end;

{ Navigates to the previous page }
procedure TFlowmotion.PrevPage;
begin
  if FCurrentPage > 0 then
    ShowPage(FCurrentPage - 1);
end;

{ Selects the next image, or moves to next page if at end }
procedure TFlowmotion.SelectNextImage;
begin
  if FInFallAnimation then
    Exit;
  if FCurrentSelectedIndex < FImages.Count - 1 then
  begin
    // In free float mode, directly select without animation
    if FFlowLayout = flFreeFloat then
    begin
      // Clear previous selection
      if FSelectedImage <> nil then
      begin
        FSelectedImage.IsSelected := False;
        if FSelectedImage = FWasSelectedItem then
          FWasSelectedItem := nil;
      end;

      // Set new selection directly
      FSelectedImage := TImageItem(FImages[FCurrentSelectedIndex + 1]);
      FCurrentSelectedIndex := FCurrentSelectedIndex + 1;
      FSelectedImage.IsSelected := True;
      FHotItem := FSelectedImage;

      // Trigger selection event
      if Assigned(FOnItemSelected) then
        FOnItemSelected(Self, FSelectedImage, FCurrentSelectedIndex);

      Invalidate;
    end
    else
      SetSelectedImage(TImageItem(FImages[FCurrentSelectedIndex + 1]), FCurrentSelectedIndex + 1);
  end
  else if FCurrentPage < GetPageCount - 1 then
  begin
    NextPage;
    if FImages.Count > 0 then
    begin
      if FFlowLayout = flFreeFloat then
      begin
        // Clear previous selection
        if FSelectedImage <> nil then
        begin
          FSelectedImage.IsSelected := False;
          if FSelectedImage = FWasSelectedItem then
            FWasSelectedItem := nil;
        end;

        // Set new selection directly
        FSelectedImage := TImageItem(FImages[0]);
        FCurrentSelectedIndex := 0;
        FSelectedImage.IsSelected := True;
        FHotItem := FSelectedImage;

        // Trigger selection event
        if Assigned(FOnItemSelected) then
          FOnItemSelected(Self, FSelectedImage, FCurrentSelectedIndex);

        Invalidate;
      end
      else
        SetSelectedImage(TImageItem(FImages[0]), 0);
    end;
  end;
end;
{ Selects the previous image, or moves to previous page if at start }

procedure TFlowmotion.SelectPreviousImage;
begin
  if FInFallAnimation then
    Exit;
  if FCurrentSelectedIndex > 0 then
  begin
    // In free float mode, directly select without animation
    if FFlowLayout = flFreeFloat then
    begin
      // Clear previous selection
      if FSelectedImage <> nil then
      begin
        FSelectedImage.IsSelected := False;
        if FSelectedImage = FWasSelectedItem then
          FWasSelectedItem := nil;
      end;

      // Set new selection directly
      FSelectedImage := TImageItem(FImages[FCurrentSelectedIndex - 1]);
      FCurrentSelectedIndex := FCurrentSelectedIndex - 1;
      FSelectedImage.IsSelected := True;
      FHotItem := FSelectedImage;

      // Trigger selection event
      if Assigned(FOnItemSelected) then
        FOnItemSelected(Self, FSelectedImage, FCurrentSelectedIndex);

      Invalidate;
    end
    else
      SetSelectedImage(TImageItem(FImages[FCurrentSelectedIndex - 1]), FCurrentSelectedIndex - 1);
  end
  else if FCurrentPage > 0 then
  begin
    PrevPage;
    if FImages.Count > 0 then
    begin
      if FFlowLayout = flFreeFloat then
      begin
        // Clear previous selection
        if FSelectedImage <> nil then
        begin
          FSelectedImage.IsSelected := False;
          if FSelectedImage = FWasSelectedItem then
            FWasSelectedItem := nil;
        end;

        // Set new selection directly
        FSelectedImage := TImageItem(FImages[FImages.Count - 1]);
        FCurrentSelectedIndex := FImages.Count - 1;
        FSelectedImage.IsSelected := True;
        FHotItem := FSelectedImage;

        // Trigger selection event
        if Assigned(FOnItemSelected) then
          FOnItemSelected(Self, FSelectedImage, FCurrentSelectedIndex);

        Invalidate;
      end
      else
        SetSelectedImage(TImageItem(FImages[FImages.Count - 1]), FImages.Count - 1);
    end;
  end;
end;


// -----------------------------------------------------------------------------
// Caption property setters
// -----------------------------------------------------------------------------
procedure TFlowmotion.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Invalidate;
  Repaint;
end;

procedure TFlowmotion.SetCaptionColor(Value: TColor);
begin
  if FCaptionColor <> Value then
  begin
    FCaptionColor := Value;
    Invalidate;
    Repaint;
  end;
end;

procedure TFlowmotion.SetCaptionBackground(Value: TColor);
begin
  if FCaptionBackground <> Value then
  begin
    FCaptionBackground := Value;
    Invalidate;
    Repaint;
  end;
end;

procedure TFlowmotion.SetSelectedCaptionColor(Value: TColor);
begin
  if FSelectedCaptionColor <> Value then
  begin
    FSelectedCaptionColor := Value;
    Invalidate;
    Repaint;
  end;
end;

procedure TFlowmotion.SetSelectedCaptionBackground(Value: TColor);
begin
  if FSelectedCaptionBackground <> Value then
  begin
    FSelectedCaptionBackground := Value;
    Invalidate;
    Repaint;
  end;
end;

procedure TFlowmotion.SetCaptionAlpha(Value: Byte);
begin
  if FCaptionAlpha <> Value then
  begin
    FCaptionAlpha := Value;
    Invalidate;
    Repaint;
  end;
end;

procedure TFlowmotion.SetCaptionOffsetY(Value: Integer);
begin
  if FCaptionOffsetY <> Value then
  begin
    FCaptionOffsetY := Value;
    Invalidate;
    Repaint;
  end;
end;

procedure TFlowmotion.SetShowCaptions(Value: Boolean);
begin
  if FShowCaptions <> Value then
  begin
    FShowCaptions := Value;
    Invalidate;
    Repaint;
  end;
end;

procedure TFlowmotion.SetCaptionOnHoverOnly(Value: Boolean);
begin
  if FCaptionOnHoverOnly <> Value then
  begin
    FCaptionOnHoverOnly := Value;
    Invalidate;
    Repaint;
  end;
end;

{ define area where no images should be placed automatically at flyin }
procedure TFlowmotion.SetKeepAreaFreeRect(const Value: TRect);
begin
  if not EqualRect(FKeepAreaFreeRect, Value) then
  begin
    FKeepAreaFreeRect := Value;
    // Recalculate layout to move images out of the newly defined free area
    if Visible then
    begin
      CalculateLayout;
      Invalidate;
    end;
  end;
end;


{ Checks if any animations are currently running }
function TFlowmotion.AnimationsRunning: Boolean;
var
  i: Integer;
begin
  Result := False;
  if FInFallAnimation then
    Result := True
  else
    for i := 0 to FImages.Count - 1 do
      if TImageItem(FImages[i]).Animating then
      begin
        Result := True;
        Exit;
      end;
end;

{ Waits until all image loading threads have finished }
procedure TFlowmotion.WaitForAllLoads;
begin
  while (FLoadingCount > 0) do
  begin
    Sleep(5);
    CheckSynchronize(10);
  end;
end;

{ Waits until all animations have finished (with timeout) }
procedure TFlowmotion.WaitForAllAnimations;
var
  StartTick: DWORD;
begin
  StartTick := GetTickCount;
  while AnimationsRunning do
  begin
    if (GetTickCount - StartTick) >= THREAD_CLEANUP_TIMEOUT then
      Exit;
    Sleep(5);
    CheckSynchronize(10);
  end;
end;

end.
