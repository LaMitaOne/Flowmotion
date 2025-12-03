
{------------------------------------------------------------------------------}
{                                                                              }
{ Flowmotion v0.984                                                            }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{ larate@gmx.net                                                               }
{ https://lamita.jimdosite.com                                                 }
{                                                                              }
{------------------------------------------------------------------------------}
{
 ----Latest Changes
  v 0.984
    - higher hotzoomed get painted above lower hotzoomed
      (think that way almost perfect z-order... as possible for me at least)
    - fixed z-order of new animated incoming single images from Addimage
    - fixed z-order of prev selected animating back from selectnext or prev pic function.
    - fixed that last flicker sometimes of just hotzoomed down, in line,
      that moment before it gets static pic again. Now all...perfect smooth,
      no flicker)
  v 0.983
    - Animations now Threaded, massive performance gain like 20 times faster
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
  v 0.93
    - Bug fixed at single picture add
  v 0.92
    - Some stability and performance improvements
    - Some code cleanup
  v 0.91
    - Fixed performance problem with background pic
  v 0.90
    - Fixed sometimes not working dblclick
    - Layout changed
    - Improved performance
}

unit UFLowmotion;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms, JPEG, Math,
  Pngimage;

const
  // Animation constants
  TARGET_FPS = 30;
  MIN_FRAME_TIME = 1000 div TARGET_FPS;
  DEFAULT_ANIMATION_SPEED = 3;
  DEFAULT_ALPHA = 255;

  // Timeouts
  THREAD_CLEANUP_TIMEOUT = 3000;

  // Spacing / Effects
  DEFAULT_GLOW_WIDTH = 2;
  DEFAULT_MAX_ZOOM_SIZE = 300;
  HOT_ZOOM_MIN_FACTOR = 1.02;
  HOT_ZOOM_MAX_FACTOR = 1.4;
  HOT_ZOOM_IN_SPEED = 0.07;
  HOT_ZOOM_OUT_SPEED = 0.09;
  MIN_CELL_SIZE = 22;
  HOT_ZOOM_IN_PER_SEC = 2.5;
  HOT_ZOOM_OUT_PER_SEC = 3.0;
  HOT_ZOOM_EPSILON = 0.0001;
  BREATHING_AMPLITUDE = 2.0;
  BREATHING_SPEED_PER_SEC = 0.06;

type
  TFlowLayout = (flPerfectSize, flSorted);

  TImageLoadEvent = procedure(Sender: TObject; const FileName: string; Success: Boolean) of object;

  TZoomAnimationType = (zatSlide, zatFade, zatZoom, zatBounce);

  TFlowmotionLoadMode = (lmLoadAll, lmLazy, lmLazyAndFree);

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

  TBooleanGrid = array of array of Boolean;

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
    constructor Create(const AFileName, ACaption, APath: string; AOwner: TObject);
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
    FAnimationThread : TAnimationThread;
    FLoadingThreads: TList; // Active loading threads
    FLoadingCount: Integer; // Number of loading threads
    FClearing: Boolean;

    // Animation
    inPaintCycle: Boolean;  //.paint cycle running atm
    FAnimationSpeed: Integer; // 1-100, higher = faster
    FAnimationEasing: Boolean; // Use easing function
    FInFallAnimation: Boolean; // Currently in fall animation
    FFallingOut: Boolean; // Page change fall animation
    FPageOutProgress: Double; // 0.0 to 1.0
    FLastPaintTick: Cardinal; // For FPS limiting
    FImageEntryStyle: TImageEntryStyle;
    FEntryPoint: TPoint; // only used with iesFromPoint
    FLastHotTrackCalc: Cardinal;

    // Layout
    FFlowLayout: TFlowLayout;
    FKeepSpaceforZoomed: Boolean;
    FSpacing: Integer; // Space between images
    FKeepAspectRatio: Boolean;
    FMaxColumns: Integer; // 0 = auto
    FMaxRows: Integer; // 0 = auto
    FSorted: Boolean; // True = load order, False = size order

    // Visual
    FBackgroundColor: TColor;
    FBackgroundpicture: TBitmap;
    FBackgroundCache: TBitmap;
    FTempBitmap: TBitmap;
    FBackgroundCacheValid: Boolean;
    FHotTrackColor: TColor;
    FHotTrackZoom: Boolean;
    FBreathingPhase: Double;
    FBreathingEnabled: Boolean;
    FZoomSelectedtoCenter: Boolean;
    FGlowColor: TColor;
    FGlowWidth: Integer;

    // Selection & Zoom
    FSelectedImage: TImageItem;
    FWasSelectedItem: TImageItem; // Previous selection (for animation)
    FCurrentSelectedIndex: Integer; // Index in FImages (relative to page)
    FMaxZoomSize: Integer;
    FZoomAnimationType: TZoomAnimationType;
    FSelectedMovable: Boolean;
      // Allow dragging selected image (feature not fully implemented)
    FDraggingSelected: Boolean;
    FHotItem: TImageItem;

    // Paging
    FPageSize: Integer; // Images per page
    FCurrentPage: Integer; // 0-based page index
    FPageChangeInProgress: Boolean;
    FLoadMode: TFlowmotionLoadMode;

    // State
    FActive: Boolean;
    FAutoActiveOnMouseMove: Boolean;
    FThreadPriority: TThreadPriority;
    FLastMouseButton: TMouseButton;

    // Events
    FOnImageLoad: TImageLoadEvent;
    FOnItemSelected: TImageSelectEvent;
    FOnSelectedItemMouseDown: TOnSelectedItemMouseDown;
    FOnAllAnimationsFinished: TOnAllAnimationsFinished;
    FOnSelectedImageDblClick: TOnSelectedImageDblClick;

    // Internal methods - Animation
    procedure WMUser1(var Message: TMessage); message WM_USER + 1;
    procedure WMUser2(var Message: TMessage);
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
    procedure AnimateImage(ImageItem: TImageItem; EntryStyle: TImageEntryStyle);
    procedure StartZoomAnimation(ImageItem: TImageItem; ZoomIn: Boolean);
    procedure SetImageEntryStyle(Value: TImageEntryStyle);


    // Internal methods - Layout
    procedure CalculateLayout;
    procedure CalculateLayoutSorted;
    procedure CalculateLayoutPerfectSized;
    function IsAreaFree(const Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer): Boolean;
    procedure MarkAreaOccupied(var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer);
    procedure PlaceImage(ImageItem: TImageItem; var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer; BaseCellWidth, BaseCellHeight: Integer);
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

    // Internal methods - Utilities
    function GetImageItem(Index: Integer): TImageItem;
    function GetImageCount: Integer;
    function GetLoadingCount: Integer;
    procedure SetSelectedImage(ImageItem: TImageItem; Index: Integer);

    // Property setters
    procedure SetLoadMode(Value: TFlowmotionLoadMode);
    procedure SetFlowLayout(Value: TFlowLayout);
    procedure SetActive(Value: Boolean);
    procedure SetSelectedMovable(Value: Boolean);
    procedure SetSorted(Value: Boolean);
    procedure SetKeepSpaceforZoomed(Value: Boolean);
    procedure SetAutoActiveOnMouseMove(Value: Boolean);
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure SetHotTrackColor(Value: TColor);
    procedure SetGlowColor(Value: TColor);
    procedure SetGlowWidth(Value: Integer);
    procedure SetAnimationSpeed(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetKeepAspectRatio(const Value: Boolean);
    procedure SetHotTrackZoom(const Value: Boolean);
    procedure SetZoomSelectedtoCenter(Value: Boolean);
    procedure SetBreathingEnabled(Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetMaxColumns(const Value: Integer);
    procedure SetMaxRows(const Value: Integer);

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

    // Image management
    procedure AddImage(const FileName: string); overload;
    procedure AddImage(const FileName, ACaption, APath: string); overload;
    procedure AddImage(Bitmap: TBitmap); overload;
    procedure AddImageAsync(const FileName: string; const ACaption: string = ''; const APath: string = '');
    procedure AddImages(const FileNames, Captions, Paths: TStringList);
    procedure AddImagesAsync(const FileNames, Captions, Paths: TStringList);
    procedure InsertPicture(Pic: TPicture; const XCaption, XPath: string);
    procedure InsertImageAsync(const FileName, Caption: string);
    procedure SetImage(Index: Integer; Bitmap: TBitmap);
    procedure RemoveImage(Index: Integer; animated: Boolean = True); overload;
    procedure RemoveImage(Index: Integer; Animated: Boolean; FallingTargetPos: TRect; FallingStyle: TImageEntryStyle); overload;
    procedure Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom); overload;
    procedure Clear(animated: Boolean; ZoominSelected: Boolean = false); overload;
    procedure MoveImageToPos(IndexFrom, IndexTo: Integer);

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
    property SelectedMovable: Boolean read FSelectedMovable write SetSelectedMovable default False;
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
    property AutoActiveOnMouseMove: Boolean read FAutoActiveOnMouseMove write SetAutoActiveOnMouseMove default False;
    property ThreadPriority: TThreadPriority read FThreadPriority write SetThreadPriority;
    property OnSelectedItemMouseDown: TOnSelectedItemMouseDown read FOnSelectedItemMouseDown write FOnSelectedItemMouseDown;
    property OnAllAnimationsFinished: TOnAllAnimationsFinished read FOnAllAnimationsFinished write FOnAllAnimationsFinished;
    property OnSelectedImageDblClick: TOnSelectedImageDblClick read FOnSelectedImageDblClick write FOnSelectedImageDblClick;
    property ZoomAnimationType: TZoomAnimationType read FZoomAnimationType write FZoomAnimationType default zatSlide;
    property PageSize: Integer read FPageSize write SetPageSize;
    property CurrentPage: Integer read FCurrentPage;
    property ImageEntryStyle: TImageEntryStyle read FImageEntryStyle write SetImageEntryStyle default iesRandom;
    property EntryPoint: TPoint read FEntryPoint write FEntryPoint;

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
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FOwner := AOwner;
  FLastTick := GetTickCount;
  FStopRequested := False;
  FEvent := CreateEvent(nil, True, False, nil);
  Priority := tpLower;
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
  procedure ProcessFewMessages;
  var
    Msg: TMsg;
    i: Integer;
  begin
      // Process only a small number of messages (e.g., 10)
      // and then release control. This keeps the main thread responsive.
    for i := 1 to 20 do
    begin
      if not PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
        Break; // No more messages in the queue, so we're done for now
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
  end;
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
    LastTick := NowTick;             // Reset the timer for the next iteration.

    // --- 3. Wait until the next frame is due ---
    // We need to subtract the work duration from the total frame time to achieve our target FPS.
    SleepTime := 0;
    if ElapsedMS < MIN_FRAME_TIME then
    begin
      SleepTime := MIN_FRAME_TIME - ElapsedMS;
    end;

    // --- FIX: The 'cooperative' waiting that gives the main thread time to breathe ---
    // When we wait here, we use the calculated sleep time. This is much more efficient
    // than a fixed Sleep() duration because it allows the thread to wake up immediately
    // if a stop is requested.
    if WaitForSingleObject(FEvent, SleepTime) = WAIT_OBJECT_0 then
      Break // Stop event was triggered, exit the loop immediately.
    else
    begin
      // During the wait time, we use the opportunity to allow the main thread
      // to process its message queue (e.g., Synchronize requests from other threads).
      ProcessFewMessages;
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
  FVisible := True;
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
constructor TImageLoadThread.Create(const AFileName, ACaption, APath: string; AOwner: TObject);
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
var
  PNG: TPNGObject;
  JPEGImage: TJPEGImage;
  Ext: string;
begin
  FSuccess := False;
  if not FileExists(FFileName) then
  begin
    if not Terminated then
      Synchronize(SyncAddImage);
    Exit;
  end;
  Ext := LowerCase(ExtractFileExt(FFileName));

  try
    if (Ext = '.jpg') or (Ext = '.jpeg') then
    begin
      JPEGImage := TJPEGImage.Create;
      try
        JPEGImage.LoadFromFile(FFileName);
        FBitmap.Assign(JPEGImage);
        FSuccess := True;
      finally
        JPEGImage.Free;
      end;
    end
    else if Ext = '.png' then
    begin
      PNG := TPNGObject.Create;
      try
        PNG.LoadFromFile(FFileName);
        FBitmap.Assign(PNG);
        FSuccess := True;
      finally
        PNG.Free;
      end;
    end
    else
    begin
      FBitmap.LoadFromFile(FFileName);
      FSuccess := True;
    end;
  except
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
begin
  try
    if Assigned(FOwner) and (FOwner is TFlowmotion) then
    begin
      if FSuccess then
      begin
        try
          TFlowmotion(FOwner).AddImage(FBitmap);
          with TFlowmotion(FOwner) do
          begin
            if FImages.Count > 0 then
            begin
              if FCaption <> '' then
                TImageItem(FImages[FImages.Count - 1]).Caption := FCaption
              else
                TImageItem(FImages[FImages.Count - 1]).Caption := ExtractFileName(FFileName);
              if FPath <> '' then
                TImageItem(FImages[FImages.Count - 1]).Path := FPath
              else
                TImageItem(FImages[FImages.Count - 1]).Path := FFileName;
            end;
          end;
        finally
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
  FImages := TList.Create;
  FClearing := False;
  FBreathingPhase := 0.0;
  FLoadingThreads := TList.Create;
  FAllFiles := TStringList.Create;
  FAllCaptions := TStringList.Create;
  FAllPaths := TStringList.Create;
  FBackgroundpicture := TBitmap.Create;
  FBackgroundCache := TBitmap.Create;
  FTempBitmap := TBitmap.Create;
  FLastHotTrackCalc := 0;
  FAnimationThread := nil;
  FImageEntryStyle := iesRandom;
  FEntryPoint := Point(-1000, -1000);
  FFlowLayout := flSorted;
  FKeepSpaceforZoomed := False;
  FLoadMode := lmLazy;
  FAnimationSpeed := DEFAULT_ANIMATION_SPEED;
  FSpacing := 0;
  FKeepAspectRatio := True;
  FBackgroundColor := clBlack;
  FHotTrackColor := clTeal;
  HotTrackZoom := True;
  FMaxColumns := 0;
  FMaxRows := 0;
  FSelectedMovable := False;   // Feature not fully implemented yet
  FDraggingSelected := False;
  FSorted := True;
  FAnimationEasing := True;
  FLoadingCount := 0;
  FGlowColor := clAqua;
  FGlowWidth := DEFAULT_GLOW_WIDTH;
  FSelectedImage := nil;
  FWasSelectedItem := nil;
  FZoomAnimationType := zatSlide;     // zatSlide, zatFade, zatZoom, zatBounce   not working atm
  FBackgroundCacheValid := False;
  FPageSize := 100;
  FCurrentPage := 0;
  FCurrentSelectedIndex := -1;
  FLastPaintTick := 0;
  FActive := True;
  FAutoActiveOnMouseMove := False;
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
      CheckSynchronize;
      Sleep(5);
    end;
    // Force clear remaining threads
    FLoadingThreads.Clear;
    CheckSynchronize;
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

{ Sets the flow layout algorithm }
procedure TFlowmotion.SetFlowLayout(Value: TFlowLayout);
begin
  if FFlowLayout <> Value then
  begin
    FFlowLayout := Value;
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

{ Enables or disables animations }
procedure TFlowmotion.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
  end;
end;

{ Sets whether component should auto-activate on mouse move }
procedure TFlowmotion.SetAutoActiveOnMouseMove(Value: Boolean);
begin
  FAutoActiveOnMouseMove := Value;
end;

{ Sets the priority for image loading threads }
procedure TFlowmotion.SetThreadPriority(Value: TThreadPriority);
begin
  FThreadPriority := Value;
end;

{ Inserts a picture from TPicture with caption and path }
procedure TFlowmotion.InsertPicture(Pic: TPicture; const XCaption, XPath: string);
var
  Bitmap: TBitmap;
begin
  if Pic = nil then
    Exit;
  Bitmap := TBitmap.Create;
  try
    Bitmap.Assign(Pic.Graphic);
    AddImage(Bitmap);
    if FImages.Count > 0 then
      with TImageItem(FImages[FImages.Count - 1]) do
      begin
        Caption := XCaption;
        Path := XPath;
      end;
  finally
    Bitmap.Free;
  end;
end;

procedure TFlowmotion.PerformAnimationUpdate(DeltaMS: Cardinal);
var
  i: Integer;
  DeltaTime: Double;
  XNow: Cardinal;
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
    Result := (A.Left = B.Left) and (A.Top = B.Top) and
              (A.Right = B.Right) and (A.Bottom = B.Bottom);
  end;

  // -----------------------------------------------------------------------
  // Returns true when an image item has no more animation work left
  // (position, zoom, hot-zoom, breathing)
  // -----------------------------------------------------------------------
  function ItemFinished(const It: TImageItem): Boolean;
  begin
    Result := (It.AnimationProgress >= 1.0) and
              ((It.ZoomProgress <= 0.0001) or (It.ZoomProgress >= 0.9999)) and
              RectsEqual(It.CurrentRect, It.TargetRect) and
              (Abs(It.FHotZoom - It.FHotZoomTarget) <= 0.006);

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

  XNow := GetTickCount;
  FLastHotTrackCalc := XNow;

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

        TempRect := Rect(
          Round(ImageItem.StartRect.Left   + (ImageItem.TargetRect.Left   - ImageItem.StartRect.Left)   * Eased),
          Round(ImageItem.StartRect.Top    + (ImageItem.TargetRect.Top    - ImageItem.StartRect.Top)    * Eased),
          Round(ImageItem.StartRect.Right  + (ImageItem.TargetRect.Right  - ImageItem.StartRect.Right)  * Eased),
          Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Eased)
        );

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

        TempRect := Rect(
          Round(ImageItem.StartRect.Left   + (ImageItem.TargetRect.Left   - ImageItem.StartRect.Left)   * Progress),
          Round(ImageItem.StartRect.Top    + (ImageItem.TargetRect.Top    - ImageItem.StartRect.Top)    * Progress),
          Round(ImageItem.StartRect.Right  + (ImageItem.TargetRect.Right  - ImageItem.StartRect.Right)  * Progress),
          Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Progress)
        );

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
        if (ImageItem = FWasSelectedItem) and
           (ImageItem.ZoomProgress <= 0.0001) and // Must be fully zoomed out
           RectsEqual(ImageItem.CurrentRect, ImageItem.TargetRect) then // Must be at its final position
        begin
          FWasSelectedItem := nil;
        end;
      end;
    end;

    // ===================================================================
    // PHASE 2.5: Hot-track zoom + Breathing animation
    // ===================================================================
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);

      if not (ImageItem.Visible and HotTrackZoom) then
        Continue;

      // Breathing when selected AND hovered
      if FBreathingEnabled and (ImageItem = FSelectedImage) and (ImageItem = FHotItem) then
        TargetZoom := 1.0 + BREATHING_AMPLITUDE * 0.2 * (Sin(FBreathingPhase * 2 * Pi) + 1.0)

      // Normal hot-zoom when only hovered
      else if ImageItem = FHotItem then
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
      ImageItem.FHotZoomTarget := TargetZoom; // for ItemFinished check

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

procedure TFlowmotion.WMUser1(var Message: TMessage);
begin
  if not FClearing then
    Invalidate;
end;

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
  PNG: TPNGObject;
  JPEGImage: TJPEGImage;
  Ext: string;
begin
  try
    if FileExists(Path) then
    begin
      Ext := LowerCase(ExtractFileExt(Path));

      if (Ext = '.jpg') or (Ext = '.jpeg') then
      begin
        JPEGImage := TJPEGImage.Create;
        try
          JPEGImage.LoadFromFile(Path);
          FBackgroundpicture.Assign(JPEGImage);
        finally
          JPEGImage.Free;
        end;
      end
      else if Ext = '.png' then
      begin
        PNG := TPNGObject.Create;
        try
          PNG.LoadFromFile(Path);
          FBackgroundpicture.Assign(PNG);
        finally
          PNG.Free;
        end;
      end
      else
        FBackgroundpicture.LoadFromFile(Path);
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
begin
  if FHotTrackZoom <> Value then
  begin
    FHotTrackZoom := Value;
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


{ Moves an image from one position to another within the current page }
procedure TFlowmotion.MoveImageToPos(IndexFrom, IndexTo: Integer);
var
  PageStart, PageEnd: Integer;
  Item: Pointer;
begin
  PageStart := GetPageStartIndex;
  PageEnd := GetPageEndIndex;
  if (IndexFrom < PageStart) or (IndexFrom > PageEnd) or (IndexTo < PageStart) or (IndexTo > PageEnd) or (IndexFrom = IndexTo) then
    Exit;
  Item := FImages[IndexFrom];
  FImages.Delete(IndexFrom);
  FImages.Insert(IndexTo, Item);
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
  JPEGImage: TJPEGImage;
  Ext: string;
  IsLastPage: Boolean;
  IsSpaceOnPage: Boolean;
  WasEmpty: Boolean;
  PNG: TPNGObject;
  NewItem: TImageItem;
begin
  WasEmpty := (FAllFiles.Count = 0);
  // Add to master list if not already present
 // if FAllFiles.IndexOf(FileName) = -1 then      //forbid duplicates
  begin
    FAllFiles.Add(FileName);
    FAllCaptions.Add(ACaption);
    FAllPaths.Add(APath);
  end;
  if WasEmpty or (FLoadMode = lmLoadAll) then
  begin
    ShowPage(FCurrentPage);
    Exit;
  end;

  IsLastPage := (FCurrentPage = GetPageCount - 1);
  IsSpaceOnPage := (FImages.Count < FPageSize);
  if WasEmpty or (FLoadMode = lmLoadAll) then
    ShowPage(FCurrentPage);
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
      Ext := LowerCase(ExtractFileExt(FileName));
      if (Ext = '.jpg') or (Ext = '.jpeg') then
      begin
        JPEGImage := TJPEGImage.Create;
        try
          JPEGImage.LoadFromFile(FileName);
          Bitmap.Assign(JPEGImage);
        finally
          JPEGImage.Free;
        end;
      end
      else if Ext = '.png' then
      begin
        PNG := TPNGObject.Create;
        try
          PNG.LoadFromFile(FileName);
          Bitmap.Assign(PNG);
        finally
          PNG.Free;
        end;
      end
      else
        Bitmap.LoadFromFile(FileName);

      NewItem := TImageItem.Create;
      NewItem.Bitmap.Assign(Bitmap);
      NewItem.Caption := ACaption;
      NewItem.Path := APath;
      NewItem.Direction := GetEntryDirection;
      FImages.Add(NewItem);

      if Visible then
      begin
        CalculateLayout;
        AnimateImage(NewItem, NewItem.Direction);
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
  ImageItem.Direction := GetEntryDirection;
  FImages.Add(ImageItem);
  if Visible then
  begin
    CalculateLayout;
    AnimateImage(ImageItem, ImageItem.Direction);
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
  Index: Integer;
  Caption, Path: string;
begin
  // Check if file already exists in master list
  Index := FAllFiles.IndexOf(FileName);
  if Index >= 0 then
  begin
    Caption := FAllCaptions[Index];
    Path := FAllPaths[Index];
  end
  else
  begin
    Caption := ACaption;
    Path := APath;
  end;
  LoadThread := TImageLoadThread.Create(FileName, Caption, Path, Self);
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
    //if FAllFiles.IndexOf(FileNames[i]) = -1 then   //no duplicates with thi
    begin
      FAllFiles.Add(FileNames[i]);
      FAllCaptions.Add(Captions[i]);
      FAllPaths.Add(Paths[i]);
    end;
  end;
  if WasEmpty then
    ShowPage(FCurrentPage);  //else  to do
end;

{ Adds multiple images asynchronously (does not wait) }
procedure TFlowmotion.AddImagesAsync(const FileNames, Captions, Paths: TStringList);
var
  i: Integer;
  Thread: TImageLoadThread;
  WasEmpty: Boolean;
begin
  WasEmpty := (FAllFiles.Count = 0);
  if not WasEmpty then
    WaitForAllLoads;
  FLoadingCount := 0;

  // Add all files to master list
  for i := 0 to FileNames.Count - 1 do
  begin
    if FAllFiles.IndexOf(FileNames[i]) = -1 then
    begin
      FAllFiles.Add(FileNames[i]);
      FAllCaptions.Add(Captions[i]);
      FAllPaths.Add(Paths[i]);
    end;
  end;
  // Start loading threads
  for i := 0 to FileNames.Count - 1 do
  begin
    Thread := TImageLoadThread.Create(FileNames[i], Captions[i], Paths[i], Self);
    FLoadingThreads.Add(Thread);
    Inc(FLoadingCount);
  end;

  if WasEmpty then
    ShowPage(FCurrentPage);
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
begin
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
  AnimSpeed,
  NewW, NewH, CurCX, CurCY, CurW, CurH, TargetCX, TargetCY, MoveX, MoveY: Integer;
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
    if GetTickCount - StartTick > 30 then
    begin
      Application.ProcessMessages;
      if FClearing and (csDestroying in ComponentState) then
        Break;
      if GetAsyncKeyState(VK_ESCAPE) < 0 then
        Break;
    end;
    Sleep(trunc(AnimSpeed / 3));

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
            // Fallback if no target
          if (R.Bottom < -100) or (R.Top > Height + 100) or (R.Right < -100) or (R.Left > Width + 100) then
          ImageItem.Visible := False
        else
          AllOut := False;
      end;

        //to be safe if i calculate something wrong :P
      if (GetTickCount - StartTick) > THREAD_CLEANUP_TIMEOUT then
        AllOut := True;

      Invalidate;
      Application.ProcessMessages;
      Sleep(2);

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
procedure TFlowmotion.InsertImageAsync(const FileName, Caption: string);
var
  Thread: TImageLoadThread;
begin
  Thread := TImageLoadThread.Create(FileName, Caption, FileName, Self);
  Thread.Priority := FThreadPriority;
  FLoadingThreads.Add(Thread);
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

{ Calculates the layout for all images based on current FlowLayout setting }
procedure TFlowmotion.CalculateLayout;
begin
  case FFlowLayout of
    flPerfectSize:
      CalculateLayoutPerfectSized;
    flSorted:
      CalculateLayoutSorted;
  end;
end;

{ Calculates layout with perfect-sized algorithm (not yet implemented) }
procedure TFlowmotion.CalculateLayoutPerfectSized;
begin
  // TODO: Implement perfect-sized layout algorithm
  // This would optimize image sizes to fill available space more efficiently
end;

{ Calculates layout using sorted algorithm: places images in grid based on aspect ratio }
procedure TFlowmotion.CalculateLayoutSorted;
var
  i, Cols, Rows: Integer;
  ImageItem: TImageItem;
  BaseCellWidth, BaseCellHeight: Integer;
  VCount, Row, Col, AddforZoomed: Integer;
  Grid: TBooleanGrid;
  SpanCols, SpanRows: Integer;
  Placed: Boolean;
  ImageAspectRatio: Double;
  VisibleImages: TList;
  SortList: TList;

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
        // Reserve space in center for zoomed image (later get needed size from layout)

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
      //this sorts picturesizes
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

    Cols := Max(3, Ceil(Sqrt(VCount * 2))) + AddforZoomed;
    Rows := Max(3, Ceil(VCount / Cols) + 2) + AddforZoomed;

    while (Cols * Rows < VCount * 2.0) do
    begin
      Inc(Cols);
      if Cols > 12 then
      begin
        Inc(Rows);
        Cols := Max(3, Ceil(Sqrt(VCount * 2.0)));
      end;
    end;
    // mincellsize calc
    BaseCellWidth := Max(MIN_CELL_SIZE, (Width - FSpacing * (Cols + 1)) div Cols);
    BaseCellHeight := Max(MIN_CELL_SIZE, (Height - FSpacing * (Rows + 1)) div Rows);

    // Initialize grid
    SetLength(Grid, Rows, Cols);
    for Row := 0 to Rows - 1 do
      for Col := 0 to Cols - 1 do
        Grid[Row, Col] := False;

    // Place images from top-left to bottom-right
    Row := 0;
    Col := 0;
    for i := 0 to VisibleImages.Count - 1 do
    begin
      ImageItem := TImageItem(VisibleImages[i]);
      if (ImageItem.Bitmap.Width = 0) or (ImageItem.Bitmap.Height = 0) then
        Continue;
      ImageAspectRatio := ImageItem.Bitmap.Width / ImageItem.Bitmap.Height;

      // Determine spanning
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

      // Find next free spot
      Placed := False;
      while (Row <= Rows - SpanRows) and (Col <= Cols - SpanCols) do
      begin
        if IsAreaFree(Grid, Row, Col, SpanRows, SpanCols) then
        begin
          PlaceImage(ImageItem, Grid, Row, Col, SpanRows, SpanCols, BaseCellWidth, BaseCellHeight);
          Placed := True;
          Break;
        end;
        Inc(Col);
        if Col > Cols - SpanCols then
        begin
          Col := 0;
          Inc(Row);
        end;
      end;

      // Fallback: force 1x1 if nothing fits
      if not Placed then
      begin
        Row := 0;
        Col := 0;
        while (Row < Rows) and (Col < Cols) do
        begin
          if IsAreaFree(Grid, Row, Col, 1, 1) then
          begin
            PlaceImage(ImageItem, Grid, Row, Col, 1, 1, BaseCellWidth, BaseCellHeight);
            Placed := True;
            Break;
          end;
          Inc(Col);
          if Col >= Cols then
          begin
            Col := 0;
            Inc(Row);
          end;
        end;
      end;

      // Next image
      if Placed then
      begin
        Inc(Col, SpanCols);
        if Col >= Cols then
        begin
          Col := 0;
          Inc(Row);
        end;
      end;
    end;
  finally
    VisibleImages.Free;
  end;
end;

{ Checks if an area in the grid is free for placing an image }
function TFlowmotion.IsAreaFree(const Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer): Boolean;
var
  r, c: Integer;
  CenterRow, CenterCol, ProtectedSize: Integer;
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

{ Places an image in the grid at the specified position and calculates its target rectangle }
procedure TFlowmotion.PlaceImage(ImageItem: TImageItem; var Grid: TBooleanGrid; Row, Col, SpanRows, SpanCols: Integer; BaseCellWidth, BaseCellHeight: Integer);
var
  X, Y: Integer;
  CellWidth, CellHeight: Integer;
  ImageSize: TSize;
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

  ImageItem.TargetRect := Rect(X, Y, X + ImageSize.cx, Y + ImageSize.cy);
  ImageItem.StartRect := ImageItem.CurrentRect;

  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;

  MarkAreaOccupied(Grid, Row, Col, SpanRows, SpanCols);
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


{ Returns the image item at the specified screen coordinates, or nil if none }
function TFlowmotion.GetImageAtPoint(X, Y: Integer): TImageItem;
var
  i: Integer;
  ImageItem: TImageItem;
  P: TPoint;
  CenterX, CenterY, BaseW, BaseH, DrawW, DrawH: Integer;
  DrawRect: TRect;
  ZoomFactor: Double;
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
    InflateRect(DrawRect, 4, 4);

    if PtInRect(DrawRect, P) then
    begin
      Result := FSelectedImage;
      Exit;
    end;
  end;

  // ==================================================================
  // 2. All other images (back to front)
  // ==================================================================
  for i := FImages.Count - 1 downto 0 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if not ImageItem.Visible or (ImageItem = FSelectedImage) then
      Continue;

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

      if PtInRect(DrawRect, P) then
      begin
        Result := ImageItem;
        Exit;
      end;
    end
    else if PtInRect(ImageItem.CurrentRect, P) then
    begin
      Result := ImageItem;
      Exit;
    end;
  end;
end;


{ Handles mouse button release events }
procedure TFlowmotion.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FClearing then
    Exit;
  if FDraggingSelected and (Button = mbLeft) then
  begin
    FDraggingSelected := False;
    MouseCapture := False;
  end;
  FLastMouseButton := Button;
  inherited MouseUp(Button, Shift, X, Y);
end;

{ Handles mouse movement for hot-tracking }
procedure TFlowmotion.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHot: TImageItem;
begin
  if FClearing then
    Exit;

  NewHot := GetImageAtPoint(X, Y);

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
    begin
      FHotItem.FHotZoomTarget := 1.0;
    end;
    FHotItem := NewHot;
    StartAnimationThread;
    if not inPaintCycle then
      Invalidate;
  end;

  // Cursor
  if FHotItem <> nil then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

{ Handles mouse button press events for selection, double-click and dragging }
procedure TFlowmotion.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ImageItem: TImageItem;
  Index: Integer;
begin
  FLastMouseButton := Button;
  inherited MouseDown(Button, Shift, X, Y);

  ImageItem := GetImageAtPoint(X, Y);
  Index := FImages.IndexOf(ImageItem);

  // ------------------------------------------------------------------
  // 1. Double-click on an image
  // ------------------------------------------------------------------
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

  // ------------------------------------------------------------------
  // 2. Click on background ? deselect and zoom everything back to normal view
  // ------------------------------------------------------------------
  if (Button = mbLeft) and (ImageItem = nil) then
  begin
    if FSelectedImage <> nil then
    begin
      DeselectZoomedImage;   // Smoothly returns all images to normal grid size
      Invalidate;           // Repaint immediately
    end;
    Exit;
  end;

  // ------------------------------------------------------------------
  // 3. Left click on an actual image
  // ------------------------------------------------------------------
  if Button = mbLeft then
  begin
    // External handler (e.g. for dragging support)
    if Assigned(FOnSelectedItemMouseDown) then
      FOnSelectedItemMouseDown(Self, ImageItem, Index, X, Y, Button, Shift);

    // Select new image (if not already selected)
    if FSelectedImage <> ImageItem then
    begin
      // Small tactile "dip" when clicking a hot-tracked (zoomed) image
      if ImageItem.FHotZoom >= 1.1 then
        ImageItem.FHotZoom := ImageItem.FHotZoom - 0.1;

      SetSelectedImage(ImageItem, Index);
    end
    else
    begin
      // Already selected ? small breathing push for tactile feedback
      FBreathingPhase := FBreathingPhase - 0.4;
    end;
  end;
end;

{ Sets up animation for a new image entering the gallery }
procedure TFlowmotion.AnimateImage(ImageItem: TImageItem; EntryStyle: TImageEntryStyle);
var
  StartX, StartY, W, H: Integer;
  Target: TRect;
  EffectiveStyle: TImageEntryStyle;
begin
  if ImageItem = nil then
    Exit;

  Target := ImageItem.TargetRect;
  W := Target.Right - Target.Left;
  H := Target.Bottom - Target.Top;

  // Use the passed style, but resolve Random here
  EffectiveStyle := EntryStyle;
  if EffectiveStyle = iesRandom then
    EffectiveStyle := TImageEntryStyle(Random(8) + 1);  //random without iesFromCenter and iesFromPoint

  case EffectiveStyle of
    iesFromLeft:
      begin
        StartX := -W;
        StartY := Target.Top + (Target.Bottom - Target.Top - H) div 2;
      end;
    iesFromRight:
      begin
        StartX := Width;
        StartY := Target.Top + (Target.Bottom - Target.Top - H) div 2;
      end;
    iesFromTop:
      begin
        StartX := Target.Left + (Target.Right - Target.Left - W) div 2;
        StartY := -H;
      end;
    iesFromBottom:
      begin
        StartX := Target.Left + (Target.Right - Target.Left - W) div 2;
        StartY := Height;
      end;

    iesFromTopLeft:
      begin
        StartX := -W;
        StartY := -H;
      end;
    iesFromTopRight:
      begin
        StartX := Width;
        StartY := -H;
      end;
    iesFromBottomLeft:
      begin
        StartX := -W;
        StartY := Height;
      end;
    iesFromBottomRight:
      begin
        StartX := Width;
        StartY := Height;
      end;

    iesFromCenter:
      begin
        StartX := Target.Left + W div 2;
        StartY := Target.Top + H div 2;
      end;

    iesFromPoint:
      begin
        StartX := FEntryPoint.X - W div 2;
        StartY := FEntryPoint.Y - H div 2;
      end;
  else
    StartX := Target.Left;
    StartY := Target.Top;
  end;

  // =================================================================
  // StartRect: point only for center pop
  // =================================================================
  if EffectiveStyle = iesFromCenter then
    ImageItem.StartRect := Rect(StartX, StartY, StartX, StartY)
  else
    ImageItem.StartRect := Rect(StartX, StartY, StartX + W, StartY + H);

  ImageItem.CurrentRect := ImageItem.StartRect;
  ImageItem.AnimationProgress := 0;
  ImageItem.Animating := True;
 // if ImageItem.FHotZoom <= 1 then ImageItem.FHotZoom := 1.05;

  // =================================================================
  // Fade only for center pop
  // =================================================================
  if EffectiveStyle = iesFromCenter then
  begin
    ImageItem.Alpha := 0;
    ImageItem.TargetAlpha := 255;
  end
  else
  begin
    ImageItem.Alpha := 255;
    ImageItem.TargetAlpha := 255;
  end;
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

  {                          // todo
  case FZoomAnimationType of
    zatSlide:
      ImageItem.StartRect := ImageItem.CurrentRect;
    zatFade:
      begin
        if ZoomIn then
        begin
          ImageItem.StartRect := ImageItem.CurrentRect;
          ImageItem.Alpha := 0;
          ImageItem.TargetAlpha := 220;
        end;
      end;
    zatZoom:
      begin
        if ZoomIn then
        begin
          CenterX := ImageItem.CurrentRect.Left + (ImageItem.CurrentRect.Right -
            ImageItem.CurrentRect.Left) div 2;
          CenterY := ImageItem.CurrentRect.Top + (ImageItem.CurrentRect.Bottom -
            ImageItem.CurrentRect.Top) div 2;
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
     }
  if ZoomIn then
  begin
    ImageSize := GetOptimalSize(ImageItem.Bitmap.Width, ImageItem.Bitmap.Height, Min(FMaxZoomSize, Self.Width div 2), Min(FMaxZoomSize, Self.Height div 2));
    CenterX := (Self.Width - ImageSize.cx) div 2;
    CenterY := (Self.Height - ImageSize.cy) div 2;
    ImageItem.TargetRect := Rect(CenterX, CenterY, CenterX + ImageSize.cx, CenterY + ImageSize.cy);
    CalculateLayout;
  end;
end;

{ Sets the selected image and starts zoom animations }
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
    if OldSelected.FHotZoom >= 1 then OldSelected.FHotZoom := 1.1;
  end;

  // Set new selection
  FWasSelectedItem := FSelectedImage;
  if FWasSelectedItem <> nil then
    FWasSelectedItem.FAnimating := True;
  FSelectedImage := ImageItem;
  FCurrentSelectedIndex := Index;

  if ImageItem <> nil then
  begin
    ImageItem.IsSelected := True;
    ImageItem.ZoomProgress := 0;
    if ImageItem.FHotZoom < 1 then
      ImageItem.FHotZoom := 1;
    ImageItem.FHotZoomTarget := 1;
    ImageItem.AnimationProgress := 0;
    ImageItem.Animating := True;
    // Clear hot item if it's the selected image to avoid drawing conflicts
    if (FHotItem <> nil) and (FHotItem = ImageItem) then
      FHotItem := nil;
  end
  else
    FCurrentSelectedIndex := -1;

  CalculateLayout;

  if FZoomSelectedtoCenter then
  begin
    // Start zoom animations
    if OldSelected <> nil then
      StartZoomAnimation(OldSelected, False);

    if ImageItem <> nil then
      StartZoomAnimation(ImageItem, True);
  end;

  StartAnimationThread;

  Invalidate;

  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self, ImageItem, Index);
end;


{ Main paint procedure: draws background and all images in (almost always) correct z-order }
procedure TFlowmotion.Paint;
var
  i: Integer;
  ImageItem: TImageItem;
  DrawRect: TRect;
  AnimatingItems: TList;

  // --------------------------------------------------------------
  // Compare function: ascending HotZoom ? higher zoom = drawn later = on top
  // --------------------------------------------------------------
  function CompareHotZoom(Item1, Item2: Pointer): Integer;
  var
    Z1, Z2: Double;
  begin
    Z1 := TImageItem(Item1).FHotZoom;
    Z2 := TImageItem(Item2).FHotZoom;
    if Z1 < Z2 then Result := -1
    else if Z1 > Z2 then Result := 1
    else Result := 0;
  end;

  // --------------------------------------------------------------
  // Draw static item (no zoom, no animation)
  // --------------------------------------------------------------
  procedure DrawNormalItem(Item: TImageItem);
  var
    BlendFunction: TBlendFunction;
    W, H: Integer;
  begin
    if not Item.Visible or Item.Bitmap.Empty or (Item.Alpha <= 0) then Exit;

    W := Item.CurrentRect.Right  - Item.CurrentRect.Left;
    H := Item.CurrentRect.Bottom - Item.CurrentRect.Top;

    if Item.Alpha < 255 then
    begin
      FTempBitmap.Width  := W;
      FTempBitmap.Height := H;
      FTempBitmap.Canvas.StretchDraw(Rect(0,0,W,H), Item.Bitmap);

      BlendFunction.BlendOp             := AC_SRC_OVER;
      BlendFunction.BlendFlags          := 0;
      BlendFunction.SourceConstantAlpha := Item.Alpha;
      BlendFunction.AlphaFormat         := 0;

      AlphaBlend(Canvas.Handle,
                 Item.CurrentRect.Left, Item.CurrentRect.Top,
                 W, H,
                 FTempBitmap.Canvas.Handle, 0, 0, W, H,
                 BlendFunction);
    end
    else
      Canvas.StretchDraw(Item.CurrentRect, Item.Bitmap);
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
    BF: TBlendFunction;
  begin
    if not Item.Visible or Item.Bitmap.Empty then Exit;

    // Base size and center
    if (Item.CurrentRect.Right > Item.CurrentRect.Left) and
       (Item.CurrentRect.Bottom > Item.CurrentRect.Top) then
    begin
      CenterX := Item.CurrentRect.Left + (Item.CurrentRect.Right  - Item.CurrentRect.Left) div 2;
      CenterY := Item.CurrentRect.Top  + (Item.CurrentRect.Bottom - Item.CurrentRect.Top)  div 2;
      BaseW   := Item.CurrentRect.Right  - Item.CurrentRect.Left;
      BaseH   := Item.CurrentRect.Bottom - Item.CurrentRect.Top;
    end
    else
    begin
      CenterX := Item.TargetRect.Left + (Item.TargetRect.Right  - Item.TargetRect.Left) div 2;
      CenterY := Item.TargetRect.Top  + (Item.TargetRect.Bottom - Item.TargetRect.Top)  div 2;
      BaseW   := Item.TargetRect.Right  - Item.TargetRect.Left;
      BaseH   := Item.TargetRect.Bottom - Item.TargetRect.Top;
    end;

    ZoomFactor := Item.FHotZoom;
    NewW := Round(BaseW * ZoomFactor);
    NewH := Round(BaseH * ZoomFactor);

    R := Rect(CenterX - NewW div 2, CenterY - NewH div 2,
              CenterX + NewW div 2, CenterY + NewH div 2);

    // Keep inside control (glow margin)
    OffsetX := 0; OffsetY := 0;
    if R.Left   < 0       then OffsetX := -R.Left   + FGlowWidth;
    if R.Right  > Width   then OffsetX := Width  - R.Right - FGlowWidth;
    if R.Top    < 0       then OffsetY := -R.Top    + FGlowWidth;
    if R.Bottom > Height  then OffsetY := Height - R.Bottom - FGlowWidth;
    OffsetRect(R, OffsetX, OffsetY);

    // Draw with alpha if needed
    if Item.Alpha < 255 then
    begin
      FTempBitmap.Width  := NewW;
      FTempBitmap.Height := NewH;
      FTempBitmap.Canvas.StretchDraw(Rect(0,0,NewW,NewH), Item.Bitmap);

      BF.BlendOp             := AC_SRC_OVER;
      BF.BlendFlags          := 0;
      BF.SourceConstantAlpha := Item.Alpha;
      BF.AlphaFormat         := 0;

      AlphaBlend(Canvas.Handle, R.Left, R.Top, NewW, NewH,
                 FTempBitmap.Canvas.Handle, 0, 0, NewW, NewH, BF);
    end
    else
      Canvas.StretchDraw(R, Item.Bitmap);

    // Glow / hot border
    if IsCurrentHot or Item.IsSelected then
    begin
      InflateRect(R, 2, 2);
      Canvas.Pen.Width := FGlowWidth;
      Canvas.Pen.Color := ifThen(Item.IsSelected, FGlowColor, FHotTrackColor);
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    end;
  end;

begin
  if inPaintCycle or FClearing then Exit;
  inPaintCycle := True;
  Canvas.Lock;
  try
    // 1. Background
    if not FBackgroundpicture.Empty then
    begin
      if not FBackgroundCacheValid or (FBackgroundCache.Width <> Width) or (FBackgroundCache.Height <> Height) then
      begin
        FBackgroundCache.Width  := Width;
        FBackgroundCache.Height := Height;
        FBackgroundCache.Canvas.StretchDraw(Rect(0,0,Width,Height), FBackgroundpicture);
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
    try
      // 2. Collect every item that is still animating
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        if //(ImageItem.FAnimationProgress < 1.0) or
           (ImageItem.ZoomProgress > 0.01) or
           (Abs(ImageItem.FHotZoom - ImageItem.FHotZoomTarget) > 0.01) or
           (ImageItem.Alpha < 255) or
           (ImageItem = FWasSelectedItem) then
          AnimatingItems.Add(ImageItem);
      end;

      // 3. Draw completely static items
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        if AnimatingItems.IndexOf(ImageItem) >= 0 then Continue;
        DrawNormalItem(ImageItem);
      end;

      // 4. Draw all animating items – sorted by zoom
      if AnimatingItems.Count > 0 then
      begin
        AnimatingItems.Sort(@CompareHotZoom);
        for i := 0 to AnimatingItems.Count - 1 do
          DrawHotZoomedItem(TImageItem(AnimatingItems[i]),
                            TImageItem(AnimatingItems[i]) = FHotItem);
      end;

      // 5. Current hovered item on top (unless it's selected)
      if (FHotItem <> nil) and (FHotItem <> FSelectedImage) then
        DrawHotZoomedItem(FHotItem, True);

      // 6. Selected item – ALWAYS absolute top
      if FSelectedImage <> nil then
      begin
        if FSelectedImage.FHotZoom > 1.0 then
          DrawHotZoomedItem(FSelectedImage, FSelectedImage = FHotItem)
        else
        begin
          DrawNormalItem(FSelectedImage);
          DrawRect := FSelectedImage.CurrentRect;
          if IsRectEmpty(DrawRect) then DrawRect := FSelectedImage.TargetRect;
          InflateRect(DrawRect, 2, 2);
          Canvas.Pen.Color := FGlowColor;
          Canvas.Pen.Width := FGlowWidth;
          Canvas.Brush.Style := bsClear;
          Canvas.Rectangle(DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom);
        end;
      end;

    finally
      AnimatingItems.Free;
    end;

  finally
    Canvas.Unlock;
    inPaintCycle := False;
  end;
end;



{ Handles component resize: invalidates background cache and recalculates layout }
procedure TFlowmotion.Resize;
var
  NewSize: TSize;
  L, T: Integer;
begin
  inherited Resize;

  FBackgroundCacheValid := False;
  if FZoomSelectedtoCenter and (FSelectedImage <> nil) then
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

{ Shows a specific page: loads and displays images for that page }
procedure TFlowmotion.ShowPage(Page: Integer);
var
  i, StartIdx, EndIdx: Integer;
  Bitmap: TBitmap;
  JPEGImage: TJPEGImage;
  PNG: TPNGObject;
  Ext: string;
begin
  if not visible then
    Exit;
  if FClearing then
    Exit;
  if FPageChangeInProgress then
    Exit;
  FPageChangeInProgress := True;
  try
    WaitForAllLoads;
    WaitForAllAnimations;

    for i := 0 to FLoadingThreads.Count - 1 do
      TImageLoadThread(FLoadingThreads[i]).Terminate;
    FLoadingThreads.Clear;
    FLoadingCount := 0;

    if (Page <> FCurrentPage) and (FImages.Count > 0) then
      clear(true, true);

    for i := FImages.Count - 1 downto 0 do
      TImageItem(FImages[i]).Free;
    FImages.Clear;

    StartIdx := Page * FPageSize;
    EndIdx := Min(StartIdx + FPageSize, FAllFiles.Count) - 1;

    for i := StartIdx to EndIdx do
    begin
      if not FileExists(FAllFiles[i]) then
        Continue;
      Bitmap := TBitmap.Create;
      try
        Ext := LowerCase(ExtractFileExt(FAllFiles[i]));
        if (Ext = '.jpg') or (Ext = '.jpeg') then
        begin
          JPEGImage := TJPEGImage.Create;
          try
            JPEGImage.LoadFromFile(FAllFiles[i]);
            Bitmap.Assign(JPEGImage);
          finally
            JPEGImage.Free;
          end;
        end
        else if Ext = '.png' then
        begin
          PNG := TPNGObject.Create;
          try
            PNG.LoadFromFile(FAllFiles[i]);
            Bitmap.Assign(PNG);
          finally
            PNG.Free;
          end;
        end
        else
          Bitmap.LoadFromFile(FAllFiles[i]);

        AddImage(Bitmap);
        with TImageItem(FImages[FImages.Count - 1]) do
        begin
          Caption := FAllCaptions[i];
          Path := FAllPaths[i];
        end;
      finally
        Bitmap.Free;
      end;
    end;

    FCurrentPage := Page;
    CalculateLayout;
    Invalidate;
    StartAnimationThread;
  finally
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
    SetSelectedImage(TImageItem(FImages[FCurrentSelectedIndex + 1]), FCurrentSelectedIndex + 1)
  else if FCurrentPage < GetPageCount - 1 then
  begin
    NextPage;
    if FImages.Count > 0 then
      SetSelectedImage(TImageItem(FImages[0]), 0);
  end;
end;

{ Selects the previous image, or moves to previous page if at start }
procedure TFlowmotion.SelectPreviousImage;
begin
  if FInFallAnimation then
    Exit;
  if FCurrentSelectedIndex > 0 then
    SetSelectedImage(TImageItem(FImages[FCurrentSelectedIndex - 1]), FCurrentSelectedIndex - 1)
  else if FCurrentPage > 0 then
  begin
    PrevPage;
    if FImages.Count > 0 then
      SetSelectedImage(TImageItem(FImages[FImages.Count - 1]), FImages.Count - 1);
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
    CheckSynchronize;
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
    CheckSynchronize;
  end;
end;

end.

