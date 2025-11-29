
{------------------------------------------------------------------------------}
{                                                                              }
{ Flowmotion v0.98                                                             }
{ by Lara Miriam Tamy Reschke                                                  }
{                                                                              }
{ larate@gmx.net                                                               }
{ https://lamita.jimdosite.com                                                 }
{                                                                              }
{------------------------------------------------------------------------------}
{
 ----Latest Changes
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
  Windows, SysUtils, Classes, Graphics, Controls, ExtCtrls, Forms,
  JPEG, Math, Pngimage
  // , GR32, GR32_Image    // Future: Consider using Graphics32 for better performance
;

const
  // Animation constants
  DEFAULT_TIMER_INTERVAL = 42; // ~24 FPS
  MIN_FRAME_TIME = 16; // ~60 FPS max
  DEFAULT_ANIMATION_SPEED = 12;
  DEFAULT_ALPHA = 255;
  MAX_ALPHA = 255;

  // Grid constants
  MIN_GRID_COLS = 3;
  MIN_GRID_ROWS = 3;
  MAX_GRID_COLS = 24;
  GRID_MULTIPLIER = 2.0;

  // Timeouts
  THREAD_CLEANUP_TIMEOUT = 3000;

  // Spacing / Effects
  DEFAULT_GLOW_WIDTH = 2;
  FALL_EXTRA_DISTANCE = 100;
  FALL_STEP_BASE = 25;
  DEFAULT_MAX_ZOOM_SIZE = 400;
  HOT_ZOOM_MIN_FACTOR = 1.2;
  HOT_ZOOM_MAX_FACTOR = 1.3;
  HOT_ZOOM_IN_SPEED = 0.07;
  HOT_ZOOM_OUT_SPEED = 0.05;
  MIN_CELL_SIZE = 22;
  MIN_HOTTRACK_CELL_SIZE = 80;
  HOT_ZOOM_REFERENCE_SIZE = 100;
  BREATHING_AMPLITUDE = 0.8;
  BREATHING_IN_SPEED = 0.0015;
  BREATHING_OUT_SPEED = 0.004;

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

  {
    TFlowmotion - Main component for animated image gallery

    Key Features:
      - Animated grid layout with customizable spacing
      - Zoom functionality for selected images
      - Paging support for large image collections
      - Async image loading with threads
      - Various animation types (slide, fade, zoom, bounce)

    Usage:
      1. Add images via AddImage() or AddImageAsync() or AddImagesAsync()
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
    FLoadingThreads: TList; // Active loading threads
    FLoadingCount: Integer; // Number of loading threads
    FClearing: Boolean;

    // Animation
    FAnimationTimer: TTimer;
    FHotTrackTimer: TTimer;
    FAnimationSpeed: Integer; // 1-100, higher = faster
    FAnimationEasing: Boolean; // Use easing function
    FInFallAnimation: Boolean; // Currently in fall animation
    FFallingOut: Boolean; // Page change fall animation
    FPageOutProgress: Double; // 0.0 to 1.0
    FLastPaintTick: Cardinal; // For FPS limiting
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
    FZoomedPosition: TRect;
    FMaxZoomSize: Integer;
    FZoomAnimationType: TZoomAnimationType;
    FSelectedMovable: Boolean;
      // Allow dragging selected image (feature not fully implemented)
    FDraggingSelected: Boolean;
    FDragStartPos: TPoint;
    FDragImageStartRect: TRect;
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
    FBlockImageEnterDuringLoad: Boolean;

    // Events
    FOnImageLoad: TImageLoadEvent;
    FOnItemSelected: TImageSelectEvent;
    FOnSelectedItemMouseDown: TOnSelectedItemMouseDown;
    FOnAllAnimationsFinished: TOnAllAnimationsFinished;
    FOnSelectedImageDblClick: TOnSelectedImageDblClick;

    // Internal methods - Animation
    function AnimationsRunning: Boolean;
    function EaseInOutQuad(t: Double): Double;
    function GetEntryDirection: TImageEntryStyle;
    procedure WaitForAllAnimations;
    procedure TimerAnimation(Sender: TObject);
    procedure HotTrackTimerTick(Sender: TObject);
    procedure AnimateImage(ImageItem: TImageItem; EntryStyle: TImageEntryStyle);
    procedure StartZoomAnimation(ImageItem: TImageItem; ZoomIn: Boolean);
    procedure SetImageEntryStyle(Value: TImageEntryStyle);
    procedure AnimatePageChange;


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
    TFlowmotion(FOwner).FAnimationTimer.Enabled := True;
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

  FImageEntryStyle := iesRandom;
  FEntryPoint := Point(-1000, -1000);

  FFlowLayout := flSorted;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := DEFAULT_TIMER_INTERVAL;
  FAnimationTimer.OnTimer := TimerAnimation;
  FAnimationTimer.Enabled := False;

  FHotTrackTimer := TTimer.Create(Self);
  FHotTrackTimer.Interval := DEFAULT_TIMER_INTERVAL - 10;
  FHotTrackTimer.OnTimer := HotTrackTimerTick;
  FHotTrackTimer.Enabled := False;

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
  FSelectedMovable := False; // Feature not fully implemented yet
  FDraggingSelected := False;
  FSorted := True;
  FAnimationEasing := True;
  FLoadingCount := 0;
  FGlowColor := clAqua;
  FGlowWidth := DEFAULT_GLOW_WIDTH;
  FMaxZoomSize := DEFAULT_MAX_ZOOM_SIZE;
  FSelectedImage := nil;
  FWasSelectedItem := nil;
  FZoomAnimationType := zatSlide;     // zatSlide, zatFade, zatZoom, zatBounce
  FBackgroundCacheValid := False;
  FPageSize := 100;
  FCurrentPage := 0;
  FCurrentSelectedIndex := -1;
  FLastPaintTick := 0;
  FActive := True;
  FAutoActiveOnMouseMove := False;
  FThreadPriority := tpHigher;
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
    // Stop timer immediately
    FAnimationTimer.Enabled := False;
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
      Application.ProcessMessages;
      Sleep(10);
    end;
    // Force clear remaining threads
    FLoadingThreads.Clear;
    Application.ProcessMessages;
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
    FAnimationTimer.Free;
    FAllFiles.Free;
    FAllCaptions.Free;
    FAllPaths.Free;
  except
    // Ensure destructor completes even if errors occur
  end;
  inherited Destroy;
end;

{ Timer event for hot-track zoom animation and breathing effect }
procedure TFlowmotion.HotTrackTimerTick(Sender: TObject);
var
  i: Integer;
  Item: TImageItem;
  AvgCellSize, CurrentW, CurrentH, RelSize: Double;
  BaseZoom, BreathOffset, TargetZoom, Speed: Double;
  NeedRepaint: Boolean;

  function GetAverageCellSize: Double;
  var
    TotalW, TotalH, Count: Integer;
    j: Integer;
  begin
    Result := 1;
    TotalW := 0;
    TotalH := 0;
    Count := 0;
    for j := 0 to FImages.Count - 1 do
      with TImageItem(FImages[j]) do
        if Visible and (CurrentRect.Right > CurrentRect.Left) then
        begin
          Inc(TotalW, CurrentRect.Right - CurrentRect.Left);
          Inc(TotalH, CurrentRect.Bottom - CurrentRect.Top);
          Inc(Count);
        end;
    if Count = 0 then
      Exit;
    Result := (TotalW + TotalH) / (2 * Count);
  end;

begin
  if FClearing or (not Visible) then
  begin
    FHotTrackTimer.Enabled := False;
    Exit;
  end;
  if AnimationsRunning then
  begin
    // new select set back hot for breath
    if FSelectedImage <> nil then
    begin
      if FSelectedImage.FHotZoom < 1 then
        FSelectedImage.FHotZoom := 1.0;
      FSelectedImage.FHotZoomTarget := 1.0;
    end;
    Exit; // HotTrack pause
  end;

  AvgCellSize := GetAverageCellSize;

  // Advance breathing phase ONLY when selected AND hovered
  if FBreathingEnabled then
    if (FSelectedImage <> nil) and (FSelectedImage = FHotItem) then
    begin
      if Sin(FBreathingPhase * 2 * Pi) > 0 then
        FBreathingPhase := FBreathingPhase + BREATHING_IN_SPEED // inhale
      else
        FBreathingPhase := FBreathingPhase + BREATHING_OUT_SPEED; // exhale

      if FBreathingPhase >= 1.0 then
        FBreathingPhase := FBreathingPhase - 1.0;
    end;

  for i := 0 to FImages.Count - 1 do
  begin
    Item := TImageItem(FImages[i]);

    // 1. HOVER ? calculate base HotTrack zoom first
    if (Item = FHotItem) and HotTrackZoom and Item.Visible then
    begin
      CurrentW := Item.CurrentRect.Right - Item.CurrentRect.Left;
      CurrentH := Item.CurrentRect.Bottom - Item.CurrentRect.Top;
      RelSize := (CurrentW + CurrentH) / 2.0 / AvgCellSize;

      if RelSize < 0.6 then
        BaseZoom := HOT_ZOOM_MAX_FACTOR
      else if RelSize < 1.0 then
        BaseZoom := HOT_ZOOM_MAX_FACTOR - (HOT_ZOOM_MAX_FACTOR - HOT_ZOOM_MIN_FACTOR) * (RelSize - 0.6) / 0.4
      else
        BaseZoom := HOT_ZOOM_MIN_FACTOR + (HOT_ZOOM_MAX_FACTOR - HOT_ZOOM_MIN_FACTOR) * (1.0 / RelSize) * 0.5;

      BaseZoom := EnsureRange(BaseZoom, HOT_ZOOM_MIN_FACTOR, HOT_ZOOM_MAX_FACTOR);

      // If it's the selected image ? breathe AROUND the base zoom (never below!)
      if FBreathingEnabled and (Item = FSelectedImage) then
      begin
        BreathOffset := BREATHING_AMPLITUDE * 0.2 * (Sin(FBreathingPhase * 2 * Pi) + 1);
        TargetZoom := BaseZoom + BreathOffset;
      end
      else
        TargetZoom := BaseZoom * HOT_ZOOM_MIN_FACTOR;

      // Speed: fast in, slow out
      if Item.FHotZoom < TargetZoom then
        Speed := HOT_ZOOM_IN_SPEED
      else
        Speed := HOT_ZOOM_OUT_SPEED;
    end

      // 2. Selected but NOT hovered ? stay at 1.0
    else if Item = FSelectedImage then
    begin
      TargetZoom := 1.0;
      Speed := HOT_ZOOM_OUT_SPEED;
    end

      // 3. Normal image ? back to 1.0
    else
    begin
      TargetZoom := 1.0;
      Speed := HOT_ZOOM_OUT_SPEED;
    end;

    Item.FHotZoomTarget := TargetZoom;

    if Abs(Item.FHotZoom - TargetZoom) > 0.006 then
    begin
      Item.FHotZoom := Item.FHotZoom + (TargetZoom - Item.FHotZoom) * Speed;
    end
    else
      Item.FHotZoom := TargetZoom;
  end;

  NeedRepaint := False;

  for i := 0 to FImages.Count - 1 do
    if Abs(TImageItem(FImages[i]).FHotZoom - TImageItem(FImages[i]).FHotZoomTarget) > 0.006 then
    begin
      NeedRepaint := True;
      Break;
    end;

  if (FSelectedImage <> nil) and (FSelectedImage = FHotItem) then
    NeedRepaint := True;

  if NeedRepaint then
    Invalidate
  else
    FHotTrackTimer.Enabled := False;
end;

{ Deselects the currently zoomed/selected image }
procedure TFlowmotion.DeselectZoomedImage;
begin
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
    FAnimationTimer.Enabled := FActive;
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

{ Adds an image from file (uses filename as caption and path) }
procedure TFlowmotion.AddImage(const FileName: string);
begin
  AddImage(FileName, ExtractFileName(FileName), FileName);
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
begin
  WasEmpty := (FAllFiles.Count = 0);
  // Add to master list if not already present
  if FAllFiles.IndexOf(FileName) = -1 then
  begin
    FAllFiles.Add(FileName);
    FAllCaptions.Add(ACaption);
    FAllPaths.Add(APath);
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
      AddImage(Bitmap);
      with TImageItem(FImages[FImages.Count - 1]) do
      begin
        Caption := ACaption;
        Direction := GetEntryDirection;
        Path := APath;
      end;
      DoImageLoad(FileName, True);
    except
      on E: Exception do
      begin
        Bitmap.Free;
        Exit;
      end;
    end;
    Bitmap.Free;
    FAnimationTimer.Enabled := true;
  end
  else
  begin
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
  FAnimationTimer.Enabled := True;
end;

{ Scrolls to and selects an image by absolute index (switches page if needed) }
procedure TFlowmotion.ScrollToIndex(Index: Integer; Animate: Boolean = True);
var
  PageStart, PageEnd: Integer;
  ImageItem: TImageItem;
begin
  PageStart := GetPageStartIndex;
  PageEnd := GetPageEndIndex;
  // If index is not on current page, switch to the correct page
  if (Index < PageStart) or (Index > PageEnd) then
  begin
    if (Index >= 0) and (Index < FAllFiles.Count) then
    begin
      FCurrentPage := Index div FPageSize;
      if (FCurrentPage >= 0) and (FCurrentPage < PageCount) then
        ShowPage(FCurrentPage);
      ScrollToIndex(Index, Animate);
    end;
    Exit;
  end;
  // Select the image on current page
  if (Index >= 0) and (Index < FImages.Count) then
  begin
    FAnimationTimer.Enabled := True;
    ImageItem := TImageItem(FImages[Index]);
    if Animate then
      SetSelectedImage(ImageItem, Index)
    else
    begin
      FCurrentSelectedIndex := Index;
      SetSelectedImage(ImageItem, Index);
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
  Thread: TImageLoadThread;
  WasEmpty: Boolean;
begin
  WasEmpty := (FAllFiles.Count = 0);
  WaitForAllLoads;
  FLoadingCount := 0;
  FBlockImageEnterDuringLoad := True;
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

  WaitForAllLoads;
  FBlockImageEnterDuringLoad := False;

  if WasEmpty then
    ShowPage(FCurrentPage);
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

{ Waits until all image loading threads have finished }
procedure TFlowmotion.WaitForAllLoads;
begin
  while (FLoadingCount > 0) do
  begin
    Application.ProcessMessages;
    Sleep(5);
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

{ Clears all images with animation options and optional zoom to target position }
procedure TFlowmotion.Clear(animated: Boolean; ZoominSelected: Boolean; SelectedTargetPos, FallingTargetPos: TRect; FallingStyle: TImageEntryStyle = iesFromBottom);
var
  i: Integer;
  StartTick: DWORD;
  ImageItem: TImageItem;
  AllOut, SelectedDone: Boolean;
  ShrinkFactor: Real;
  R: TRect;
  NewW, NewH, CurCX, CurCY, CurW, CurH, TargetCX, TargetCY, MoveX, MoveY, Speed: Integer;
  SelectedItem: TImageItem;
begin
  FallingStyle := iesFromPoint;   //   iesFromCenter
  FallingTargetPos := SelectedTargetPos;

  if FImages.Count = 0 then
    Exit;
  WaitForAllLoads;
  WaitForAllAnimations;

  for i := 0 to FLoadingThreads.Count - 1 do
  begin
    try
      TImageLoadThread(FLoadingThreads[i]).Terminate;
    except

    end;
  end;

  FLoadingThreads.Clear;
  FLoadingCount := 0;
  FAnimationTimer.Enabled := False;
  FHotTrackTimer.Enabled := False;
  FInFallAnimation := True;
  SelectedDone := False;
  SelectedItem := nil;

  if ZoominSelected then
    for i := 0 to FImages.Count - 1 do
      if TImageItem(FImages[i]).IsSelected
      then SelectedItem := TImageItem(FImages[i]);

  for i := 0 to FImages.Count - 1 do
  begin
    TImageItem(FImages[i]).FHotZoom := 1.0;
    TImageItem(FImages[i]).FHotZoomTarget := 1.0;
    TImageItem(FImages[i]).FAnimating := False;
  end;

  try

    StartTick := GetTickCount;
    if animated and (FImages.Count > 0) then
    begin

      // Phase 1: Animate non-selected images flying out
      repeat
        AllOut := True;

        Speed := FAnimationSpeed +2;

        for i := 0 to FImages.Count - 1 do
        begin
          ImageItem := TImageItem(FImages[i]);

          if
           (ImageItem.IsSelected and (not ZoominSelected) and (SelectedTargetPos.Left = FallingTargetPos.Left)
            and (SelectedTargetPos.Top = FallingTargetPos.Top) )
            or
           ((not ImageItem.IsSelected) or (ImageItem.IsSelected and ((not ZoominSelected) and IsRectEmpty(SelectedTargetPos)))) then
          begin
            R := ImageItem.CurrentRect;

            if not SelectedDone
             then SelectedDone := ImageItem = SelectedItem;

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

                  ImageItem.FHotZoom := 1.0;
                  ImageItem.FHotZoomTarget := 1.0;
                  TargetCX := (FallingTargetPos.Left + FallingTargetPos.Right) div 2;
                  TargetCY := (FallingTargetPos.Top + FallingTargetPos.Bottom) div 2;

                  // Calculate movement steps
                  MoveX := (TargetCX - (R.Left + R.Right) div 2) div Max(1, Trunc(FAnimationSpeed * 0.7));
                  MoveY := (TargetCY - (R.Top + R.Bottom) div 2) div Max(1, Trunc(FAnimationSpeed * 0.7));
                 if Abs(MoveX) < 3 then
                    MoveX := Sign(MoveX) * Max(3, Speed);
                  if Abs(MoveY) < 3 then
                    MoveY := Sign(MoveY) * Max(3, Speed);


                  // Shrink image gradually
                  if (R.Right - R.Left > 20) and (R.Bottom - R.Top > 20) then
                  begin
                    CurW := R.Right - R.Left;
                    CurH := R.Bottom - R.Top;
                    ShrinkFactor := 0.92 + (FAnimationSpeed * 0.0015);
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
                if ((Abs(MoveX) <= 20) and (Abs(MoveY) <= 20)) or ((R.Bottom - R.Top) < 30) then
                begin
                  ImageItem.Visible := False;
                end
                else
                begin
                  ImageItem.CurrentRect := R;
                  AllOut := False;
                end;
              end
              else
                // Fallback if no target
                if (R.Bottom < -100) or (R.Top > Height + 100) or (R.Right < -100) or (R.Left > Width + 100) then
              begin
                ImageItem.Visible := False;
              end
              else
              begin
                AllOut := False;
              end;
            end;
          end;

          //to be safe if i calculate something wrong :P
          //if (GetTickCount - StartTick) > THREAD_CLEANUP_TIMEOUT then AllOut := True;

        end;

        Invalidate;
        Application.ProcessMessages;
        Sleep(Trunc(FAnimationSpeed /2));

      until AllOut;

      FAnimationTimer.Enabled := False;
      FHotTrackTimer.Enabled := False; 

      if not SelectedDone then begin
        // Phase 2: Zoom in or move selected image to target position
        if ZoominSelected and (SelectedItem <> nil) then
        begin
          StartTick := GetTickCount;
          repeat
            AllOut := True;
            R := SelectedItem.CurrentRect;
            // Move to target position if specified
            if not IsRectEmpty(SelectedTargetPos) then
            begin
              SelectedItem.FHotZoom := 1.0;
              SelectedItem.FHotZoomTarget := 1.0;
              TargetCX := (SelectedTargetPos.Left + SelectedTargetPos.Right) div 2;
              TargetCY := (SelectedTargetPos.Top + SelectedTargetPos.Bottom) div 2;

              // Calculate movement steps
              MoveX := (TargetCX - (R.Left + R.Right) div 2) div Max(1, Trunc(FAnimationSpeed * 0.7));
              MoveY := (TargetCY - (R.Top + R.Bottom) div 2) div Max(1, Trunc(FAnimationSpeed * 0.7));

              // Shrink image gradually
              if (R.Right - R.Left > 20) and (R.Bottom - R.Top > 20) then
              begin
                CurW := R.Right - R.Left;
                CurH := R.Bottom - R.Top;
                ShrinkFactor := 0.92 + (FAnimationSpeed * 0.0008);
                NewW := Trunc(CurW * ShrinkFactor);
                NewH := Trunc(CurH * ShrinkFactor);
                CurCX := (R.Left + R.Right) div 2;
                CurCY := (R.Top + R.Bottom) div 2;
                R.Left := CurCX - NewW div 2;
                R.Top := CurCY - NewH div 2;
                R.Right := CurCX + NewW div 2;
                R.Bottom := CurCY + NewH div 2;
              end;

              // 3. Move to target position
              OffsetRect(R, MoveX, MoveY);

              // 4. Check if animation is finished
              if ((Abs(MoveX) <= 20) and (Abs(MoveY) <= 20)) or ((R.Bottom - R.Top) < 30) then
              begin
                SelectedItem.Visible := False;
              end
              else
              begin
                SelectedItem.CurrentRect := R;
                AllOut := False;
              end;
            end
            else
            begin
              // Zoom in animation
              CurCX := (R.Left + R.Right) div 2;
              CurCY := (R.Top + R.Bottom) div 2;

              CurW := R.Right - R.Left;
              CurH := R.Bottom - R.Top;

              ShrinkFactor := 1.01 + (FAnimationSpeed * 0.007);

              NewW := Trunc(CurW * ShrinkFactor);
              NewH := Trunc(CurH * ShrinkFactor);

              R.Left := CurCX - NewW div 2;
              R.Top := CurCY - NewH div 2;
              R.Right := CurCX + NewW div 2;
              R.Bottom := CurCY + NewH div 2;

              SelectedItem.CurrentRect := R;

              // Check if image has moved outside visible area
              if (R.Left < 100) or (R.Right > Width - 100) or (R.Top < 100) or (R.Bottom > Height - 100) then
              begin
                SelectedItem.Visible := False;
              end
              else
                AllOut := False;
            end;
            Invalidate;
            Application.ProcessMessages;
            //if ((GetTickCount - StartTick) > THREAD_CLEANUP_TIMEOUT) then AllOut := True;
            Sleep(Trunc(FAnimationSpeed /2));
          until AllOut;
        end;
      end;
    end;
  finally
    // Final
    FClearing := True;
    //-------------------------try to disable all...sometimes one free down there av -only when we send images to targetrect
    //anyone finds why? tell me :D but its still working after atm and happens only once mostly....
    //maybe done now, not had it again...still keeping eye on it
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
    FSelectedImage := nil;
    FWasSelectedItem := nil;
    FCurrentSelectedIndex := -1;
    FCurrentPage := 0;
    FClearing := False;
    FInFallAnimation := False;
    FAnimationTimer.Enabled := False;
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
  CurCX,  CurCY,
  TargetCX, TargetCY,
  MoveX, MoveY: Integer;
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
    FAnimationTimer.Enabled := False;
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

  {  repeat
      R := ImageItem.CurrentRect;
      if R.Top >= Height + 50 then
        Break;
      StepY := 25;
      OffsetRect(R, 0, StepY);
      ImageItem.CurrentRect := R;
      Invalidate;
      Update;
      Application.ProcessMessages;
      Sleep(5);
    until False;  }

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
  if FBlockImageEnterDuringLoad then
    Exit;
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
begin
  // First check selected image (has highest priority)
  if (FSelectedImage <> nil) and FSelectedImage.Visible and PtInRect(FSelectedImage.CurrentRect, Point(X, Y)) then
  begin
    Result := FSelectedImage;
    Exit;
  end;

  // Then check other images in reverse order (top to bottom)
  for i := FImages.Count - 1 downto 0 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if ImageItem.Visible and PtInRect(ImageItem.CurrentRect, Point(X, Y)) and (ImageItem <> FSelectedImage) then
    begin
      Result := ImageItem;
      Exit;
    end;
  end;
  Result := nil;
end;

{ Handles mouse button release events }
procedure TFlowmotion.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FClearing then
    Exit;
  if AnimationsRunning then
    Exit;
  if FDraggingSelected and (Button = mbLeft) then
  begin
    FDraggingSelected := False;
    MouseCapture := False;
  end;
  FLastMouseButton := Button;
  inherited MouseUp(Button, Shift, X, Y);
end;

{ Handles mouse movement for hot-tracking and dragging }
procedure TFlowmotion.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewHot: TImageItem;
begin
  if AnimationsRunning or FClearing then
    Exit;

  NewHot := GetImageAtPoint(X, Y);

  // ==== NEU: Sofortiges MouseLeave erkennen ====
  if (NewHot = nil) and (FHotItem <> nil) then
  begin
    // Maus hat das aktuelle Hot-Bild verlassen ? sofort resetten
    FHotItem.FHotZoomTarget := 1.0;
    FHotItem := nil; // ? wichtig!
    FHotTrackTimer.Enabled := True;
    Invalidate;
  end
  else if (NewHot <> FHotItem) and (NewHot <> nil) then
  begin
    // Maus ist auf ein NEUES Bild gekommen
    if FHotItem <> nil then
      FHotItem.FHotZoomTarget := 1.0; // altes verliert Hot sofort

    FHotItem := NewHot;
    FHotItem.FHotZoomTarget := HOT_ZOOM_MAX_FACTOR;
      // oder deine gewünschte Stärke
    FHotTrackTimer.Enabled := True;
    Invalidate;
  end;

  // Cursor
  if FHotItem <> nil then
    Cursor := crHandPoint
  else
    Cursor := crDefault;
end;

{ Handles mouse button press events for selection and dragging }
procedure TFlowmotion.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ImageItem: TImageItem;
  Index: Integer;
begin
  if FClearing then
    Exit;
  if AnimationsRunning then
    Exit;
  FLastMouseButton := Button;
  inherited MouseDown(Button, Shift, X, Y);
  ImageItem := GetImageAtPoint(X, Y);
  Index := FImages.IndexOf(ImageItem);

  if Button = mbLeft then
  begin
    // Start dragging if selected image is clicked and dragging is enabled
    if (ImageItem = FSelectedImage) and FActive and FSelectedMovable then
    begin
      ImageItem.FHotZoom := ImageItem.FHotZoom - 0.1;
      FDraggingSelected := True;
      FDragStartPos := Point(X, Y);
      FDragImageStartRect := FZoomedPosition;
      MouseCapture := True;
      Exit;
    end;
  end;

  // Handle double click
  if (Button = mbLeft) and (ssDouble in Shift) then
  begin
    if ImageItem <> nil then
    begin
      if FSelectedImage <> ImageItem then
        SetSelectedImage(ImageItem, Index);
      if Assigned(FOnSelectedImageDblClick) then
        FOnSelectedImageDblClick(Self, ImageItem, Index);
      inherited DblClick;
    end;
    Exit;
  end;

  // Normal single click selection
  if Button = mbLeft then
  begin
    if Assigned(FOnSelectedItemMouseDown) and (ImageItem <> nil) then
      FOnSelectedItemMouseDown(Self, ImageItem, Index, X, Y, Button, Shift);

    if (ImageItem <> nil) and (FSelectedImage <> ImageItem) then
    begin
      if ImageItem.FHotZoom >= 1.1 then
        ImageItem.FHotZoom := ImageItem.FHotZoom - 0.1; //tactile reaction
      SetSelectedImage(ImageItem, Index);
    end;

    if (ImageItem <> nil) and (FSelectedImage = ImageItem) then
      FBreathingPhase := FBreathingPhase - 0.4; //tactile reaction on selected

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
  FHotTrackTimer.Enabled := False;
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
  if (ImageItem = nil) and (FSelectedImage <> nil) and (FSelectedImage.ZoomProgress > 0.1) and (FSelectedImage.ZoomProgress < 0.99) then
    Exit;

  if FSelectedImage = ImageItem then
    Exit;
  if ImageItem = nil then
    FHotItem := nil;

  OldSelected := FSelectedImage;

  // Handle old selection
  if OldSelected <> nil then
  begin
    OldSelected.IsSelected := False;
    OldSelected.ZoomProgress := 0;
  end;

  // Set new selection
  FWasSelectedItem := FSelectedImage;
  FSelectedImage := ImageItem;
  FCurrentSelectedIndex := Index;

  if ImageItem <> nil then
  begin
    FHotTrackTimer.Enabled := False;
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

  FAnimationTimer.Enabled := True;

  Invalidate;

  if Assigned(FOnItemSelected) then
    FOnItemSelected(Self, ImageItem, Index);
end;

{ Main animation timer: updates all image positions, zooms, and alpha values }
procedure TFlowmotion.TimerAnimation(Sender: TObject);
var
  i: Integer;
  ImageItem: TImageItem;
  Progress, Eased: Double;
  AnyAnimating, AnyAnimatingAfter, AllFinishedAtStart: Boolean;
  NowTick: Cardinal;

  { Checks if two rectangles are equal }

  function RectsEqual(const A, B: TRect): Boolean;
  begin
    Result := (A.Left = B.Left) and (A.Top = B.Top) and (A.Right = B.Right) and (A.Bottom = B.Bottom);
  end;

  { Checks if an image item has finished all animations }
  function ItemFinished(const It: TImageItem): Boolean;
  begin
    Result := (It.AnimationProgress >= 1.0) and ((It.ZoomProgress = 0.0) or (It.ZoomProgress = 1.0)) and RectsEqual(It.CurrentRect, It.TargetRect);
  end;

begin
  if FClearing then
    Exit;
  if not Visible then
    Exit;
  if FBlockImageEnterDuringLoad then
    Exit;

  // FPS limiter: prevent excessive repaints
  NowTick := GetTickCount;
  if (NowTick - FLastPaintTick) < MIN_FRAME_TIME then
    Exit;
  FLastPaintTick := NowTick;

  // Phase 0: Pre-check - determine if any animations are running
  AnyAnimating := FFallingOut;
  AllFinishedAtStart := not AnyAnimating;

  for i := 0 to FImages.Count - 1 do
  begin
    ImageItem := TImageItem(FImages[i]);
    if not ItemFinished(ImageItem) then
    begin
      AllFinishedAtStart := False;
    end;
  end;

  try
    // Phase 1: Fall animation (page change)
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
        ImageItem.CurrentRect := Rect(Round(ImageItem.StartRect.Left + (ImageItem.TargetRect.Left - ImageItem.StartRect.Left) * Eased), Round(ImageItem.StartRect.Top + (ImageItem.TargetRect.Top - ImageItem.StartRect.Top) * Eased), Round(ImageItem.StartRect.Right + (ImageItem.TargetRect.Right - ImageItem.StartRect.Right) * Eased), Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Eased));
        ImageItem.Alpha := 255;
      end;
    end
    else
      // Phase 2: Normal animations (position, zoom, fade)
    begin
      for i := FImages.Count - 1 downto 0 do
      begin
        ImageItem := TImageItem(FImages[i]);
        ImageItem.Alpha := 255;

        // Position
        if ImageItem.AnimationProgress < 1.0 then
          ImageItem.AnimationProgress := Min(1.0, ImageItem.AnimationProgress + FAnimationSpeed / 100);

        // Zoom
        if ImageItem.IsSelected then
          ImageItem.ZoomProgress := Min(1.0, ImageItem.ZoomProgress + FAnimationSpeed / 100)
        else if ImageItem.ZoomProgress > 0.0 then
          ImageItem.ZoomProgress := Max(0.0, ImageItem.ZoomProgress - FAnimationSpeed / 100);

        // Progress
        Progress := Max(ImageItem.AnimationProgress, ImageItem.ZoomProgress);
        if FAnimationEasing then
          Progress := EaseInOutQuad(Progress);

        // Rect
      //  if FZoomSelectedtoCenter then ImageItem.CurrentRect := ImageItem.TargetRect
      //   else
        ImageItem.CurrentRect := Rect(Round(ImageItem.StartRect.Left + (ImageItem.TargetRect.Left - ImageItem.StartRect.Left) * Progress), Round(ImageItem.StartRect.Top + (ImageItem.TargetRect.Top - ImageItem.StartRect.Top) * Progress), Round(ImageItem.StartRect.Right + (ImageItem.TargetRect.Right - ImageItem.StartRect.Right) * Progress), Round(ImageItem.StartRect.Bottom + (ImageItem.TargetRect.Bottom - ImageItem.StartRect.Bottom) * Progress));

        // Animating
        ImageItem.Animating := not ItemFinished(ImageItem);

        // Zoom + Position done -> reset selected
        if (ImageItem.AnimationProgress >= 1.0) and ((ImageItem.ZoomProgress = 0.0) or (ImageItem.ZoomProgress = 1.0)) then
          FWasSelectedItem := nil;
      end;
    end;

    // --- Phase 2b: Breathing-Effect for Selected Images ---
    if FBreathingEnabled then
      if (FSelectedImage <> nil) and (not FSelectedImage.Animating) and FSelectedImage.Visible then
      begin
        FBreathingPhase := FBreathingPhase + BREATHING_OUT_SPEED;
        if FBreathingPhase >= 1.0 then
          FBreathingPhase := FBreathingPhase - 1.0; // repeat cycle
      end
      else
        FBreathingPhase := 0.0; // Reset if none selected

    // Phase 3: Timer management
    if AllFinishedAtStart then
    begin
      // All animations finished, stop timer
      if FAnimationTimer.Enabled then
        FAnimationTimer.Enabled := False;
      if Assigned(FOnAllAnimationsFinished) then
        FOnAllAnimationsFinished(Self);
      Exit;
    end;

    // Check if any animations are still running
    AnyAnimatingAfter := FFallingOut;
    for i := 0 to FImages.Count - 1 do
    begin
      ImageItem := TImageItem(FImages[i]);
      if ImageItem.Animating then
      begin
        AnyAnimatingAfter := True;
        Break;
      end;
    end;

    if AnyAnimatingAfter then
      Invalidate
    else
    begin
      // All animations finished, draw final frame
      Invalidate;
      FAnimationTimer.Enabled := False;
      if Assigned(FOnAllAnimationsFinished) then
        FOnAllAnimationsFinished(Self);

      // --------------------------------------------------------------
      // FINAL
      // --------------------------------------------------------------
      if (FSelectedImage <> nil) or (FHotItem <> nil) then
      begin
        // hotzoom somewhere still active?
        for i := 0 to FImages.Count - 1 do
        begin
          ImageItem := TImageItem(FImages[i]);
          if Abs(ImageItem.FHotZoom - ImageItem.FHotZoomTarget) > 0.006 then
          begin
            FHotTrackTimer.Enabled := True;
            Break;
          end;
        end;

        // select hot
        if (FSelectedImage <> nil) and (FSelectedImage = FHotItem) then
          FHotTrackTimer.Enabled := True;
      end;
      // --------------------------------------------------------------
    end;

  except
    on E: Exception do
      ; // Silently handle exceptions to prevent timer crashes
  end;
end;

{ Main paint procedure: draws background and all images in correct z-order }
procedure TFlowmotion.Paint;
var
  BackgroundAlpha: Byte;
  i: Integer;
  Border: TRect;
  ImageItem: TImageItem;
  BlendFunction: TBlendFunction;
  StaticImages, AnimatingImages: TList;
  DrawRect: TRect;
  CenterX, CenterY, MinSize: Integer;

  { Draws an image with hot-track zoom effect applied }

  procedure DrawZoomedItem(Item: TImageItem; IsCurrentHot: Boolean);
  var
    CenterX, CenterY, W, H, NewW, NewH: Integer;
    ZoomFactor: Double;
    DrawRect: TRect;
    OffsetX, OffsetY: Integer;
  begin
    if Item = nil then
      Exit;
    if (not Item.Visible) or Item.Bitmap.Empty then
      Exit;
    try
      // If CurrentRect is too small (e.g., zatZoom animation start), use TargetRect center
      if (Item.CurrentRect.Right <= Item.CurrentRect.Left) or (Item.CurrentRect.Bottom <= Item.CurrentRect.Top) then
      begin
        CenterX := (Item.TargetRect.Left + Item.TargetRect.Right) div 2;
        CenterY := (Item.TargetRect.Top + Item.TargetRect.Bottom) div 2;
        W := Item.TargetRect.Right - Item.TargetRect.Left;
        H := Item.TargetRect.Bottom - Item.TargetRect.Top;
      end
      else
      begin
        CenterX := (Item.CurrentRect.Left + Item.CurrentRect.Right) div 2;
        CenterY := (Item.CurrentRect.Top + Item.CurrentRect.Bottom) div 2;
        W := Item.CurrentRect.Right - Item.CurrentRect.Left;
        H := Item.CurrentRect.Bottom - Item.CurrentRect.Top;
      end;
      ZoomFactor := Item.FHotZoom;

      NewW := Round(W * ZoomFactor);
      NewH := Round(H * ZoomFactor);

      // Smart clipping: adjust position if image extends beyond edges
      OffsetX := 0;
      OffsetY := 0;

      DrawRect := Rect(CenterX - NewW div 2, CenterY - NewH div 2, CenterX + NewW div 2, CenterY + NewH div 2);

      if DrawRect.Left < 0 then
        OffsetX := -DrawRect.Left + FGlowWidth;
      if DrawRect.Right > Width then
        OffsetX := Width - DrawRect.Right - FGlowWidth;

      if DrawRect.Top < 0 then
        OffsetY := -DrawRect.Top + FGlowWidth;
      if DrawRect.Bottom > Height then
        OffsetY := Height - DrawRect.Bottom - FGlowWidth;

      OffsetRect(DrawRect, OffsetX, OffsetY);
      Canvas.StretchDraw(DrawRect, Item.Bitmap);

      // Draw border for hot/selected images
      if IsCurrentHot or Item.IsSelected then
      begin
        Border := DrawRect;
        InflateRect(Border, 1, 1);
        Canvas.Pen.Width := FGlowWidth;
        Canvas.Pen.Color := ifThen(Item.IsSelected, FGlowColor, FHotTrackColor);
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle(Border);
      end;
    except

    end;
  end;

  { Draws an image with optional alpha blending }
  procedure DrawImage(Item: TImageItem; WithAlpha: Boolean = False);
  begin
    if Item = nil then
      Exit;
    if (not Item.Visible) or (Item.Alpha <= 0) or Item.Bitmap.Empty then
      Exit;

    try
      // Use alpha blending if requested and alpha is less than 255
      if WithAlpha and (Item.Alpha < 255) then
      begin
        try
          FTempBitmap.Canvas.Lock;
          FTempBitmap.Width := Item.CurrentRect.Right - Item.CurrentRect.Left;
          FTempBitmap.Height := Item.CurrentRect.Bottom - Item.CurrentRect.Top;

          FTempBitmap.Canvas.StretchDraw(Rect(0, 0, FTempBitmap.Width, FTempBitmap.Height), Item.Bitmap);
          BlendFunction.BlendOp := AC_SRC_OVER;
          BlendFunction.BlendFlags := 0;
          BlendFunction.SourceConstantAlpha := Min(Item.Alpha, BackgroundAlpha);
          BlendFunction.AlphaFormat := 0;
          AlphaBlend(Canvas.Handle, Item.CurrentRect.Left, Item.CurrentRect.Top, FTempBitmap.Width, FTempBitmap.Height, FTempBitmap.Canvas.Handle, 0, 0, FTempBitmap.Width, FTempBitmap.Height, BlendFunction);
        finally
          FTempBitmap.Canvas.Unlock;
        end;
      end
      else
      begin
        Canvas.StretchDraw(Item.CurrentRect, Item.Bitmap);
      end;
    except
    end;
  end;

begin
  if FClearing then
    Exit;
  Canvas.Lock;
  try
    // Initialize background alpha for blending
    BackgroundAlpha := 255;

    // Layer 0: Draw background
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
    if FBlockImageEnterDuringLoad then
      Exit;

    // Sort images by layer
    StaticImages := TList.Create;
    AnimatingImages := TList.Create;
    try
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        // Selected images are handled separately in Layer 7, but also add to AnimatingImages if animating
        if ImageItem.IsSelected then
        begin
          if ImageItem.Animating then
            AnimatingImages.Add(ImageItem);
          Continue;
        end;
        if ImageItem = FWasSelectedItem then
          Continue;
        if ImageItem = FHotItem then
          Continue;
        if ImageItem.FHotZoom > 1.0 then
          Continue;
        if ImageItem.Animating then
          AnimatingImages.Add(ImageItem)
        else
          StaticImages.Add(ImageItem);
      end;

      // Layer 1: static images
      for i := 0 to StaticImages.Count - 1 do
      begin
        ImageItem := TImageItem(StaticImages[i]);
        if ImageItem = FHotItem then
          Continue;
        if ImageItem.IsSelected then
          Continue;
        if ImageItem = FWasSelectedItem then
          Continue;
        if ImageItem.FHotZoom > 1.0 then
          Continue;
        DrawImage(ImageItem, ImageItem.Alpha < 255);
      end;

      // Layer 2: animating images
      for i := 0 to AnimatingImages.Count - 1 do
        DrawImage(TImageItem(AnimatingImages[i]), False);

      // Layer 3: Previously hot images (not currently hot, not selected)
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        if ImageItem <> nil then
          if (ImageItem <> FHotItem) and (ImageItem.FHotZoom > 1) then
            if (ImageItem <> FSelectedImage) then
              DrawZoomedItem(ImageItem, False);
      end;

      // Layer 4: Previously hot images that are now selected
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        if ImageItem <> nil then
          if (ImageItem <> FHotItem) and (ImageItem.FHotZoom > 1) then
            if (ImageItem = FSelectedImage) then
              DrawZoomedItem(ImageItem, False);
      end;

      // Layer 5: Currently hot image (mouse hover)
      for i := 0 to FImages.Count - 1 do
      begin
        ImageItem := TImageItem(FImages[i]);
        if ImageItem <> nil then
          if (ImageItem = FHotItem) then
            DrawZoomedItem(ImageItem, True);
      end;

      // Layer 6: Currently selected image (on top, if not hot)
      if (FSelectedImage <> FHotItem) then
        if (FSelectedImage <> nil) then
          if FSelectedImage.FHotZoom <= 1.0 then
          begin
            ImageItem := FSelectedImage;
            // Always draw selected image, even if CurrentRect is very small (zatZoom animation start)
            if ImageItem.Visible and (ImageItem.Alpha > 0) and (ImageItem.Bitmap <> nil) and not ImageItem.Bitmap.Empty then
            begin
              // Check if rect has valid size (not a point)
              if (ImageItem.CurrentRect.Right > ImageItem.CurrentRect.Left) and (ImageItem.CurrentRect.Bottom > ImageItem.CurrentRect.Top) then
              begin
                Canvas.StretchDraw(ImageItem.CurrentRect, ImageItem.Bitmap);
                Canvas.Pen.Color := FGlowColor;
                Canvas.Pen.Width := FGlowWidth;
                Canvas.Brush.Style := bsClear;
                Canvas.Rectangle(ImageItem.CurrentRect);
              end
              else
              begin
                // If rect is a point or invalid (zatZoom start), use TargetRect center with minimum size
                CenterX := (ImageItem.TargetRect.Left + ImageItem.TargetRect.Right) div 2;
                CenterY := (ImageItem.TargetRect.Top + ImageItem.TargetRect.Bottom) div 2;
                MinSize := 20; // Minimum visible size
                DrawRect := Rect(CenterX - MinSize div 2, CenterY - MinSize div 2, CenterX + MinSize div 2, CenterY + MinSize div 2);
                Canvas.StretchDraw(DrawRect, ImageItem.Bitmap);
                Canvas.Pen.Color := FGlowColor;
                Canvas.Pen.Width := FGlowWidth;
                Canvas.Brush.Style := bsClear;
                Canvas.Rectangle(DrawRect);
              end;
            end;
          end;
    finally
      StaticImages.Free;
      AnimatingImages.Free;
    end;
  finally
    Canvas.Unlock;
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
      AnimatePageChange;

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
    FAnimationTimer.Enabled := True;
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

{ Animates all images falling out when changing pages }
procedure TFlowmotion.AnimatePageChange;
var
  i: Integer;
begin
  if FImages.Count = 0 then
    Exit;
  FInFallAnimation := True;
  FFallingOut := True;
  FPageOutProgress := 0;
  for i := 0 to FImages.Count - 1 do
  begin
    with TImageItem(FImages[i]) do
    begin
      StartRect := CurrentRect;
      TargetRect := Rect(CurrentRect.Left, Height + 100, CurrentRect.Right, Height + 100 + (CurrentRect.Bottom - CurrentRect.Top));
      AnimationProgress := 0;
      Animating := True;
      Alpha := 255;
      TargetAlpha := 0;
    end;
  end;
  FAnimationTimer.Enabled := True;
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
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

end.
