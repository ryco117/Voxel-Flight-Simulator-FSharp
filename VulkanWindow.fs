module LightVulkanWindow

open System.Diagnostics
open System.Windows.Forms
open Vulkan
open Vulkan.Windows

let primaryScreenDimensions () =
    let screen = Screen.PrimaryScreen.Bounds
    screen.Width, screen.Height

type LightVulkanWindow (width: int, height: int, title: string) as self =
    inherit Form (Text = title, Size = System.Drawing.Size(width, height), FormBorderStyle = FormBorderStyle.Sizable)

    do self.SetStyle (ControlStyles.UserPaint + ControlStyles.Opaque, true)
    do self.UpdateStyles ()
    
    let mutable (drawFunction: (unit -> unit) option) = None
    let mutable resized = false
    do self.Resize.Add(fun _args -> resized <- true)

    let mutable fullscreen = false

    member this.CreateWindowSurface (instance: Instance) =
        let info = new Windows.Win32SurfaceCreateInfoKhr  (Hwnd = this.Handle, Hinstance = Process.GetCurrentProcess().Handle)
        instance.CreateWin32SurfaceKHR info

    member _.Extent
        with get () = let s = self.ClientSize in Extent2D (Width = uint32 s.Width, Height = uint32 s.Height)
        and set (extent: Extent2D) =
            self.ClientSize <- System.Drawing.Size (int extent.Width, int extent.Height)
            self.Resized <- true

    member _.Resized 
        with get () = resized
        and set resized' = resized <- resized'

    member _.DrawFunction
        with get () = drawFunction
        and set func = drawFunction <- func

    member _.ToggleFullscreen () =
        fullscreen <- not fullscreen
        if fullscreen then
            let width, height = primaryScreenDimensions ()
            self.FormBorderStyle <- FormBorderStyle.None
            self.WindowState <- FormWindowState.Normal
            self.Extent <- Extent2D (Width = uint32 width, Height = uint32 height)
            self.Bounds <- Screen.PrimaryScreen.Bounds
        else
            self.FormBorderStyle <- FormBorderStyle.Sizable
            self.Extent <- Extent2D (Width = uint32 width, Height = uint32 height)
            
    // TODO: Refactor logic so key controls aren't governed by Window class
    override _.OnKeyDown args =
        match args.KeyCode with
        | Keys.Escape -> exit 0
        | Keys.F11 -> self.ToggleFullscreen ()
        | _ -> base.OnKeyDown args

    override _.OnPaintBackground _ = ()

    override _.OnPaint _args =
        if self.Visible then
            match drawFunction with
            | Some drawFunc -> drawFunc ()
            | _ -> ()