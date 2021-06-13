module LightApp

open System.Windows.Forms

open LightVulkanWindow
open LightDevice
open LightRenderer
open LightRenderSystem
open LightModel
open LightObject
open LightState

type LightApp () =
    let window = new LightVulkanWindow (600, 400, "Volcano")
    let device = new LightDevice (window)
    let renderer = new LightRenderer (window, device)

    let mutable state = {
        upTime = System.Diagnostics.Stopwatch.StartNew ()
        gameObjects =
        let quadObject =
            let vertices = [|
                (-1.f, -1.f)
                ( 1.f, -1.f)
                (-1.f,  1.f)
                ( 1.f,  1.f)|]
            let model = new LightModel (device, vertices)
            LightObject (Model = Some model)
        [|quadObject|]}

    let mutable disposed = false

    member _.Run () =
        let renderSystem = new LightRenderSystem (device, renderer.SwapchainRenderPass)
        let drawFunc () =
            match renderer.BeginFrame () with
            | Some commandBuffer ->
                renderer.BeginSwapchainRenderPass commandBuffer
                renderSystem.RenderGameObjects commandBuffer state
                renderer.EndSwapchainRenderPass commandBuffer
                renderer.EndFrame ()
            | None -> ()
            window.Invalidate ()
        window.DrawFunction <- Some drawFunc
        let handleInput (args: KeyEventArgs) =
            match args.KeyCode with
            | Keys.Escape -> exit 0
            | Keys.F5 -> renderSystem.RegenerateWorld ()
            | Keys.F11 -> window.ToggleFullscreen ()
            | _ -> ()
        window.HandleInputFunction <- Some handleInput

        System.Windows.Forms.Application.Run window
        device.Device.WaitIdle ()

    interface System.IDisposable with
        override _.Dispose () =
            if not disposed then
                disposed <- true
    override self.Finalize () = (self :> System.IDisposable).Dispose ()