module LightApp

open System.Windows.Forms

open LightVulkanWindow
open LightDevice
open LightRenderer
open LightRenderSystem
open LightModel
open LightObject
open LightState

let newDefaultState () = {
    demoControls = true
    keyForwardInput = 0.f
    keyLeftInput = 0.f
    keyRightInput = 0.f
    keyBackInput = 0.f
    lastFrameTime = 0.
    lastSpeed = 0.f
    playerPosition = System.Numerics.Vector3 (0.f, 0.125f, -2.5f)
    playerQuaternion = Maths.Vector4.UnitQuaternion
    upTime = System.Diagnostics.Stopwatch.StartNew ()
    gameObjects = Array.empty}

type LightApp () =
    let window = new LightVulkanWindow (600, 400, "Volcano")
    let device = new LightDevice (window)
    let renderer = new LightRenderer (window, device)

    let mutable state =
        { newDefaultState () with
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
        let updateState () =
            if state.demoControls then
                state <- {state with lastFrameTime = state.upTime.Elapsed.TotalSeconds}
            else
                let deltaTime =
                    state.upTime.Elapsed.TotalSeconds - state.lastFrameTime
                    |> float32
                let newSpeed =
                    let defaultSpeed = 0.2f
                    let frac = exp (-2.5f * deltaTime)
                    frac * state.lastSpeed + (1.f - frac) * defaultSpeed * System.MathF.Pow (Voxels.octreeScale state.playerPosition renderSystem.VoxelData, 0.425f)
                let forward =
                    newSpeed * System.Numerics.Vector3.UnitZ
                    |> state.playerQuaternion.RotateVectorAsQuaternion
                let roll = Maths.Vector4.BuildQuaternion System.Numerics.Vector3.UnitZ (deltaTime * (state.keyLeftInput - state.keyRightInput))
                let pitch = Maths.Vector4.BuildQuaternion System.Numerics.Vector3.UnitX (0.75f * deltaTime * (state.keyForwardInput - state.keyBackInput))
                let deltaP = deltaTime*forward
                state <-
                    {state with
                        lastFrameTime = state.upTime.Elapsed.TotalSeconds
                        lastSpeed = newSpeed
                        playerPosition = state.playerPosition + deltaP
                        playerQuaternion =
                            pitch.QuaternionMultiply roll
                            |> state.playerQuaternion.QuaternionMultiply}
        let drawFunc () =
            updateState ()
            match renderer.BeginFrame () with
            | Some commandBuffer ->
                renderer.BeginSwapchainRenderPass commandBuffer
                renderSystem.RenderGameObjects commandBuffer state
                renderer.EndSwapchainRenderPass commandBuffer
                renderer.EndFrame ()
            | None -> ()
            window.Invalidate ()
        window.DrawFunction <- Some drawFunc
        let handleKeyDown (args: KeyEventArgs) =
            match args.KeyCode with
            | Keys.Escape -> exit 0
            | Keys.F5 ->
                renderSystem.RegenerateWorld ()
                state.upTime.Restart ()
                state <-
                    {newDefaultState () with
                        keyForwardInput = state.keyForwardInput
                        keyLeftInput = state.keyLeftInput
                        keyRightInput = state.keyRightInput
                        keyBackInput = state.keyBackInput
                        gameObjects = state.gameObjects
                        upTime = state.upTime}
            | Keys.W -> state <- {state with demoControls = false; keyForwardInput = 1.f}
            | Keys.S -> state <- {state with demoControls = false; keyBackInput = 1.f}
            | Keys.D -> state <- {state with demoControls = false; keyRightInput = 1.f}
            | Keys.A -> state <- {state with demoControls = false; keyLeftInput = 1.f}
            | Keys.F11 -> window.ToggleFullscreen ()
            | _ -> ()
        window.HandleKeyDown <- Some handleKeyDown
        let handleKeyUp (args: KeyEventArgs) =
            match args.KeyCode with
            | Keys.W -> state <- {state with demoControls = false; keyForwardInput = 0.f}
            | Keys.S -> state <- {state with demoControls = false; keyBackInput = 0.f}
            | Keys.D -> state <- {state with demoControls = false; keyRightInput = 0.f}
            | Keys.A -> state <- {state with demoControls = false; keyLeftInput = 0.f}
            | _ -> ()
        window.HandleKeyUp <- Some handleKeyUp

        System.Windows.Forms.Application.Run window
        device.Device.WaitIdle ()

    interface System.IDisposable with
        override _.Dispose () =
            if not disposed then
                disposed <- true
    override self.Finalize () = (self :> System.IDisposable).Dispose ()