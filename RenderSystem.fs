module LightRenderSystem

open Vulkan

open LightDevice
open LightPipeline
open LightObject

[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)>]
type PushConstantData =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val time: float32
        new (time') = {time = time'}
    end
let pushConstantSize = sizeof<PushConstantData>

type LightRenderSystem (device: LightDevice, initialRenderPass: RenderPass) =
    let pipelineLayout =
        assert (device.Properties.Limits.MaxPushConstantsSize >= uint32 pushConstantSize)
        let pushConstantRange = PushConstantRange (StageFlags = ShaderStageFlags.Fragment, Offset = 0u, Size = uint32 pushConstantSize)
        device.Device.CreatePipelineLayout (new PipelineLayoutCreateInfo (PushConstantRanges = [|pushConstantRange|]))

    let createPipeline renderPass =
        let config = {defaultPipelineConfig () with renderPass = renderPass; pipelineLayout = pipelineLayout}
        new LightPipeline (device, "shaders/object.vert.spv", "shaders/ray_march.frag.spv", config)
    let mutable pipeline = createPipeline initialRenderPass

    let upTime = System.Diagnostics.Stopwatch.StartNew ()    // TODO: Refactor to a GameState object?
    let mutable disposed = false

    member _.RenderGameObjects buffer (gameObjects: LightObject[]) =
        pipeline.Bind buffer
        for obj in gameObjects do
            match obj.Model with
            | None -> ()
            | Some model ->
                let mutable structure =
                    let time = float32 upTime.Elapsed.TotalSeconds
                    PushConstantData (time)
                buffer.CmdPushConstants (pipelineLayout, ShaderStageFlags.Fragment, 0u, uint32 pushConstantSize, NativeInterop.NativePtr.toNativeInt &&structure)
                model.Bind buffer
                model.Draw buffer

    interface System.IDisposable with
        override _.Dispose () =
            if not disposed then
                disposed <- true
                device.Device.DestroyPipelineLayout pipelineLayout
    override self.Finalize () = (self :> System.IDisposable).Dispose ()