﻿module LightApp

open Vulkan

open LightVulkanWindow
open LightDevice
open LightPipeline
open LightSwapchain
open LightModel
open LightObject

[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)>]
type PushConstantData =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val time: float32
        new (time') = {time = time'}
    end
let pushConstantSize = sizeof<PushConstantData>

type LightApp () =
    // Initialize Vulkan processes
    let window = new LightVulkanWindow (600, 400, "Volcano")
    let device = new LightDevice (window)
    let mutable swapchain = new LightSwapchain (device, window.Extent, None)
    let pipelineLayout =
        assert (device.Properties.Limits.MaxPushConstantsSize >= uint32 pushConstantSize)
        let pushConstantRange = PushConstantRange (StageFlags = ShaderStageFlags.Fragment + ShaderStageFlags.Vertex, Offset = 0u, Size = uint32 pushConstantSize)
        device.Device.CreatePipelineLayout (new PipelineLayoutCreateInfo (PushConstantRanges = [|pushConstantRange|]))
    let createPipeline (swapchain: LightSwapchain) =
        let config = {defaultPipelineConfig () with renderPass = swapchain.RenderPass; pipelineLayout = pipelineLayout}
        new LightPipeline (device, "shaders/object.vert.spv", "shaders/ray_march.frag.spv", config)
    let mutable pipeline = createPipeline swapchain
    let createCommandBuffers () =
        let info = new CommandBufferAllocateInfo (Level = CommandBufferLevel.Primary, CommandPool = device.CommandPool, CommandBufferCount = uint32 swapchain.ImageCount)
        device.Device.AllocateCommandBuffers info
    let mutable commandBuffers = createCommandBuffers ()

    let gameObjects =
        let quadObject =
            let vertices = [|
                (-1.f, -1.f)
                ( 1.f, -1.f)
                (-1.f,  1.f)
                ( 1.f,  1.f)|]
            let model = new LightModel (device, vertices)
            LightObject (Model = Some model)
        [quadObject]

    let upTime = new System.Diagnostics.Stopwatch ()
    let mutable disposed = false

    let renderGameObjects buffer =
        pipeline.Bind buffer

        for obj in gameObjects do
            match obj.Model with
            | None -> ()
            | Some model ->
                let mutable structure =
                    let time = float32 upTime.Elapsed.TotalSeconds
                    PushConstantData (time)
                buffer.CmdPushConstants (pipelineLayout, ShaderStageFlags.Fragment + ShaderStageFlags.Vertex, 0u, uint32 pushConstantSize, NativeInterop.NativePtr.toNativeInt &&structure)
                model.Bind buffer
                model.Draw buffer

    member _.RecordCommandBuffer i =
        let beginInfo = new CommandBufferBeginInfo ()
        let buff = commandBuffers.[i]
        buff.Begin beginInfo
        let renderPassInfo =
            let clearValues = [|
                new ClearValue (Color = new ClearColorValue [|0.1f; 0.1f; 0.1f; 1.f|]);
                new ClearValue (DepthStencil = ClearDepthStencilValue (Depth = 1.f, Stencil = 0u))|]
            new RenderPassBeginInfo (
                RenderPass = swapchain.RenderPass,
                Framebuffer = swapchain.GetFramebuffer i,
                RenderArea = Helpers.rectFromFourNumbers 0 0 swapchain.Extent.Width swapchain.Extent.Height,
                ClearValues = clearValues)
        buff.CmdBeginRenderPass (renderPassInfo, SubpassContents.Inline)

        let size = swapchain.Extent
        let viewport =
            Viewport (
                X = 0.f,
                Y = 0.f,
                Width = float32 size.Width,
                Height = float32 size.Height,
                MinDepth = 0.f,
                MaxDepth = 1.f)
        let scissor = Helpers.rectFromFourNumbers 0 0 size.Width size.Height
        buff.CmdSetViewport (0u, viewport)
        buff.CmdSetScissor (0u, scissor)

        renderGameObjects buff

        buff.CmdEndRenderPass ()
        buff.End ()

    member _.RecreateSwapchain () =
        let size = window.Extent
        if size.Width = 0u || size.Height = 0u then
            ()
        else
            device.Device.WaitIdle ()
            swapchain <- new LightSwapchain (device, size, Some swapchain)
            if swapchain.ImageCount <> commandBuffers.Length then
                device.Device.FreeCommandBuffers (device.CommandPool, commandBuffers)
                commandBuffers <- createCommandBuffers ()

            pipeline <- createPipeline swapchain // TODO: Only recreate pipeline if previous pipeline is not compatible with new render pass

    member self.Run () =
        let drawFunc () =
            let handleResize () =
                self.RecreateSwapchain ()
                window.Resized <- false

            if window.Resized then
                handleResize ()
            else
                try
                    let imageIndex = swapchain.AcquireNextImageAsIndex ()
                    self.RecordCommandBuffer imageIndex
                    swapchain.SubmitCommandBuffers commandBuffers.[imageIndex] imageIndex
                with
                | :? ResultException as e when (e.Result = Result.ErrorOutOfDateKhr || e.Result = Result.SuboptimalKhr) -> handleResize ()
            window.Invalidate ()
        window.DrawFunction <- Some drawFunc

        upTime.Restart ()
        System.Windows.Forms.Application.Run window
        device.Device.WaitIdle ()

    interface System.IDisposable with
        override _.Dispose () =
            if not disposed then
                disposed <- true
                device.Device.DestroyPipelineLayout pipelineLayout
    override self.Finalize () = (self :> System.IDisposable).Dispose ()