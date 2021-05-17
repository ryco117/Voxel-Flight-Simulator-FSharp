module LightApp

open Vulkan

open LightVulkanWindow
open LightDevice
open LightPipeline
open LightSwapchain
open LightModel

type PushConstantData = {time: float32}

type LightApp () =
    // Initialize Vulkan processes
    let window = new LightVulkanWindow (600, 400, "Volcano")
    let device = new LightDevice (window)
    let mutable swapchain = new LightSwapchain (device, window.Extent, None)
    let pipelineLayout =
        let pushConstantRange = PushConstantRange (StageFlags = ShaderStageFlags.Fragment, Offset = 0u, Size = uint32 sizeof<PushConstantData>)
        device.Device.CreatePipelineLayout (new PipelineLayoutCreateInfo (PushConstantRanges = [|pushConstantRange|]))
    let createPipeline (swapchain: LightSwapchain) =
        let config = {defaultPipelineConfig () with renderPass = swapchain.RenderPass; pipelineLayout = pipelineLayout}
        new LightPipeline (device, "shaders/object.vert.spv", "shaders/ray_march.frag.spv", config)
    let mutable pipeline = createPipeline swapchain
    let createCommandBuffers () =
        let info = new CommandBufferAllocateInfo (Level = CommandBufferLevel.Primary, CommandPool = device.CommandPool, CommandBufferCount = uint32 swapchain.ImageCount)
        device.Device.AllocateCommandBuffers info
    let mutable commandBuffers = createCommandBuffers ()

    let quadObject =
        let vertices = [|
            (-1.f, -1.f)
            ( 1.f, -1.f)
            (-1.f,  1.f)
            ( 1.f,  1.f)|]
        new LightModel (device, vertices)

    let upTime = new System.Diagnostics.Stopwatch ()

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

        pipeline.Bind buff
        // TODO: Dynamically change vertex count appropriately
        //buff.CmdDraw (4u, 1u, 0u, 0u)
        quadObject.Bind buff

        // PushConstant Test
        let push =
            let mutable time = float32 upTime.Elapsed.TotalSeconds
            let ptr = NativeInterop.NativePtr.toNativeInt &&time
            //System.Runtime.InteropServices.Marsh (structure, ptr, false)
            ptr
        buff.CmdPushConstants (pipelineLayout, ShaderStageFlags.Fragment, 0u, uint32 sizeof<PushConstantData>, push)
        //System.Runtime.InteropServices.Marshal.FreeHGlobal push

        quadObject.Draw buff

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

    override _.Finalize () =
        device.Device.DestroyPipelineLayout pipelineLayout
        (quadObject :> System.IDisposable).Dispose ()