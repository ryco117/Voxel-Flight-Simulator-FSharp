module LightRenderSystem

open Vulkan

open LightDevice
open LightPipeline
open LightObject
open Voxels

[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)>]
type PushConstantData =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val time: float32
        new (time') = {time = time'}
    end
let pushConstantSize = sizeof<PushConstantData>

type LightRenderSystem (device: LightDevice, initialRenderPass: RenderPass) =

    // Create Shader Storage Buffer to contain a sparse-voxel-octree
    let voxelData = generateSparseVoxelOctree 200_000
    let voxelBufferDeviceSize = DeviceSize.op_Implicit (sizeof<VoxelCompact> * voxelData.Length)
    let voxelBuffer, voxelBufferMemory =
        let buffer, memory = device.CreateBuffer voxelBufferDeviceSize BufferUsageFlags.StorageBuffer (MemoryPropertyFlags.HostVisible + MemoryPropertyFlags.HostCoherent)
        let memPtr = device.Device.MapMemory (memory, Helpers.deviceSizeZero, voxelBufferDeviceSize)
        Helpers.MarshalArrayOfStruct voxelData memPtr
        device.Device.UnmapMemory memory
        buffer, memory

    let descriptorPool =
        use poolInfo =
            (*TODO: Allow more than one descriptor set*)
            new DescriptorPoolCreateInfo (
                PoolSizes = [|DescriptorPoolSize (Type = DescriptorType.StorageBuffer, DescriptorCount = 1u)|],
                MaxSets = 1u)
        device.Device.CreateDescriptorPool poolInfo

    let descriptorSetLayout =
        use voxelOctreeBinding =
            new DescriptorSetLayoutBinding (
                Binding = 0u,
                DescriptorCount = 1u,
                DescriptorType = DescriptorType.StorageBuffer,
                StageFlags = ShaderStageFlags.Fragment)
        use setLayoutInfo =
            new DescriptorSetLayoutCreateInfo (
                BindingCount = 1u,
                Bindings = [|voxelOctreeBinding|])
        device.Device.CreateDescriptorSetLayout setLayoutInfo

    let descriptorSet =
        use allocInfo =
            new DescriptorSetAllocateInfo (
                DescriptorPool = descriptorPool,
                DescriptorSetCount = 1u,
                SetLayouts = [|descriptorSetLayout|])
        match device.Device.AllocateDescriptorSets allocInfo with
        | [|descriptorSet|] -> descriptorSet
        | _ -> raise (System.Exception "Exactly one descriptor set is expected to be created")

    do (*Update Descriptor Sets*)
        use buffInfo =
            new DescriptorBufferInfo (
                Buffer = voxelBuffer,
                Offset = DeviceSize.op_Implicit 0,
                Range = voxelBufferDeviceSize)
        use setWrite =
            new WriteDescriptorSet (
                DstBinding = 0u,
                DstSet = descriptorSet,
                DescriptorType = DescriptorType.StorageBuffer,
                BufferInfo = [|buffInfo|])
        device.Device.UpdateDescriptorSets ([|setWrite|], null)

    let pipelineLayout =
        assert (device.Properties.Limits.MaxPushConstantsSize >= uint32 pushConstantSize)
        let pushConstantRange = PushConstantRange (StageFlags = ShaderStageFlags.Fragment, Offset = 0u, Size = uint32 pushConstantSize)
        let pipelineCreateInfo =
            new PipelineLayoutCreateInfo (
                PushConstantRanges = [|pushConstantRange|],
                SetLayouts = [|descriptorSetLayout|])
        device.Device.CreatePipelineLayout pipelineCreateInfo

    let createPipeline renderPass =
        let config = {defaultPipelineConfig () with renderPass = renderPass; pipelineLayout = pipelineLayout}
        new LightPipeline (device, "shaders/object.vert.spv", "shaders/ray_march_voxels.frag.spv", config)
    let mutable pipeline = createPipeline initialRenderPass

    let upTime = System.Diagnostics.Stopwatch.StartNew ()    // TODO: Refactor to a GameState object?
    let mutable disposed = false

    member _.RenderGameObjects buffer (gameObjects: LightObject[]) =
        pipeline.Bind buffer
        buffer.CmdBindDescriptorSet (PipelineBindPoint.Graphics, pipelineLayout, 0u, descriptorSet, System.Nullable ())
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
                device.Device.DestroyBuffer voxelBuffer
                device.Device.FreeMemory voxelBufferMemory
                device.Device.DestroyDescriptorPool descriptorPool
                device.Device.DestroyPipelineLayout pipelineLayout
    override self.Finalize () = (self :> System.IDisposable).Dispose ()