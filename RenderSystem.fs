module LightRenderSystem

open Vulkan

open LightDevice
open LightPipeline
open LightState
open Voxels

[<System.Runtime.InteropServices.StructLayout (System.Runtime.InteropServices.LayoutKind.Explicit)>]
type PushConstantData =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val cameraPosition: System.Numerics.Vector3
        [<System.Runtime.InteropServices.FieldOffset 12>]
        val time: float32
        [<System.Runtime.InteropServices.FieldOffset 16>]
        val cameraQuaternion: Maths.Vector4
        [<System.Runtime.InteropServices.FieldOffset 32>]
        val lightDir: System.Numerics.Vector3
        [<System.Runtime.InteropServices.FieldOffset 44>]
        val aspectRatio: float32
        new (time', aspectRatio') = {
            cameraPosition = demoCameraPosition time'
            time = time'
            cameraQuaternion = demoCameraQuaternion time'
            lightDir = lightDir time'
            aspectRatio = aspectRatio'}
        new (cameraPosition', cameraQuaternion', time', aspectRatio') = {
            cameraPosition = cameraPosition'
            time = time'
            cameraQuaternion = cameraQuaternion'
            lightDir = lightDir time'
            aspectRatio = aspectRatio'}
    end
let pushConstantSize = sizeof<PushConstantData>

let recursiveVoxelLeafCount = 2048
let uniqueGoalCount = 3
let minGoalDepth = 6
let createNewVoxelWorld random =
    let voxels = generateRecursiveVoxelOctree random recursiveVoxelLeafCount
    addRandomGoals random uniqueGoalCount minGoalDepth voxels
    voxels

type LightRenderSystem (device: LightDevice, initialRenderPass: RenderPass) =
     // Create Shader Storage Buffer to contain a sparse-voxel-octree
    let mutable voxelData = createNewVoxelWorld (System.Random 0)
    let createVoxelBufferMemory (voxelData: VoxelCompact[]) =
        let voxelBufferDeviceSize = DeviceSize.op_Implicit (sizeof<VoxelCompact> * voxelData.Length)
        let transferToPtr memPtr = Helpers.MarshalArrayOfStruct voxelData memPtr
        let storageBuffer, memory =
            device.CreateLocalBufferWithTransfer voxelBufferDeviceSize BufferUsageFlags.StorageBuffer transferToPtr
        storageBuffer, memory, voxelBufferDeviceSize
    let mutable voxelBuffer, voxelBufferMemory, voxelBufferDeviceSize = createVoxelBufferMemory voxelData

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

    let updateDescriptorSets () =
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
    do updateDescriptorSets ()

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

    let mutable disposed = false

    member _.VoxelData = voxelData

    member _.RegenerateWorld random =
        device.Device.WaitIdle ()   // Cannot delete buffer while device is using it ;)
        device.Device.DestroyBuffer voxelBuffer
        device.Device.FreeMemory voxelBufferMemory

        voxelData <- createNewVoxelWorld random
        match createVoxelBufferMemory voxelData with
        | buffer, memory, deviceSize ->
            voxelBuffer <- buffer
            voxelBufferMemory <- memory
            voxelBufferDeviceSize <- deviceSize
        updateDescriptorSets ()

    member _.RenderGameObjects buffer (state: LightState) aspectRatio =
        pipeline.Bind buffer
        buffer.CmdBindDescriptorSet (PipelineBindPoint.Graphics, pipelineLayout, 0u, descriptorSet, System.Nullable ())
        for obj in state.gameObjects do
            match obj.Model with
            | None -> ()
            | Some model ->
                let mutable structure =
                    let time = float32 state.upTime.Elapsed.TotalSeconds
                    if state.demoControls then
                        PushConstantData (time, aspectRatio)
                    else
                        PushConstantData (state.playerPosition, state.playerQuaternion, time, aspectRatio)
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