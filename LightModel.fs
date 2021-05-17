module LightModel

open System.Numerics
open System.Runtime.InteropServices
open Vulkan

open Helpers
open LightDevice

module Vertex =
    let bindingDescriptions =
        [|VertexInputBindingDescription (Binding = 0u, Stride = 2u * (uint32 sizeof<float32>), InputRate = VertexInputRate.Vertex)|]

    let attributeDescriptions =
        [|VertexInputAttributeDescription (Binding = 0u, Location = 0u, Format = Format.R32G32Sfloat, Offset = 0u)|]

type LightModel (device: LightDevice, vertices: (float32*float32)[]) =
    let vertexBuffer, vertexBufferMemory =
        let count = 2 * vertices.Length
        let buffSize = DeviceSize.op_Implicit (sizeof<float32> * count)
        let buffer, memory = device.CreateBuffer buffSize BufferUsageFlags.VertexBuffer (MemoryPropertyFlags.HostVisible + MemoryPropertyFlags.HostCoherent)
        let memPtr = device.Device.MapMemory (memory, deviceSizeZero, buffSize)
        let data = Array.init count (fun i ->
            match i % 2 with
            | 0 -> fst vertices.[i/2]
            | 1 -> snd vertices.[i/2]
            | e -> raise (System.ArithmeticException $"Modulo operator failed with imposibility %i{e}"))
        Marshal.Copy (data, 0, memPtr, data.Length)
        device.Device.UnmapMemory memory
        buffer, memory

    member _.Bind (commandBuffer: CommandBuffer) =
        commandBuffer.CmdBindVertexBuffer (0u, vertexBuffer, deviceSizeZero)

    member _.Draw (commandBuffer: CommandBuffer) =
        commandBuffer.CmdDraw (uint32 vertices.Length, 1u, 0u, 0u)

    interface System.IDisposable with
        override _.Dispose () =
            device.Device.DestroyBuffer vertexBuffer
            device.Device.FreeMemory vertexBufferMemory