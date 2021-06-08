module Voxels

open Maths

type GraphRef<'a> =
| Ref of 'a
| Self
| Empty

type Voxel = {
    averageColour: Vector4
    nodeFTL: GraphRef<Voxel> // Front-Top-Left
    nodeFTR: GraphRef<Voxel> // Front-Top-Right
    nodeFBL: GraphRef<Voxel> // Front-Bottom-Left
    nodeFBR: GraphRef<Voxel> // Front-Bottom-Right
    nodeBTL: GraphRef<Voxel> // Back-Top-Left
    nodeBTR: GraphRef<Voxel> // Back-Top-Right
    nodeBBL: GraphRef<Voxel> // Back-Bottom-Left
    nodeBBR: GraphRef<Voxel> // Back-Bottom-Right
    flags: uint32    // TODO: Support more flags than leaf bit
}

let emptyLeafVoxel = {
    averageColour = Vector4.Zero
    nodeFTL = Self; nodeFTR = Self; nodeFBL = Self; nodeFBR = Self
    nodeBTL = Self; nodeBTR = Self; nodeBBL = Self; nodeBBR = Self
    flags = 1u}

[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)>]
type VoxelCompact =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val averageColour: Vector4
        [<System.Runtime.InteropServices.FieldOffset 16>]
        val nodeFTL: uint32 // Front-Top-Left
        [<System.Runtime.InteropServices.FieldOffset 20>]
        val nodeFTR: uint32 // Front-Top-Right
        [<System.Runtime.InteropServices.FieldOffset 24>]
        val nodeFBL: uint32 // Front-Bottom-Left
        [<System.Runtime.InteropServices.FieldOffset 28>]
        val nodeFBR: uint32 // Front-Bottom-Right
        [<System.Runtime.InteropServices.FieldOffset 32>]
        val nodeBTL: uint32 // Back-Top-Left
        [<System.Runtime.InteropServices.FieldOffset 36>]
        val nodeBTR: uint32 // Back-Top-Right
        [<System.Runtime.InteropServices.FieldOffset 40>]
        val nodeBBL: uint32 // Back-Bottom-Left
        [<System.Runtime.InteropServices.FieldOffset 44>]
        val nodeBBR: uint32 // Back-Bottom-Right
        [<System.Runtime.InteropServices.FieldOffset 48>]
        val flags: uint32 // 32-bit mask
        [<System.Runtime.InteropServices.FieldOffset 52>]
        val alignmentSpace0: uint32 // EMPTY SPACE IN BUFFER - ALIGNMENT
        [<System.Runtime.InteropServices.FieldOffset 56>]
        val alignmentSpace1: uint64 // EMPTY SPACE IN BUFFER - ALIGNMENT
        new (colour', nodeFTL', nodeFTR', nodeFBL', nodeFBR', nodeBTL', nodeBTR', nodeBBL', nodeBBR', flags') =
            {
                averageColour = colour'
                nodeFTL = nodeFTL'
                nodeFTR = nodeFTR'
                nodeFBL = nodeFBL'
                nodeFBR = nodeFBR'
                nodeBTL = nodeBTL'
                nodeBTR = nodeBTR'
                nodeBBL = nodeBBL'
                nodeBBR = nodeBBR'
                flags = flags'
                alignmentSpace0 = 0u
                alignmentSpace1 = 0UL}
    end

let nullVoxelIndex: uint32 = 0xFFFFFFFFu

let compactOctreeFromRoot rootVoxel =
    let mutable nodeIndex = 0u
    let dictionary = System.Collections.Generic.Dictionary<Voxel, uint32> ()
    let rec breadthFirstSearchOctree acc selfIndex = function
    | [] -> acc
    | voxel::tail ->
        let refToIndex toExplore = function
        | Self -> selfIndex, toExplore
        | Empty -> nullVoxelIndex, toExplore
        | Ref voxel ->
            if dictionary.ContainsKey voxel then
                dictionary.Item voxel, toExplore
            else
                nodeIndex <- nodeIndex + 1u
                dictionary.Item voxel <- nodeIndex
                nodeIndex, voxel::toExplore

        let colour = voxel.averageColour
        let ftl, bfs = refToIndex [] voxel.nodeFTL
        let ftr, bfs = refToIndex bfs voxel.nodeFTR
        let fbl, bfs = refToIndex bfs voxel.nodeFBL
        let fbr, bfs = refToIndex bfs voxel.nodeFBR
        let btl, bfs = refToIndex bfs voxel.nodeBTL
        let btr, bfs = refToIndex bfs voxel.nodeBTR
        let bbl, bfs = refToIndex bfs voxel.nodeBBL
        let bbr, bfs = refToIndex bfs voxel.nodeBBR
        let compactVoxel = VoxelCompact (colour, ftl, ftr, fbl, fbr, btl, btr, bbl, bbr, voxel.flags)
        breadthFirstSearchOctree (compactVoxel::acc) (selfIndex + 1u) (List.append (List.rev bfs) tail)
    breadthFirstSearchOctree [] 0u [rootVoxel]
    |> List.rev
    |> List.toArray

let generateSparseVoxelOctree n =
    let randomColour () = Vector4 (Helpers.randomFloat (), Helpers.randomFloat (), Helpers.randomFloat (), 1.f)
    let randomLeaf () =
        let colour = randomColour ()
        {emptyLeafVoxel with averageColour = colour}
    let queue = System.Collections.Generic.Queue ()
    for _ = 0 to (int n - 1) do
        randomLeaf ()
        |> queue.Enqueue
    while queue.Count > 1 do
        let mutable colour = Vector4 (0.f)
        let mutable weight = 0.f
        let popNodeOption () =
            let randomType = Helpers.random.NextDouble ()
            if queue.Count = 0 then
                if randomType > 0.75 then
                    Self
                else
                    Empty
            else
                if randomType > 0.4 then
                    if randomType > 0.75 then
                        Self
                    else
                        let voxel = queue.Dequeue ()
                        weight <- weight + 1.f
                        colour.AddInPlace voxel.averageColour
                        Ref voxel
                else
                    Empty

        let ftl = popNodeOption ()
        let ftr = popNodeOption ()
        let fbl = popNodeOption ()
        let fbr = popNodeOption ()
        let btl = popNodeOption ()
        let btr = popNodeOption ()
        let bbl = popNodeOption ()
        let bbr = popNodeOption ()
        {
            averageColour = if weight = 0.f then randomColour () else colour / weight
            nodeFTL = ftl; nodeFTR = ftr
            nodeFBL = fbl; nodeFBR = fbr
            nodeBTL = btl; nodeBTR = btr
            nodeBBL = bbl; nodeBBR = bbr
            flags = 0u
        }
        |> queue.Enqueue
    queue.Dequeue ()
    |> compactOctreeFromRoot

let octreeDistance (v: System.Numerics.Vector3) (octree: VoxelCompact[]) =
    let rec f p minDist = function
    | [] -> minDist
    | index::tail -> minDist
    f v (max (abs v.X) (max (abs v.Y) (abs v.Z))) []