module Voxels

open Maths

type RecursiveRef =
| Self
| Parent of RecursiveRef

type GraphRef<'a> =
| Ref of 'a
| Recurse of RecursiveRef
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
    flags: uint32    (*TODO: Support more flags than leaf bit*)}

let ftlCell = System.Numerics.Vector3 (-0.5f, 0.5f, -0.5f)
let ftrCell = System.Numerics.Vector3 (0.5f, 0.5f, -0.5f)
let fblCell = System.Numerics.Vector3 (-0.5f, -0.5f, -0.5f)
let fbrCell = System.Numerics.Vector3 (0.5f, -0.5f, -0.5f)
let btlCell = System.Numerics.Vector3 (-0.5f, 0.5f, 0.5f)
let btrCell = System.Numerics.Vector3 (0.5f, 0.5f, 0.5f)
let bblCell = System.Numerics.Vector3 (-0.5f, -0.5f, 0.5f)
let bbrCell = System.Numerics.Vector3 (0.5f, -0.5f, 0.5f)

let emptyLeafVoxel = {
    averageColour = Vector4.Zero
    nodeFTL = Recurse Self; nodeFTR = Recurse Self; nodeFBL = Recurse Self; nodeFBR = Recurse Self
    nodeBTL = Recurse Self; nodeBTR = Recurse Self; nodeBBL = Recurse Self; nodeBBR = Recurse Self
    flags = 1u}

[<System.Runtime.InteropServices.StructLayout (System.Runtime.InteropServices.LayoutKind.Explicit)>]
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
    | (voxel, parentList)::tail ->
        let refToIndex toExplore = function
        | Recurse Self -> selfIndex, toExplore
        | Recurse (Parent vxls) ->
            let rec func = function
            | _::indexTail, Parent vxls -> func (indexTail, vxls)
            | pIndex::_, Self -> pIndex, toExplore
            | [], _ -> 0u, toExplore    // Default to root index if we requested a parent past the root
            func (parentList, vxls)
        | Empty -> nullVoxelIndex, toExplore
        | Ref voxel ->
            if dictionary.ContainsKey voxel then
                dictionary.Item voxel, toExplore
            else
                nodeIndex <- nodeIndex + 1u
                dictionary.Item voxel <- nodeIndex
                nodeIndex, (voxel, selfIndex::parentList)::toExplore

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
    breadthFirstSearchOctree [] 0u [rootVoxel, []]
    |> List.rev
    |> List.toArray

let randomColour () = Vector4 (Helpers.randomFloat (), Helpers.randomFloat (), Helpers.randomFloat (), 1.f)

let randomLeaf () =
    let colour = randomColour ()
    {emptyLeafVoxel with averageColour = colour}

let generateRecursiveVoxelOctree n =
    let rec randomRecurse maxDepth =
        if maxDepth > 0 && Helpers.random.NextDouble () > 0.85 then
            Parent (randomRecurse (maxDepth - 1))
        else
            Self
    let maxRecurseDepth =
        log (float n) / log 2.
        |> int
    let queue = System.Collections.Generic.Queue ()
    for _ = 0 to (int n - 1) do
        randomLeaf ()
        |> queue.Enqueue
    while queue.Count > 1 do
        let rec rollVoxel () =
            let mutable popped = false
            let mutable colour = Vector4 (0.f)
            let mutable weight = 0.f
            let popNodeOption () =
                let randomType = Helpers.random.NextDouble ()
                if queue.Count = 0 then
                    if randomType > 0.69 then
                        Recurse (randomRecurse maxRecurseDepth)
                    else
                        Empty
                else
                    if randomType > 0.45 then
                        if randomType > 0.825 then
                            Recurse (randomRecurse maxRecurseDepth)
                        else
                            let voxel = queue.Dequeue ()
                            weight <- weight + 1.f
                            colour.AddInPlace voxel.averageColour
                            popped <- true
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
            if popped then
                {
                    averageColour = if weight = 0.f then randomColour () else colour / weight
                    nodeFTL = ftl; nodeFTR = ftr
                    nodeFBL = fbl; nodeFBR = fbr
                    nodeBTL = btl; nodeBTR = btr
                    nodeBBL = bbl; nodeBBR = bbr
                    flags = 0u
                }
            else
                rollVoxel ()
        rollVoxel ()
        |> queue.Enqueue
    queue.Dequeue ()
    |> compactOctreeFromRoot

(*let octreeDistance (v: System.Numerics.Vector3) (octree: VoxelCompact[]) =
    let rec f p minDist = function
    | [] ->
        match minDist with
        | Some dist -> dist
        | None -> raise (System.Exception "No distance found")
    | (index, scale)::tail ->
        let voxel = octree.[index]
        let expandSearch bfs = function
        | childIndex when childIndex = nullVoxelIndex -> bfs
        | childIndex -> 
    f v None [0u, 1.f]*)

let octreeScaleAndCollisionOfPoint (v: System.Numerics.Vector3) (octree: VoxelCompact[]) =
    let maxDepth = 12   // TODO: Unify maxDepth between shader and cpu programatically
    let rec f (scale: float32) iter (p: System.Numerics.Vector3) = function
    | _ when iter = maxDepth -> scale, true
    | index when index = nullVoxelIndex -> scale, false
    | index ->
        let i = int index
        let voxel = octree.[i]
        if voxel.flags &&& 1u > 0u then
            scale, true
        else
            let func p =
                f (scale + scale) (iter + 1) (p+p)
            if p.X > 0.f then
                if p.Y > 0.f then
                    if p.Z > 0.f then
                        func (p - btrCell) voxel.nodeBTR
                    else
                        func (p - ftrCell) voxel.nodeFTR
                else
                    if p.Z > 0.f then
                        func (p - bbrCell) voxel.nodeBBR
                    else
                        func (p - fbrCell) voxel.nodeFBR
            else
                if p.Y > 0.f then
                    if p.Z > 0.f then
                        func (p - btlCell) voxel.nodeBTL
                    else
                        func (p - ftlCell) voxel.nodeFTL
                else
                    if p.Z > 0.f then
                        func (p - bblCell) voxel.nodeBBL
                    else
                        func (p - fblCell) voxel.nodeFBL
    if abs v.X > 1.f || abs v.Y > 1.f || abs v.Z > 1.f then
        1.f, false
    else
        let scaleInv, b = f 1.f 0 v 0u
        1.f / scaleInv, b