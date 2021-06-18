﻿# Voxel Flight Simulator

A simple game developed for the purposes of both showcasing interesting features of sparse-voxel-octree raycasting and to utilize the Vulkan graphics API in a project.
Written in F#, I decided to use the [mono/VulkanSharp](https://github.com/mono/VulkanSharp) library to access Vulkan bindings in .NET.
The [SharpDX](https://github.com/sharpdx/SharpDX) NuGet package is taken as a dependency for Xbox controller input, but no other DirectX functionality is used.