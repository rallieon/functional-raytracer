module Core

open Types

/// Get the unit vector of a given vector
let norm (v:Vector3D) = 
    let abs = sqrt (v.x * v.x + v.y * v.y + v.z * v.z)
    v / abs

