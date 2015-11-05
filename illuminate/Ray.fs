module Ray

open Types
open Core

/// Get the position of a ray at a given time
let pointAtTime (ray:Ray, time:float) =
    ray.origin + (ray.direction * time)

  