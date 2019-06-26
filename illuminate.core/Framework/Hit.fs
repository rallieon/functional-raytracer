namespace Illuminate.Framework
open Illuminate.Framework.Types
open Illuminate.Framework.Math

module Hit = 
    let calculateShadowPoint ray point normal = 
        let biasNormal = multiplyVector normal bias
        addVectorToPoint point biasNormal

    let calcHitPoint origin ray tnear = 
        let evaluatedRay = {x = ray.dirX * tnear; y = ray.dirY * tnear; z = ray.dirZ * tnear}
        {x = origin.x + evaluatedRay.x; y = origin.y + evaluatedRay.y; z = origin.z + evaluatedRay.z}