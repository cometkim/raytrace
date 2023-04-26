open Belt
open Js.Math
open Webapi.Dom

@send external getContext: (Element.t, string) => {..} = "getContext"
@get external body: Document.t => Element.t = "body"

module Vector = {
  type t = {
    x: float,
    y: float,
    z: float,
  }

  let make = (~x, ~y, ~z) => {x, y, z}

  let times = (v, k) => {
    x: v.x *. k,
    y: v.y *. k,
    z: v.z *. k,
  }

  let minus = (v1, v2) => {
    x: v1.x -. v2.x,
    y: v1.y -. v2.y,
    z: v1.z -. v2.z,
  }

  let plus = (v1, v2) => {
    x: v1.x +. v2.x,
    y: v1.y +. v2.y,
    z: v1.z +. v2.z,
  }

  let dot = (v1, v2) => {
    v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z
  }

  let mag = ({x, y, z}) => sqrt(x *. x +. y *. y +. z *. z)

  let norm = v => {
    let mag = v->mag
    let div = mag == 0.0 ? infinity : 1.0 /. mag
    v->times(div)
  }

  let cross = (v1, v2) => {
    x: v1.y *. v2.z -. v1.z *. v2.y,
    y: v1.z *. v2.x -. v1.x *. v2.z,
    z: v1.x *. v2.y -. v1.y *. v2.x,
  }
}

module Color = {
  type t = {
    r: float,
    g: float,
    b: float,
  }

  let make = (~r, ~g, ~b) => {
    r,
    g,
    b,
  }

  let scale = (v, k) => {
    r: v.r *. k,
    g: v.g *. k,
    b: v.b *. k,
  }

  let plus = (v1, v2) => {
    r: v1.r +. v2.r,
    g: v1.g +. v2.g,
    b: v1.b +. v2.b,
  }

  let times = (v1, v2) => {
    r: v1.r *. v2.r,
    g: v1.g *. v2.g,
    b: v1.b *. v2.b,
  }

  let white = {
    r: 1.0,
    g: 1.0,
    b: 1.0,
  }

  let grey = {
    r: 0.5,
    g: 0.5,
    b: 0.5,
  }

  let black = {
    r: 0.0,
    g: 0.0,
    b: 0.0,
  }

  let background = black

  let defaultColor = black

  let toDrawingColor = c => {
    {
      r: floor_float(min_float(c.r, 1.0) *. 255.0),
      g: floor_float(min_float(c.g, 1.0) *. 255.0),
      b: floor_float(min_float(c.b, 1.0) *. 255.0),
    }
  }
}

module Camera = {
  type t = {
    pos: Vector.t,
    forward: Vector.t,
    right: Vector.t,
    up: Vector.t,
  }

  let make = (~pos, ~lookAt) => {
    let down = Vector.make(~x=0.0, ~y=-1.0, ~z=0.0)
    let forward = lookAt->Vector.minus(pos)->Vector.norm
    let right = forward->Vector.cross(down)->Vector.norm->Vector.times(1.5)
    let up = forward->Vector.cross(right)->Vector.norm->Vector.times(1.5)
    {pos, forward, right, up}
  }
}

type ray = {
  start: Vector.t,
  dir: Vector.t,
}

type light = {
  pos: Vector.t,
  color: Color.t,
}

module Surface = {
  type t = {
    diffuse: (. Vector.t) => Color.t,
    specular: (. Vector.t) => Color.t,
    reflect: (. Vector.t) => float,
    roughness: float,
  }

  let shiny = {
    diffuse: (. _pos) => Color.white,
    specular: (. _pos) => Color.grey,
    reflect: (. _pos) => 0.7,
    roughness: 250.0,
  }

  let checkerboard = {
    diffuse: (. {x, z}) => {
      switch mod(Float.toInt(z->floor_float) + Float.toInt(x->floor_float), 2) {
      | 0 => Color.black
      | _ => Color.white
      }
    },
    specular: (. _pos) => Color.white,
    reflect: (. {x, z}) => {
      switch mod(Float.toInt(z->floor_float) + Float.toInt(x->floor_float), 2) {
      | 0 => 0.7
      | _ => 0.1
      }
    },
    roughness: 150.0,
  }
}

module Thing = {
  type t =
    | Sphere({center: Vector.t, radius2: float, surface: Surface.t})
    | Plane({normal: Vector.t, offset: float, surface: Surface.t})

  type intersection = {
    thing: t,
    ray: ray,
    dist: float,
  }

  let makeSphere = (~center, ~radius, ~surface) => {
    Sphere({center, radius2: radius *. radius, surface})
  }

  let makePlane = (~normal, ~offset, ~surface) => {
    Plane({normal, offset, surface})
  }

  let surface = thing => {
    let Sphere({surface}) | Plane({surface}) = thing
    surface
  }

  let normal = (thing, ~pos) =>
    switch thing {
    | Sphere({center}) => pos->Vector.minus(center)->Vector.norm
    | Plane({normal}) => normal
    }

  let intersect = (thing, ~ray) => {
    switch thing {
    | Sphere({center, radius2}) => {
        let eo = center->Vector.minus(ray.start)
        switch eo->Vector.dot(ray.dir) {
        | v if v >= 0.0 =>
          switch radius2 -. (Vector.dot(eo, eo) -. v *. v) {
          | disc if disc >= 0.0 => Some({thing, ray, dist: v -. sqrt(disc)})
          | _ => None
          }
        | _ => None
        }
      }
    | Plane({normal, offset}) =>
      switch normal->Vector.dot(ray.dir) {
      | denom if denom > 0.0 => None
      | denom =>
        Some({
          thing,
          ray,
          dist: (normal->Vector.dot(ray.start) +. offset) /. -.denom,
        })
      }
    }
  }
}

module Scene = {
  type t = {
    things: array<Thing.t>,
    lights: array<light>,
    camera: Camera.t,
  }

  let default = {
    things: [
      Thing.makePlane(
        ~normal=Vector.make(~x=0.0, ~y=1.0, ~z=0.0),
        ~offset=0.0,
        ~surface=Surface.checkerboard,
      ),
      Thing.makeSphere(
        ~center=Vector.make(~x=0.0, ~y=1.0, ~z=-0.25),
        ~radius=1.0,
        ~surface=Surface.shiny,
      ),
      Thing.makeSphere(
        ~center=Vector.make(~x=-1.0, ~y=0.5, ~z=1.5),
        ~radius=0.5,
        ~surface=Surface.shiny,
      ),
    ],
    lights: [
      {
        pos: Vector.make(~x=-2.0, ~y=2.5, ~z=0.0),
        color: Color.make(~r=0.49, ~g=0.07, ~b=0.07),
      },
      {
        pos: Vector.make(~x=1.5, ~y=2.5, ~z=1.5),
        color: Color.make(~r=0.07, ~g=0.07, ~b=0.49),
      },
      {
        pos: Vector.make(~x=1.5, ~y=2.5, ~z=-1.5),
        color: Color.make(~r=0.07, ~g=0.49, ~b=0.071),
      },
      {
        pos: Vector.make(~x=0.0, ~y=3.5, ~z=0.0),
        color: Color.make(~r=0.21, ~g=0.21, ~b=0.35),
      },
    ],
    camera: Camera.make(
      ~pos=Vector.make(~x=3.0, ~y=2.0, ~z=4.0),
      ~lookAt=Vector.make(~x=-1.0, ~y=0.5, ~z=0.0),
    ),
  }
}

module RayTracer = {
  let maxDepth = 5

  let intersections = (~ray, ~scene) => {
    let closest = ref(infinity)
    let closestInter = ref(None)
    let len = scene.Scene.things->Array.length
    for i in 0 to len - 1 {
      let thing = scene.things->Array.getUnsafe(i)
      switch thing->Thing.intersect(~ray) {
      | Some(inter) if inter.dist < closest.contents => {
          closestInter := Some(inter)
          closest := inter.dist
        }
      | _ => ()
      }
    }
    closestInter.contents
  }

  let testRay = (~ray, ~scene) => {
    switch intersections(~ray, ~scene) {
    | Some({dist}) => Some(dist)
    | None => None
    }
  }

  let rec traceRay = (~ray, ~scene, ~depth) => {
    switch intersections(~ray, ~scene) {
    | Some(intersection) => shade(~intersection, ~scene, ~depth)
    | None => Color.background
    }
  }
  and getReflectionColor = (~thing, ~pos, ~rd, ~scene, ~depth) => {
    let surface = thing->Thing.surface
    traceRay(~ray={start: pos, dir: rd}, ~scene, ~depth=depth + 1)->Color.scale(
      surface.reflect(. pos),
    )
  }
  and getNaturalColor = (~thing, ~pos, ~normal, ~rd, ~scene) => {
    let surface = thing->Thing.surface
    scene.Scene.lights->Belt.Array.reduce(Color.defaultColor, (color, light) => {
      let ldis = light.pos->Vector.minus(pos)
      let livec = Vector.norm(ldis)
      switch testRay(~ray={start: pos, dir: livec}, ~scene) {
      | Some(neatIsect) if neatIsect <= Vector.mag(ldis) => color
      | _ => {
          let illum = livec->Vector.dot(normal)
          let lcolor = illum > 0.0 ? light.color->Color.scale(illum) : Color.defaultColor
          let specular = livec->Vector.dot(Vector.norm(rd))
          let scolor =
            specular > 0.0
              ? light.color->Color.scale(pow_float(~base=specular, ~exp=surface.roughness))
              : Color.defaultColor
          color->Color.plus(
            Color.plus(
              lcolor->Color.times(surface.diffuse(. pos)),
              scolor->Color.times(surface.specular(. pos)),
            ),
          )
        }
      }
    })
  }
  and shade = (~intersection, ~scene, ~depth) => {
    open Thing

    let {thing, dist, ray: {start, dir}} = intersection
    let pos = dir->Vector.times(dist)->Vector.plus(start)
    let normal = thing->Thing.normal(~pos)
    let target = normal->Vector.times(normal->Vector.dot(dir))->Vector.times(2.0)
    let rd = dir->Vector.minus(target)
    let naturalColor =
      Color.background->Color.plus(getNaturalColor(~thing, ~pos, ~normal, ~rd, ~scene))
    let reflectedColor =
      depth >= maxDepth ? Color.grey : getReflectionColor(~thing, ~pos, ~rd, ~scene, ~depth)
    Color.plus(naturalColor, reflectedColor)
  }

  let render = (~ctx, ~scene, ~screenWidth, ~screenHeight) => {
    let recenterX = x => {
      let x = x->Int.toFloat
      let screenWidth = screenWidth->Int.toFloat
      (x -. screenWidth /. 2.0) /. 2.0 /. screenWidth
    }
    let recenterY = y => {
      let y = y->Int.toFloat
      let screenHeight = screenHeight->Int.toFloat
      -.(y -. screenHeight /. 2.0) /. 2.0 /. screenHeight
    }
    let getPoint = (x, y, camera) =>
      camera.Camera.forward
      ->Vector.plus(
        Vector.plus(
          camera.right->Vector.times(recenterX(x)),
          camera.up->Vector.times(recenterY(y)),
        ),
      )
      ->Vector.norm

    let {Scene.camera: camera} = scene

    for y in 0 to screenHeight - 1 {
      for x in 0 to screenWidth - 1 {
        let {r, g, b} =
          traceRay(
            ~ray={start: camera.pos, dir: getPoint(x, y, camera)},
            ~scene,
            ~depth=0,
          )->Color.toDrawingColor

        ctx["fillStyle"] = `rgb(${r->Float.toString}, ${g->Float.toString}, ${b->Float.toString})`
        ctx["fillRect"](. x, y, 1, 1)
      }
    }
  }
}

let exec = (~width, ~height) => {
  let canv = document->Document.createElement("canvas")
  canv->Element.setAttribute("width", width->Int.toString)
  canv->Element.setAttribute("height", height->Int.toString)

  RayTracer.render(
    ~ctx=canv->getContext("2d"),
    ~scene=Scene.default,
    ~screenWidth=width,
    ~screenHeight=height,
  )

  document->body->Element.appendChild(~child=canv)
}

let size = 256
let render = () =>
  Js.Promise2.make((~resolve, ~reject as _) => {
    Webapi.requestAnimationFrame(_ => {
      Js.Console.timeStart("raytrace ReScript")
      exec(~width=size, ~height=size)
      Js.Console.timeEnd("raytrace ReScript")
      resolve(. ())
    })
  })

open Js.Promise2
render()
->then(render)
->then(render)
->then(render)
->then(render)
->then(render)
->then(_ => {
  document->Document.documentElement->Element.classList->DomTokenList.add("done")
  resolve()
})
->ignore
