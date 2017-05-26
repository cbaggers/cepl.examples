(in-package :cepl.examples+camera)

;;- - - - - - - - - - - - - - - - - -

(defparameter sphere-stream nil)
(defparameter brick nil)

(defstruct sphere
  (pos (v! 0 0 -10))
  (rot (q:identity)))

(defparameter sphere-a (make-sphere :pos (v! 0 0 -1.5)))

;;- - - - - - - - - - - - - - - - - -

(defun model->world (x)
  (m4:* (m4:translation (sphere-pos x)) (q:to-mat4 (sphere-rot x))))

(defun world->clip (c)
  (m4:* (cam->clip c) (world->cam c)))

(defun model->clip (m c)
  (m4:* (world->clip c) (model->world m)))

;;- - - - - - - - - - - - - - - - - -

(defparameter bp (make-blending-params))
(defparameter camera (make-camera))
(defparameter factor 0)

;;- - - - - - - - - - - - - - - - - -

;; Inline glsl for vertex shaders is not currently supported
(defun-g sphere-vert ((vert g-pnt) &uniform (model->clip :mat4))
  (values (* model->clip (v! (pos vert) 1))
          (pos vert)))

(def-glsl-stage sphere-tess-con (("position" (:vec3 3))
                                  &context :450 :tessellation-control
                                  (:patch 3))
  "
   layout(vertices = 3) out;

   void main()
   {
       float TessLevelInner = 5f;
       float TessLevelOuter = 5f;

       v_out[gl_InvocationID].pos = v_in[gl_InvocationID].position;
       if (gl_InvocationID == 0) {
           gl_TessLevelInner[0] = TessLevelInner;
           gl_TessLevelOuter[0] = TessLevelOuter;
           gl_TessLevelOuter[1] = TessLevelOuter;
           gl_TessLevelOuter[2] = TessLevelOuter;
       }
   }
  "
  (("pos" :vec3)))

(def-glsl-stage sphere-tess-eval (("position" (:vec3 3))
                                  &uniform ("model_to_clip" :mat4)
                                  &context :450 :tessellation-evaluation
                                  (:patch 3))
  "
   layout(triangles, equal_spacing, ccw) in;

   void main()
   {
       vec3 p0 = gl_TessCoord.x * v_in[0].position;
       vec3 p1 = gl_TessCoord.y * v_in[1].position;
       vec3 p2 = gl_TessCoord.z * v_in[2].position;
       vec3 pos = normalize(p0 + p1 + p2);

       gl_Position = model_to_clip * vec4(pos, 1);
       v_out.pos = pos;
       v_out.patch_distance = gl_TessCoord;
   }
  "
  (("pos" :vec3) ("patch_distance" :vec3)))

(def-glsl-stage sphere-geom (("position" (:vec3 3))
                             ("patch_distance" (:vec3 3))
                             &uniform ("normal_mat" :mat3)
                             &context :450 :geometry)
  "layout(triangle_strip, max_vertices = 3) out;

   void main()
   {
       vec3 A = v_in[2].position - v_in[0].position;
       vec3 B = v_in[1].position - v_in[0].position;
       vec3 f_norm = normal_mat * normalize(cross(A, B));

       gl_Position = gl_in[0].gl_Position;
       v_out.patch_dist = v_in[0].patch_distance;
       v_out.tri_dist = vec3(1, 0, 0);
       v_out.facet_norm = f_norm;
       EmitVertex();

       gl_Position = gl_in[1].gl_Position;
       v_out.patch_dist = v_in[1].patch_distance;
       v_out.tri_dist = vec3(0, 1, 0);
       v_out.facet_norm = f_norm;
       EmitVertex();

       gl_Position = gl_in[2].gl_Position;
       v_out.patch_dist = v_in[2].patch_distance;
       v_out.tri_dist = vec3(0, 0, 1);
       v_out.facet_norm = f_norm;
       EmitVertex();

       EndPrimitive();
   }
  "
  (("patch_dist" :vec3) ("facet_norm" :vec3) ("tri_dist" :vec3)))

(def-glsl-stage sphere-frag (("patch_distance" :vec3) ("facet_normal" :vec3)
                             ("tri_distance" :vec3)
                             &context :450 :fragment)
  "
   float amplify(float d, float scale, float offset)
   {
       d = scale * d + offset;
       d = clamp(d, 0, 1);
       d = 1 - exp2(-2*d*d);
       return d;
   }

   void main()
   {
       vec3 LightPosition = vec3(0, 1, 0);
       vec3 DiffuseMaterial = vec3(1, 0, 0);
       vec3 AmbientMaterial = vec3(0.2, 0.2, 0.2);

       vec3 N = normalize(v_in.facet_normal);
       vec3 L = LightPosition;
       float df = abs(dot(N, L));
       vec3 color = AmbientMaterial + df * DiffuseMaterial;

       float d1 = min(min(v_in.tri_distance.x, v_in.tri_distance.y), v_in.tri_distance.z);
       float d2 = min(min(v_in.patch_distance.x, v_in.patch_distance.y), v_in.patch_distance.z);
       color = amplify(d1, 40, -0.5) * amplify(d2, 60, -0.5) * color;

       out_color = vec4(color, 1.0);
   }"
  (("out_color" :vec4)))

(def-g-> draw-sphere ((:patch 3))
  :vertex (sphere-vert g-pnt)
  :tessellation-control (sphere-tess-con (:vec3 3))
  :tessellation-evaluation (sphere-tess-eval (:vec3 3))
  :geometry (sphere-geom (:vec3 3) (:vec3 3))
  :fragment (sphere-frag :vec3 :vec3 :vec3))

;;- - - - - - - - - - - - - - - - - -

(defun step-demo ()
  (incf factor 0.001)
  (setf (sphere-rot sphere-a)
        (q:from-axis-angle (v! (sin factor) (cos factor) 1) 10s0))
  (clear)
  (map-g #'draw-sphere sphere-stream
         :model_to_clip (model->clip sphere-a camera))
  (swap))

;;- - - - - - - - - - - - - - - - - -

(defun init ()
  (destructuring-bind (d i) (dendrite.primitives:sphere-data
                             :lines-of-longitude 10
                             :lines-of-latitude 10)
    (setf sphere-stream (make-buffer-stream
                         (make-gpu-array d :element-type 'g-pnt)
                         :index-array (make-gpu-array i :element-type :ushort)
                         :draw-mode :patches)
          brick (sample
                 (cepl.sdl2-image:load-image-to-texture
                  (merge-pathnames "brick/col.png" *examples-dir*))))))

(let ((running nil))
  (defun run-loop ()
    (unless brick (init))
    (setf running t)
    (loop :while (and running (not (shutting-down-p))) :do
       (continuable
         (step-host)
         (update-repl-link)
         (step-demo))))
  (defun stop-loop () (setf running nil)))
