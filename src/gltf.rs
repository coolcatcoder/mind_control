use std::str::FromStr;

use clunky::{
    buffer_contents::Colour3DInstance,
    math::{add_3d, mul_3d_by_1d, sub_3d, Matrix4},
    physics::physics_3d::{
        aabb::{AabbCentredOrigin, AabbMinMax},
        bodies::{CommonBody, ImmovableCuboid},
        verlet::{bodies::Cuboid, Particle},
    },
};
use gltf::Gltf;

const GLTF_SCENES: &[&[u8]] = &[include_bytes!("world.glb")];

pub struct Nameless {
    pub scenes: Vec<Scene>,
}

pub struct Scene {
    pub render_objects: Vec<RenderObject>,
    pub bodies: Vec<CommonBody<f32>>,
    pub grow_zones: Vec<AabbMinMax<f32>>,
}

pub enum RenderObject {
    None,
    Cuboid { body_index: usize, colour: [f32; 4] },
    CuboidNoPhysics(Colour3DInstance),
}

pub fn load_scenes() -> Nameless {
    let mut nameless = Nameless {
        scenes: Vec::with_capacity(GLTF_SCENES.len()),
    };

    for gltf_scene in GLTF_SCENES {
        let gltf = Gltf::from_slice(gltf_scene).unwrap();

        let mut scene = Scene {
            render_objects: Vec::with_capacity(gltf.nodes().len()),
            bodies: Vec::with_capacity(gltf.nodes().len()),
            grow_zones: Vec::with_capacity(10),
        };

        for node in gltf.nodes() {
            let Some(properties) = node.extras() else {
                continue;
            };
            let properties = properties.get();
            println!("{properties}");

            let transform_decomposed = {
                let mut temp_transform_decomposed = node.transform().decomposed();

                //println!("{:?}",temp_transform_decomposed);

                //temp_transform_decomposed.0[0] = -temp_transform_decomposed.0[0];
                temp_transform_decomposed.0[1] = -temp_transform_decomposed.0[1];
                temp_transform_decomposed.0[2] = -temp_transform_decomposed.0[2];

                temp_transform_decomposed
            };
            println!("{:?}", transform_decomposed);

            let colour = if let Some(colour_index) = properties.find("\"colour\":") {
                let mut colour_string = String::with_capacity(15);
                let mut index = colour_index + 10;
                while properties.as_bytes()[index] != b"]"[0] {
                    colour_string.push(properties.as_bytes()[index] as char);
                    index += 1;
                }

                let mut temp_colour = [0.0; 4];
                for (index, float) in colour_string.split(",").enumerate() {
                    temp_colour[index] = f32::from_str(float).unwrap();
                }

                println!("temp colour: {:?}", temp_colour);

                temp_colour
            } else {
                [1.0; 4]
            };

            let mut does_not_have_required_physics = false;

            if properties.contains("\"instance\":\"cuboid no physics\"") {
                scene
                    .render_objects
                    .push(RenderObject::CuboidNoPhysics(Colour3DInstance::new(
                        colour,
                        Matrix4::from_translation(transform_decomposed.0)
                            //* Matrix4::from_quaternion([ 0.0, 0.7071068, 0.0, 0.7071068 ])
                            * Matrix4::from_quaternion(transform_decomposed.1)
                            * Matrix4::from_scale(transform_decomposed.2),
                    )));
            }

            if properties.contains("\"instance\":\"cuboid\"") {
                does_not_have_required_physics = true;
                scene.render_objects.push(RenderObject::Cuboid {
                    body_index: scene.bodies.len() + 1,
                    colour,
                }); // +1 accounts for player.
            }

            if properties.contains("\"physics\":\"cuboid\"") {
                if !does_not_have_required_physics {
                    panic!(
                        "Node '{}', has physics, when the instance type does not require it!",
                        node.name().unwrap_or("NAMELESS")
                    );
                }
                does_not_have_required_physics = false;
                scene.bodies.push(CommonBody::Cuboid(Cuboid {
                    particle: Particle::from_position(transform_decomposed.0),
                    half_size: mul_3d_by_1d(transform_decomposed.2, 0.5),
                }));
            }
            if properties.contains("\"physics\":\"immovable cuboid\"") {
                scene
                    .bodies
                    .push(CommonBody::ImmovableCuboid(ImmovableCuboid {
                        aabb: AabbCentredOrigin {
                            position: transform_decomposed.0,
                            half_size: mul_3d_by_1d(transform_decomposed.2, 0.5),
                        },
                    }));
            }

            if does_not_have_required_physics {
                panic!(
                    "Node '{}', does not have physics, when the instance type requires it!",
                    node.name().unwrap_or("NAMELESS")
                );
            }

            if properties.contains("\"special\":\"grow zone\"") {
                scene.grow_zones.push(AabbMinMax {
                    min: sub_3d(
                        transform_decomposed.0,
                        mul_3d_by_1d(transform_decomposed.2, 0.5),
                    ),
                    max: add_3d(
                        transform_decomposed.0,
                        mul_3d_by_1d(transform_decomposed.2, 0.5),
                    ),
                });
            }
        }
        scene.render_objects.shrink_to_fit();
        scene.bodies.shrink_to_fit();
        scene.grow_zones.shrink_to_fit();
        nameless.scenes.push(scene);
    }

    nameless
}

/*
fn dungeon_3d_scene_to_arrays(
    gltf: &Document,
    debug: &mut String,
    scene_prefix: &'static str,
    node_names_and_corresponding_infixes: &[(&'static str, &'static str)],
) -> String {
    let mut constant_arrays = vec![];

    for strings in node_names_and_corresponding_infixes {
        constant_arrays.push(format!(
            "pub const {}_{}_COLOUR_3D_INSTANCES: &[buffer_contents::Colour3DInstance] = &[",
            scene_prefix, strings.1
        ));
    }

    for node in gltf.nodes() {
        if let Some(node_name) = node.name() {
            let transform_decomposed = node.transform().decomposed();
            let colour = {
                if let Some(temp_colour) = node.extras() {
                    let temp_colour = temp_colour.get();

                    let temp_colour = temp_colour.get(10..temp_colour.len() - 1).unwrap();

                    temp_colour
                } else {
                    "[1.0,1.0,1.0,1.0]"
                }
            };

            for strings_index in 0..node_names_and_corresponding_infixes.len() {
                let strings = node_names_and_corresponding_infixes[strings_index];
                if node_name.contains(strings.0) {
                    constant_arrays[strings_index].push_str(&format!(
                        "
                    buffer_contents::Colour3DInstance::new(
                        {},
                        math::Matrix4::from_translation([{:?},-{:?},-{:?}]).multiply(math::Matrix4::from_scale([{:?},{:?},{:?}])),
                    ),
                    ",
                        colour,
                        transform_decomposed.0[0], transform_decomposed.0[1], transform_decomposed.0[2], transform_decomposed.2[0], transform_decomposed.2[1], transform_decomposed.2[2],
                    ));
                }
            }
        }
    }

    let mut combined_arrays = String::from("");

    for mut constant_array in constant_arrays {
        constant_array.push_str("];");

        combined_arrays.push_str(&constant_array);
    }

    debug.push_str(&format!("{}\n", combined_arrays));
    combined_arrays
}
*/
