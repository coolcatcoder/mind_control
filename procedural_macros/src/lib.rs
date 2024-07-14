extern crate proc_macro;
use std::{collections::{HashMap, HashSet}, iter};

use ahash::RandomState;
use proc_macro::TokenStream;

// Current plan is to use consts, that the proc macros use for their outputs. Don't use the token stream.
// code gen most of the code. Keep meshes here, for consistency.

// (name, path)
const GLTF_TO_IMPORT: &[(&str, &str)] = &[
    ("squaroid", "meshes/squaroid.glb"),
    ("cuboid", "meshes/cuboid.glb"),
];

// MARK: Buffers
const BUFFERS: &[(&str, BufferInfo)] = &[
    (
        "cuboid_colour_vertices",
        BufferInfo {
            buffer_type: BufferType::Device,
            public: false,
            element_type: "[instanced_simple_lit_colour_3d::Vertex]",
            fill: Fill::VerticesFromGltf {
                shader_module: "instanced_simple_lit_colour_3d",
                gltf: "cuboid",
                mesh_index: 0,
            },
        },
    ),
    (
        "cuboid_colour_indices",
        BufferInfo {
            buffer_type: BufferType::Device,
            public: false,
            element_type: "[u16]",
            fill: Fill::IndicesFromGltf {
                gltf: "cuboid",
                mesh_index: 0,
            },
        },
    ),
    (
        "cuboid_colour_instances",
        BufferInfo {
            buffer_type: BufferType::HostVec,
            public: false,
            element_type: "instanced_simple_lit_colour_3d::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
    ),
    (
        "cuboid_colour_potential_instances",
        BufferInfo {
            buffer_type: BufferType::HostHotel,
            public: true,
            element_type: "CuboidColourPotentialInstance",
            fill: Fill::None {
                starting_capacity: 50,
            },
        },
    ),
    (
        "squaroid_uv_vertices",
        BufferInfo {
            buffer_type: BufferType::Device,
            public: false,
            element_type: "[instanced_unlit_uv_2d_stretch::Vertex]",
            fill: Fill::VerticesFromGltf {
                shader_module: "instanced_unlit_uv_2d_stretch",
                gltf: "squaroid",
                mesh_index: 0,
            },
        },
    ),
    (
        "squaroid_uv_indices",
        BufferInfo {
            buffer_type: BufferType::Device,
            public: false,
            element_type: "[u16]",
            fill: Fill::IndicesFromGltf {
                gltf: "squaroid",
                mesh_index: 0,
            },
        },
    ),
    (
        "selection_menu_uv_instances",
        BufferInfo {
            buffer_type: BufferType::HostVec,
            public: true,
            element_type: "instanced_unlit_uv_2d_stretch::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
    ),
    (
        "selection_menu_text_instances",
        BufferInfo {
            buffer_type: BufferType::HostVec,
            public: true,
            element_type: "instanced_text_sdf::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
    ),
    (
        "menu_uv_instances",
        BufferInfo {
            buffer_type: BufferType::HostVec,
            public: true,
            element_type: "instanced_unlit_uv_2d_stretch::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
    ),
    (
        "menu_text_instances",
        BufferInfo {
            buffer_type: BufferType::HostVec,
            public: true,
            element_type: "instanced_text_sdf::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
    ),
];

// Bold decision to allow stuff like "camera_uniform" even though they aren't registered buffers.
// If it isn't a registered buffer then we assume that it is being gotten through a variable or something.
// MARK: Pipelines
const PIPELINES: &[PipelineInfo] = &[
    PipelineInfo {
        name: "instanced_simple_lit_colour_3d",
        descriptors: &[Descriptor::Buffer("camera_uniform")],
    },
    PipelineInfo {
        name: "simple_lit_colour_3d",
        descriptors: &[Descriptor::Buffer("camera_uniform")],
    },
    PipelineInfo {
        name: "instanced_unlit_uv_2d_stretch",
        descriptors: &[Descriptor::Buffer("camera_uniform")],
    },
    PipelineInfo {
        name: "instanced_text_sdf",
        descriptors: &[Descriptor::Buffer("camera_uniform")],
    },
];

// MARK: Commands
const COMMAND_LISTS: &[(&str, &[Command])] = &[
    (
        "creature_window",
        &[
            Command::BindPipeline("instanced_simple_lit_colour_3d"),
            Command::BindVertexBuffers { vertex_buffer: "cuboid_colour_vertices", instance_buffer: Some("cuboid_colour_instances") },
            Command::BindIndexBuffer("cuboid_colour_indices"),
            Command::DrawAssumed,
        ]
    )
];

#[derive(Clone)]
struct BufferInfo {
    public: bool,
    buffer_type: BufferType,
    element_type: &'static str,
    fill: Fill,
}

#[derive(Clone)]
enum BufferType {
    Device,
    HostVec,   // uses the subbuffer allocator
    HostHotel, // experimental
}

#[derive(Clone)]
enum Fill {
    None {
        starting_capacity: usize,
    },
    VerticesFromGltf {
        shader_module: &'static str,
        gltf: &'static str,
        mesh_index: usize,
    },
    IndicesFromGltf {
        gltf: &'static str,
        mesh_index: usize,
    },
    // Don't forget to actually set the variable in new(){}.
    FromVariable(&'static str),
}

// Currently assumes graphics pipeline.
#[derive(Clone)]
struct PipelineInfo {
    name: &'static str,
    descriptors: &'static [Descriptor],
}

#[derive(Clone)]
enum Descriptor {
    Buffer(&'static str),
    Sampler(&'static str),
    Image(&'static str),
}

impl Descriptor {
    fn name(&self) -> &'static str {
        match self {
            Descriptor::Buffer(name) => name,
            Descriptor::Sampler(name) => name,
            Descriptor::Image(name) => name,
        }
    }
}

enum Command {
    BindPipeline(&'static str),
    BindVertexBuffers{vertex_buffer: &'static str, instance_buffer: Option<&'static str>},
    BindIndexBuffer(&'static str),
    // Perhaps add custom draw index and regular draw ones, that all you to select all the extra nonsense that you want.
    DrawAssumed,
}

#[proc_macro]
pub fn generate_gltf_consts(_: TokenStream) -> TokenStream {
    let mut output = String::new();
    for (name, path) in GLTF_TO_IMPORT {
        output.push_str(&format!(
            "const {}_GLTF: &[u8] = include_bytes!(\"{path}\");\n",
            name.to_uppercase()
        ));
    }
    output.parse().unwrap()
}

#[proc_macro]
pub fn generate_buffers_struct(_: TokenStream) -> TokenStream {
    let mut buffers_struct = "pub struct ProcBuffers {\n".to_string();

    for (name, info) in BUFFERS {
        if info.public {
            buffers_struct.push_str("pub ")
        }
        buffers_struct.push_str(name);
        buffers_struct.push_str(": ");

        match info.buffer_type {
            BufferType::Device => buffers_struct.push_str("Subbuffer"),
            BufferType::HostVec => buffers_struct.push_str("Vec"),
            BufferType::HostHotel => buffers_struct.push_str("Hotel"),
        }

        buffers_struct.push_str("<");
        buffers_struct.push_str(info.element_type);
        buffers_struct.push_str(">,\n")
    }

    buffers_struct.push_str("}");

    buffers_struct.parse().unwrap()
}

#[proc_macro]
pub fn generate_buffers_struct_impl_new(_: TokenStream) -> TokenStream {
    let mut output = "let buffers = Self {\n".to_string();

    for (name, info) in BUFFERS {
        output.push_str(name);
        output.push_str(": ");

        match info.buffer_type {
            BufferType::HostVec => {
                match info.fill {
                    Fill::None { starting_capacity } => {
                        output.push_str(&format!("Vec::with_capacity({starting_capacity})"));
                    }
                    Fill::VerticesFromGltf {
                        shader_module,
                        gltf,
                        mesh_index,
                    } => {
                        output.push_str(
                            format!(
                            "{shader_module}::Vertex::get_array_from_gltf({}_GLTF, {mesh_index})",
                            gltf.to_uppercase().as_str()
                        )
                            .as_str(),
                        );
                    }
                    Fill::IndicesFromGltf { gltf, mesh_index } => {
                        output.push_str(
                            format!(
                                "meshes::get_indices_from_gltf({}_GLTF, {mesh_index})",
                                gltf.to_uppercase().as_str()
                            )
                            .as_str(),
                        );
                    }
                    Fill::FromVariable(variable) => {
                        output.push_str(variable);
                    }
                }
                output.push_str(",\n");
            }
            BufferType::HostHotel => {
                match info.fill {
                    Fill::None { starting_capacity } => {
                        output.push_str(&format!(
                            "Hotel::new(Vec::with_capacity({starting_capacity}), 5)"
                        ));
                    }
                    Fill::FromVariable(variable) => {
                        output.push_str(variable);
                    }
                    _ => todo!(),
                }
                output.push_str(",\n");
            }
            BufferType::Device => {
                output.push_str("new_device_buffer(");
                match info.fill {
                    Fill::None { starting_capacity } => {
                        panic!("Cannot have an unfilled device buffer.")
                    }
                    Fill::VerticesFromGltf {
                        shader_module,
                        gltf,
                        mesh_index,
                    } => {
                        output.push_str(&format!(
                            "{shader_module}::Vertex::get_array_from_gltf({}_GLTF, {mesh_index})",
                            gltf.to_uppercase()
                        ));
                    }
                    Fill::IndicesFromGltf { gltf, mesh_index } => {
                        output.push_str(&format!(
                            "meshes::get_indices_from_gltf({}_GLTF, {mesh_index})",
                            gltf.to_uppercase()
                        ));
                    }
                    Fill::FromVariable(variable) => {
                        output.push_str(variable);
                    }
                }
                output.push_str(", &allocators.memory_allocator, &mut command_buffer_builder),\n");
            }
            BufferType::HostHotel => todo!(),
        }
    }

    output.push_str("};");

    output.parse().unwrap()
}

fn buffers_map() -> HashMap<&'static str, BufferInfo, RandomState> {
    BUFFERS
        .iter()
        .map(|(name, info)| (*name, info.clone()))
        .collect()
}

// MARK: Pipeline Gen
#[proc_macro]
pub fn generate_pipelines_struct(_: TokenStream) -> TokenStream {
    let mut output = "struct Pipelines {\n".to_string();

    for info in PIPELINES {
        output.push_str(&format!("{}: Arc<GraphicsPipeline>,\n", info.name));
    }

    output.push_str("}");
    output.parse().unwrap()
}

#[proc_macro]
pub fn generate_pipelines_struct_impl_new(_: TokenStream) -> TokenStream {
    let mut output = "Self {".to_string();

    for info in PIPELINES {
        output.push_str(&format!(
            "\n{name}: GraphicsPipeline::new(
            device.clone(),
            None,
            GraphicsPipelineCreateInfo {{
                viewport_state: Some(ViewportState::default()),
                rasterization_state: Some(RasterizationState {{
                    cull_mode: CullMode::Back,
                    front_face: FrontFace::CounterClockwise,
                    ..Default::default()
                }}),
                input_assembly_state: Some(InputAssemblyState {{
                    topology: PrimitiveTopology::TriangleList,
                    ..Default::default()
                }}),
                multisample_state: Some(MultisampleState::default()),
                dynamic_state: [DynamicState::Viewport].into_iter().collect(),
                ..{name}::graphics_pipeline_create_info(
                    device.clone(),
                    subpass.clone(),
                )
            }},
        )
        .unwrap(),",
            name = info.name
        ));
    }

    output.push_str("}");
    output.parse().unwrap()
}

// Also here is an old comment on my strange use of PersistentDescriptorSet that I think is still relevant:
//TODO: Shouldn't this be persistent, as the name implies? Why is this being created every frame?
#[proc_macro]
pub fn generate_pipelines_struct_impl_bind_functions(_: TokenStream) -> TokenStream {
    let mut output = String::new();

    let buffers_map = buffers_map();

    for info in PIPELINES {
        let mut parameters = String::new();
        let mut descriptor_sets = String::new();

        let mut persistent_descriptor_sets = true;

        for descriptor in info.descriptors {
            match descriptor {
                Descriptor::Buffer(name) => {
                    if let Some(info) = buffers_map.get(name) {
                        match info.buffer_type {
                            BufferType::HostHotel | BufferType::HostVec => {
                                persistent_descriptor_sets = false;
                                break;
                            }
                            _ => (),
                        }
                    } else {
                        persistent_descriptor_sets = false;
                        break;
                    }
                }
                _ => todo!("support other descriptor types during setting of persistent_descriptor_sets")
            }
        }
        
        if persistent_descriptor_sets {
            for descriptor in info.descriptors {
                parameters.push_str(&format!("{}_", descriptor.name()));
                descriptor_sets.push_str(&format!("{}_", descriptor.name()));
            }
            parameters.push_str("descriptor_set: &Arc<PersistentDescriptorSet>,");
            descriptor_sets.push_str("descriptor_sets.clone(),");
        } else {
            parameters.push_str("allocators: &Allocators,\n");
            descriptor_sets.push_str(&format!("PersistentDescriptorSet::new(
                &allocators.descriptor_set_allocator,
                self.{name}.layout().set_layouts()[0].clone(),[", name=info.name));

            let mut current_binding = 0_usize;
            for descriptor in info.descriptors {
                descriptor_sets.push_str("WriteDescriptorSet::");
                match descriptor {
                    Descriptor::Buffer(name) => {
                        // How do we push parameters if we don't know the type, cause it isn't a real buffer?
                        descriptor_sets.push_str(&format!("WriteDescriptorSet::buffer({current_binding}, {name}),"));
                        current_binding += 1;
                    }
                    _ => todo!("Support other descriptor types during WriteDescriptorSet"),
                }

            }
            descriptor_sets.push_str("],[],).unwrap(),");
        }

        output.push_str(&format!(
            "\nfn proc_bind_{name}(
                &self,
                command_buffer_builder: &mut AutoCommandBufferBuilder<PrimaryAutoCommandBuffer>,
                {}
            ) {{
                command_buffer_builder
                    .bind_pipeline_graphics(self.{name}.clone())
                    .unwrap()
                    .bind_descriptor_sets(
                        PipelineBindPoint::Graphics,
                        self.{name}.layout().clone(),
                        0,
                        
                        {}
                    )
                    .unwrap();
        }}",
        parameters, descriptor_sets, name=info.name,
        ));
    }

    output.parse().unwrap()
}

// MARK: Render Gen
fn command_lists_map() -> HashMap<&'static str, &'static[Command], RandomState> {
    COMMAND_LISTS
        .iter()
        .map(|(name, command_list)| (*name, *command_list))
        .collect()
}

#[proc_macro]
pub fn command_list(input: TokenStream) -> TokenStream {
    let input = input.to_string();
    let mut output = String::new();

    let buffers_map = buffers_map();
    let command_lists_map = command_lists_map();

    let Some(command_list) = command_lists_map.get(input.as_str()) else {
        panic!("Command list not recognised.")
    };

    let allocated_buffers_map: HashSet<&str, RandomState> = iter::empty().collect();

    // add _buffer to the end of anything once allocated to avoid naming collisions

    for command in *command_list {
        match command {
            Command::BindPipeline(name) => {
                todo!();
            }
            Command::BindVertexBuffers { vertex_buffer, instance_buffer } => {
                todo!();
            }
            Command::BindIndexBuffer(name) => {
                if let Some(info) = 
            }
            Command::DrawAssumed => {
                todo!()
            }
        }
    }

    output.parse().unwrap()
}