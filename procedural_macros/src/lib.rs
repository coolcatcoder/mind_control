extern crate proc_macro;
use std::{
    collections::{HashMap, HashSet},
    iter,
};

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
const BUFFERS: &[StructBufferInfo] = &[
        StructBufferInfo {
            name: "cuboid_colour_vertices",
            buffer_type: BufferType::Device,
            public: false,
            element_type: "[instanced_simple_lit_colour_3d::Vertex]",
            fill: Fill::VerticesFromGltf {
                shader_module: "instanced_simple_lit_colour_3d",
                gltf: "cuboid",
                mesh_index: 0,
            },
        },
    
        
        StructBufferInfo {
            name: "cuboid_colour_indices",
            buffer_type: BufferType::Device,
            public: false,
            element_type: "[u16]",
            fill: Fill::IndicesFromGltf {
                gltf: "cuboid",
                mesh_index: 0,
            },
        },
    
    
        
        StructBufferInfo {
            name: "cuboid_colour_instances",
            buffer_type: BufferType::HostVec,
            public: false,
            element_type: "instanced_simple_lit_colour_3d::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
        
        StructBufferInfo {
            name: "cuboid_colour_potential_instances",
            buffer_type: BufferType::HostHotel,
            public: true,
            element_type: "CuboidColourPotentialInstance",
            fill: Fill::None {
                starting_capacity: 50,
            },
        },
   
        
        StructBufferInfo {
            name: "squaroid_uv_vertices",
            buffer_type: BufferType::Device,
            public: false,
            element_type: "[instanced_unlit_uv_2d_stretch::Vertex]",
            fill: Fill::VerticesFromGltf {
                shader_module: "instanced_unlit_uv_2d_stretch",
                gltf: "squaroid",
                mesh_index: 0,
            },
        },
    
        
        StructBufferInfo {
            name: "squaroid_uv_indices",
            buffer_type: BufferType::Device,
            public: false,
            element_type: "[u16]",
            fill: Fill::IndicesFromGltf {
                gltf: "squaroid",
                mesh_index: 0,
            },
        },
    
        
        StructBufferInfo {
            name: "selection_menu_uv_instances",
            buffer_type: BufferType::HostVec,
            public: true,
            element_type: "instanced_unlit_uv_2d_stretch::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
   
        
        StructBufferInfo {
            name: "selection_menu_text_instances",
            buffer_type: BufferType::HostVec,
            public: true,
            element_type: "instanced_text_sdf::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
    
        
        StructBufferInfo {
            name: "menu_uv_instances",
            buffer_type: BufferType::HostVec,
            public: true,
            element_type: "instanced_unlit_uv_2d_stretch::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
    
        
        StructBufferInfo {
            name: "menu_text_instances",
            buffer_type: BufferType::HostVec,
            public: true,
            element_type: "instanced_text_sdf::Instance",
            fill: Fill::None {
                starting_capacity: 30,
            },
        },
    
];

const EXTERNAL_BUFFERS: &[ConstBufferInfo] = &[
    ConstBufferInfo {
        name: "camera_uniform",
        path: "camera_uniform",
        buffer_type: BufferType::DeviceFromEditable,
        element_type: "instanced_simple_lit_colour_3d::CameraUniform",
    }
];

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
const COMMAND_LISTS: &[(&str, &[Command])] = &[(
    "creature_window",
    &[
        Command::BindPipeline("instanced_simple_lit_colour_3d"),
        Command::BindVertexBuffers {
            vertex_buffer: "cuboid_colour_vertices",
            instance_buffer: Some("cuboid_colour_instances"),
        },
        Command::BindIndexBuffer("cuboid_colour_indices"),
        Command::DrawAssumed,
    ],
)];

#[derive(Clone)]
struct StructBufferInfo {
    name: &'static str,
    public: bool,
    buffer_type: BufferType,
    element_type: &'static str,
    fill: Fill,
}

#[derive(Clone)]
struct ConstBufferInfo {
    name: &'static str,
    path: &'static str,
    buffer_type: BufferType,
    element_type: &'static str,
}

struct BufferInfo {
    name: &'static str,
    // Including name. Sometimes.
    path: String,
    buffer_type: BufferType,
    element_type: &'static str,
}

impl BufferInfo {
    fn editable(&self) -> bool {
        match self.buffer_type {
            BufferType::Device => false,
            BufferType::HostHotel | BufferType::HostVec | BufferType::DeviceFromEditable => true,
        }
    }

    // TODO: add .clone() when needed.
    fn bind_path(&self) -> String {
        if self.editable() &! matches!(self.buffer_type, BufferType::DeviceFromEditable) {
            format!("{}_buffer", self.name)
        } else {
            format!("{}", self.path)
        }
    }

    fn buffer_allocation(&self) -> String {
        match self.buffer_type {
            BufferType::Device | BufferType::DeviceFromEditable => String::new(),
            BufferType::HostHotel => todo!(),
            BufferType::HostVec => {
                format!(
                    "let {name}_buffer = self
                        .allocators
                        .subbuffer_allocator
                        .from_slice(&{});",
                    self.path,
                    name = self.name
                )
            }
        }
    }

    fn len_allocation(&self) -> String {
        match self.buffer_type {
            BufferType::Device | BufferType::DeviceFromEditable => String::new(),
            BufferType::HostHotel => todo!(),
            BufferType::HostVec => String::new(),
        }
    }

    fn len(&self) -> String {
        match self.buffer_type {
            BufferType::Device | BufferType::DeviceFromEditable => format!("{}.len() as u32", self.path),
            BufferType::HostHotel => todo!(),
            BufferType::HostVec => format!("{}.len() as u32", self.path),
        }
    }

    fn allocated_element_type(&self) -> String {
        match self.buffer_type {
            BufferType::Device | BufferType::DeviceFromEditable => self.element_type.to_string(),
            BufferType::HostHotel => todo!(),
            BufferType::HostVec => format!("[{}]", self.element_type),
        }
    }
}

#[derive(Clone, Copy)]
enum BufferType {
    Device,
    DeviceFromEditable, // A device buffer that came from an editable buffer.
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

impl PipelineInfo {
    fn persistent(&self) -> bool {
        let buffers_map = buffers_map();
        for descriptor in self.descriptors {
            match descriptor {
                Descriptor::Buffer(name) => {
                    if buffers_map.get(name).expect("Buffer should exist.").editable() {
                        return false;
                    }
                }
                _ => todo!(
                    "support other descriptor types during setting of persistent_descriptor_sets"
                ),
            }
        }
        return true;
    }
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
    BindVertexBuffers {
        vertex_buffer: &'static str,
        instance_buffer: Option<&'static str>,
    },
    BindIndexBuffer(&'static str),
    // Perhaps add custom draw index and regular draw ones, that allow you to select all the extra nonsense that you want.
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

    for info in BUFFERS {
        if info.public {
            buffers_struct.push_str("pub ")
        }
        buffers_struct.push_str(info.name);
        buffers_struct.push_str(": ");

        match info.buffer_type {
            BufferType::Device | BufferType::DeviceFromEditable => buffers_struct.push_str("Subbuffer"),
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

    for info in BUFFERS {
        output.push_str(info.name);
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
            BufferType::DeviceFromEditable => panic!("DeviceFromEditable is not allowed in a buffer struct."),
        }
    }

    output.push_str("};");

    output.parse().unwrap()
}

fn struct_buffers_map() -> HashMap<&'static str, StructBufferInfo, RandomState> {
    BUFFERS
        .iter()
        .map(|info| (info.name, info.clone()))
        .collect()
}

fn external_buffers_map() -> HashMap<&'static str, ConstBufferInfo, RandomState> {
    EXTERNAL_BUFFERS
        .iter()
        .map(|info| (info.name, info.clone()))
        .collect()
}

fn buffers_map() -> HashMap<&'static str, BufferInfo, RandomState> {
    EXTERNAL_BUFFERS
        .iter()
        .map(|info| (info.name, BufferInfo {
            name: info.name,
            path: info.path.to_string(),
            buffer_type: info.buffer_type,
            element_type: info.element_type,
        }))
        .chain(
            BUFFERS
                .iter()
                .map(|info| (info.name, BufferInfo {
                    name: info.name,
                    path: format!("self.proc_buffers.{}", info.name),
                    buffer_type: info.buffer_type,
                    element_type: info.element_type,
                })),
        )
        .collect()
}

fn pipelines_map() -> HashMap<&'static str, PipelineInfo, RandomState> {
    PIPELINES
        .iter()
        .map(|info| (info.name, info.clone()))
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

        let persistent_descriptor_sets = info.persistent();

        if persistent_descriptor_sets {
            for descriptor in info.descriptors {
                parameters.push_str(&format!("{}_", descriptor.name()));
                descriptor_sets.push_str(&format!("{}_", descriptor.name()));
            }
            parameters.push_str("descriptor_sets: &Arc<PersistentDescriptorSet>,");
            descriptor_sets.push_str("descriptor_sets.clone(),");
        } else {
            parameters.push_str("allocators: &Allocators,\n");
            descriptor_sets.push_str(&format!(
                "PersistentDescriptorSet::new(
                &allocators.descriptor_set_allocator,
                self.{name}.layout().set_layouts()[0].clone(),[",
                name = info.name
            ));

            let mut current_binding = 0_usize;
            for descriptor in info.descriptors {
                descriptor_sets.push_str("WriteDescriptorSet::");
                match descriptor {
                    Descriptor::Buffer(name) => {
                        let info = buffers_map.get(name).expect("Buffer should exist.");

                        parameters.push_str(&format!("{}: Subbuffer<{}>,", info.name, info.allocated_element_type()));
                        descriptor_sets.push_str(&format!(
                            "buffer({current_binding}, {name}),"
                        ));
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
            parameters,
            descriptor_sets,
            name = info.name,
        ));
    }

    output.parse().unwrap()
}

// MARK: Render Gen
fn command_lists_map() -> HashMap<&'static str, &'static [Command], RandomState> {
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
    let pipelines_map = pipelines_map();
    let command_lists_map = command_lists_map();

    let Some(command_list) = command_lists_map.get(input.as_str()) else {
        panic!("Command list not recognised.")
    };

    let mut allocated_buffers_map: HashSet<&str, RandomState> = iter::empty().collect();
    let mut allocation = String::new();

    let mut index_len: Option<String> = None;
    let mut instance_len = "1".to_string();

    // add _buffer to the end of anything once allocated to avoid naming collisions

    for command in *command_list {
        match command {
            Command::BindPipeline(name) => {
                let info = pipelines_map.get(name).expect("Pipeline should exist.");

                output.push_str(&format!("self.pipelines.proc_bind_{name}(&mut command_buffer_builder, "));

                if info.persistent() {
                    todo!("add persistent")
                } else {
                    output.push_str("&self.allocators, ");

                    for descriptor in info.descriptors {
                        match descriptor {
                            Descriptor::Buffer(buffer) => {
                                let buffer_info = buffers_map.get(buffer).expect("Buffer should exist.");

                                output.push_str(&format!("{}, ", buffer_info.bind_path()));

                                if matches!(allocated_buffers_map.get(buffer), None) {
                                    allocation.push_str(&buffer_info.buffer_allocation());
                                    allocated_buffers_map.insert(buffer);
                                }
                            },
                            Descriptor::Image(image) => todo!(),
                            Descriptor::Sampler(sampler) => todo!(),
                        }
                    }
                }

                output.push_str(");");
            }
            Command::BindVertexBuffers {
                vertex_buffer,
                instance_buffer,
            } => {
                let vertex_buffer_info = buffers_map
                    .get(vertex_buffer)
                    .expect("Vertex buffer should exist.");
                let vertex_buffer_path = vertex_buffer_info.bind_path();

                if matches!(allocated_buffers_map.get(vertex_buffer), None) {
                    allocation.push_str(&vertex_buffer_info.buffer_allocation());
                    allocated_buffers_map.insert(vertex_buffer);
                }

                let instance_buffer_path = if let Some(instance_buffer) = instance_buffer {
                    let instance_buffer_info = buffers_map
                        .get(instance_buffer)
                        .expect("Vertex buffer should exist.");
                    let instance_buffer_path = instance_buffer_info.bind_path();
                    instance_len = instance_buffer_info.len();

                    if matches!(allocated_buffers_map.get(instance_buffer), None) {
                        allocation.push_str(&instance_buffer_info.buffer_allocation());
                        allocation.push_str(&instance_buffer_info.len_allocation());
                        allocated_buffers_map.insert(instance_buffer);
                    }

                    instance_buffer_path
                } else {
                    instance_len = "1".to_string();
                    String::new()
                };

                output.push_str(&format!(
                    "\ncommand_buffer_builder
                        .bind_vertex_buffers(
                            0,
                            (
                                {vertex_buffer_path}
                                    .clone(),
                                {instance_buffer_path},
                            ),
                        )
                        .unwrap();"
                ));
            }
            Command::BindIndexBuffer(name) => {
                let info = buffers_map.get(name).expect("Index buffer should exist.");
                let path = info.bind_path();
                index_len = Some(info.len());

                if matches!(allocated_buffers_map.get(name), None) {
                    allocation.push_str(&info.buffer_allocation());
                    allocation.push_str(&info.len_allocation());
                    allocated_buffers_map.insert(name);
                }

                output.push_str(&format!(
                    "\ncommand_buffer_builder.bind_index_buffer(
                            {path}
                                .clone(),
                        )
                        .unwrap();"
                ));
            }
            Command::DrawAssumed => {
                let Some(index_len) = &index_len else {
                    panic!("You must bind an index buffer!")
                };

                output.push_str(&format!(
                    "\ncommand_buffer_builder.draw_indexed(
                            {index_len},
                            {instance_len},
                            0,
                            0,
                            0,
                        )
                        .unwrap();"
                ));
            }
        }
    }

    allocation.push_str(&output);
    allocation.parse().unwrap()
}
