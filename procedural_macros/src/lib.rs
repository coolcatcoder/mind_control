extern crate proc_macro;
use std::collections::HashMap;

use ahash::RandomState;
use proc_macro::TokenStream;

// Current plan is to use consts, that the proc macros use for their outputs. Don't use the token stream.
// code gen most of the code. Keep meshes here, for consistency.

// (name, path)
const GLTF_TO_IMPORT: &[(&str, &str)] = &[
    ("squaroid", "meshes/squaroid.glb")
];

const BUFFERS: &[(&str, BufferInfo)] = &[
    ("test", BufferInfo {
        editable: false,
        public: true,
        element_type: "[simple_lit_colour_3d::Vertex]",
        fill: Fill::VerticesFromGltf { shader_module: "simple_lit_colour_3d", gltf: "squaroid", mesh_index: 0 },
    })
];

#[derive(Clone)]
struct BufferInfo {
    editable: bool,
    public: bool,
    element_type: &'static str,
    fill: Fill,
}

#[derive(Clone)]
enum Fill {
    None,
    VerticesFromGltf{shader_module: &'static str, gltf: &'static str, mesh_index: usize},
    IndicesFromGltf{gltf: &'static str},
    // Don't forget to actually set the variable in new(){}.
    FromVariable(&'static str)
}

#[proc_macro]
pub fn generate_gltf_consts(_: TokenStream) -> TokenStream {
    let mut output = String::new();
    for (name, path) in GLTF_TO_IMPORT {
        output.push_str("const ");
        output.push_str(name.to_uppercase().as_str());
        output.push_str("_GLTF: &[u8] = include_bytes!(\"");
        output.push_str(path);
        output.push_str("\");\n");
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

        if info.editable {
            buffers_struct.push_str("Vec");
        } else {
            buffers_struct.push_str("Subbuffer");
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
        
        if info.editable {
            todo!()
        } else {
            output.push_str("new_device_buffer(");
            match info.fill {
                Fill::None => {
                    panic!("Cannot have an unfilled device buffer.")
                }
                Fill::VerticesFromGltf { shader_module, gltf, mesh_index } => {
                    output.push_str(shader_module);
                    output.push_str("::Vertex::get_array_from_gltf(");
                    output.push_str(gltf.to_uppercase().as_str());
                    output.push_str("_GLTF, ");
                    output.push_str(mesh_index.to_string().as_str());
                    output.push_str("), &allocators.memory_allocator, &mut command_buffer_builder");
                }
                _ => {
                    todo!()
                }
            }
            output.push_str("),\n");
        }
    }

    output.push_str("};");

    output.parse().unwrap()
}

fn buffers_map() -> HashMap<String, BufferInfo, RandomState> {
    BUFFERS.iter().map(|(name, info)| {
        (name.to_string(), info.clone())
    }).collect()
}