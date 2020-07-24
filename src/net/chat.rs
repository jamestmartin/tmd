use serde::{Deserialize, Serialize};

// TODO: Support more features.
#[derive(Serialize, Deserialize)]
pub struct Chat {
    pub text: String,
}
