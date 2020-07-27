use serde::{Deserialize, Serialize};

// TODO: Support more features.
#[derive(Clone, Serialize, Deserialize)]
pub struct Chat {
    pub text: Box<str>,
}
