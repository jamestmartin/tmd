#[macro_export]
macro_rules! define_packets {
    { $( packet $name:ident { $( $field:ident : $type:ty ),* } )+ } => {
        $(
            pub struct $name {
                $(
                    pub $field: $type,
                )*
            }

            impl crate::net::serialize::PacketReadable for $name {
                fn read(deser: &mut impl crate::net::serialize::PacketDeserializer) -> Result<Self, String> {
                    $(
                        let $field = deser.read::<$type>()?;
                    )*
                    deser.read_eof()?;
                    Ok($name {
                        $(
                            $field,
                        )*
                    })
                }
            }

            impl crate::net::serialize::PacketWritable for &$name {
                fn write(self, ser: &mut impl crate::net::serialize::PacketSerializer) {
                    $(
                        self.$field.write(ser);
                    )*
                    ser.write_eof();
                }
            }
        )*
    }
}
