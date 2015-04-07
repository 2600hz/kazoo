-record(attribute, {
    id              :: eradius_dict:attribute_id(),
    type = 'octets' :: eradius_dict:attribute_type(),
    name            :: string(),
    enc  = 'no'     :: eradius_dict:attribute_encryption()
}).

-record(vendor, {
    type :: eradius_dict:vendor_id(),
    name :: string()
}).

-record(value, {
    id   :: eradius_dict:value_id(),
    name :: string()
}).
