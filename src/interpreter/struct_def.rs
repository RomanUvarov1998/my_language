use super::utils::NameToken;
use super::data_type::DataType;

//------------------ StructDef ---------------------

pub struct StructDef {
	fields: Vec<StructFieldDef>,
}

impl StructDef {
	pub fn new(fields: Vec<StructFieldDef>) -> Result<Self, StructDefErr> {
		for i in 0..fields.len() {
			for j in 0..fields.len() {
				if i != j && fields[i].name == fields[j].name {
					return Err( StructDefErr::FieldAlreadyDefined {
						name: fields[i].name.value().to_string(),
					} );
				}
			}
		}
		
		Ok( Self {
			fields,
		} )
	}
}

//------------------ StructFieldDef ---------------------

pub struct StructFieldDef {
	name: NameToken,
	data_type: DataType,
}

//------------------ StructDefErr ---------------------

pub enum StructDefErr {
	FieldAlreadyDefined {
		name: String,
	},
}