# IGES Processing Flow

getIgesEntitities ->

BuilderIgesRaw (gets the Sections of the IGES file) IgesRaw
   \/
BuilderDirectory : buildDEs (finds the Directory Entries) DirEntry
   \/
BuilderParameter : formatParameter (Just prepares the Pameter for parsing as [Text])
                   so the parsing uses Text as token.
   \/
ParameterParser : the actual parsers, that splits for different entities (128, 144 ..)
