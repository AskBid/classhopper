# IGES Processing Flow

getIgesEntitities ->

BuilderSectionedIges (gets the Sections of the IGES file) SectionedIges
   \/
BuilderDirectory : buildDEs (finds the Directory Entries) DirEntry
   \/
BuilderParameter : formatParameter (Just prepares the Pameter for parsing as [Text])
                   so the parsing uses Text as token.
   \/
ParameterParser : the actual parsers, that splits for different entities (128, 144 ..)
