------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--          Copyright 2009-2011 M. Grella, M. Nicola, D. Christen
--
--  In collaboration with Politecnico di Torino and Università di Torino.
--
--  Supported by
--   (a) Fondazione CRT under a "VivoMeglio" grant (Speak2Home)
--   (b) Region Piedmont under a “Converging Technologies” programme (ATLAS)
--        
--  The project aims at developing open-source linguistic technologies for the
--  Italian language to improve the welfare of people, especially impaired users.
--
--  This is free software; you can redistribute it and/or modify it under
--  terms of the GNU General Public License as published by the Free Software
--  Foundation; either version 2, or (at your option) any later version.
--  This software is distributed in the hope that it will be useful, but WITH
--  OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
--  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
--  for more details. Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Text_IO;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Text_IO;

package body NLColl.Corpus.Dictionary_Of_Forms is

    package body From_JSON is separate;
    
    procedure Load
      (Forms_Dict           : out Forms_Dictionary_Type;
       Serialized_File_Name : in  String;
       Data_Folder_Name     : in  String) is
        
        use Ada.Text_IO;
    begin
        
        Put_Line ("LOADING DICTIONARY...");
        
        if Ada.Directories.Exists (Serialized_File_Name) then
            Dict_Serialization.Deserialize 
              (Element              => Forms_Dict,
               Serialized_File_Name => Serialized_File_Name);
        else
            
            Put_Line ("-- PROCESS ");
            
            From_JSON.Load_From_JSONL_Files (Forms_Dict, Data_Folder_Name);
            
            Put_Line ("-- SERIALIZE");
            
            Dict_Serialization.Serialize 
              (Element              => Forms_Dict,
               Serialized_File_Name => Serialized_File_Name);            
        end if;
        
        Put_Line ("END.");
    end Load;
    
    procedure Finalize
      (Forms_Dictionary : in out Forms_Dictionary_Type) is
    begin
        Forms_Dictionary.Initialized := False;
        Forms_Dictionary.Dict.Clear;
        Forms_Dictionary.Forms.Clear;
        Forms_Dictionary.Lemma.Clear;
        Forms_Dictionary.Morpho_Patterns.Clear;
    end Finalize;
    
    function Hash
      (Pattern : Morpho_Pattern_Type) return Ada.Containers.Hash_Type is
        pragma Unreferenced (Pattern);
    begin
        return 1;
    end Hash;
    
    function To_String
      (Pattern : Morpho_Pattern_Type) return String is
    begin
        return To_String (Pattern.PosTag) & " " &
          Pattern.Mood'Img & " " &
          Pattern.Tense'Img & " " &
          Pattern.Person'Img & " " &
          Pattern.Number'Img & " " &
          Create_String(Pattern.GCase) & " " &
          Pattern.Degree'Img & " " &
          Pattern.Person_Poss'Img & " " &
          Pattern.Number_Poss'Img;
    end To_String;
    
    function Get_Morphological_Interpretations
      (Form_List : in String_Vectors.Vector) return Morphological_Analysis_Vectors.Vector is
        
        Morpho_Int : Morphological_Analysis_Vectors.Vector;
        pragma Unreferenced(Form_List);
    begin
        
        return Morpho_Int;
        
    end Get_Morphological_Interpretations;
    
end NLColl.Corpus.Dictionary_Of_Forms;
