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

private with Ada.Containers.Indefinite_Vectors;

private with ARColl;
private with ARColl.Readers.JSON;
private with ARColl.Containers.Bimaps.Generic_ID_Bimaps;
private with ARColl.IO.Serialization;
private with ARCOll.Strings;

with ARColl.Containers.Bimaps.String_Id_Bimaps;
use ARColl.Containers.Bimaps.String_Id_Bimaps;

with NLColl.Linguistic_Description; use NLColl.Linguistic_Description;
with NLColl.Linguistic_Description.Morphology; use NLColl.Linguistic_Description.Morphology;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Hash;

package NLColl.Corpus.Dictionary_Of_Forms is

    type Forms_Dictionary_Type is private;

    Empty_Forms_Dictionary : constant Forms_Dictionary_Type;

    package From_JSON is

        procedure Load_From_JSONL_Files
          (Forms_Dictionary  : out Forms_Dictionary_Type;
           Containing_Folder : String);

    private

        package String_Vectors is new Ada.Containers.Indefinite_Vectors (Natural, String);
        package Alphabetical_Sorting is new String_Vectors.Generic_Sorting ("<" => "<");

        function Get_All_JSONL_Files (Containing_Folder : String) return String_Vectors.Vector;

        procedure Load_JSONL_File
          (Forms_Dictionary : in out Forms_Dictionary_Type;
           Filename         : String);

        procedure Load_JSON
          (JSON_Text  : in  String;
           Form_List  : out String_Vectors.Vector;
           Morphology : out Morphological_Features_Vectors.Vector);

        package Validation is

            use ARColl.Readers.JSON;

            procedure Validate (JSON_Data : not null JSON_Element_Class_Access) with Inline;

        private

            procedure Validate_Form (JSON_Form : not null JSON_Element_Class_Access) with Inline;
            -- Form can be a non empty string or an array of non empty strings.

            procedure Validate_Morpho (JSON_Morpho : not null JSON_Element_Class_Access) with Inline;
            -- Morpho must be a non empty array containing objects.
            -- Each object must provide the following key/values elements:
            --   * "lemma"  : string, mandatory, not nullable
            --   * "pos"    : string, mandatory, not nullable
            --   * "mood"   : string, optional, nullable
            --   * "tense"  : string, optional, nullable
            --   * "person" : string, optional, nullable
            --   * "number" : string, optional, nullable
            --   * "gender" : string, optional, nullable
            --   * "case"   : string, optional, nullable
            --   * "degree" : string, optional, nullable

            procedure JSON_Assert (Check : Boolean; Message : String) with Inline;
            -- Raise an Invalid_Or_Malformed_JSON exception with Message when Check is False.

        end Validation;

        Invalid_Or_Malformed_JSON, Invalid_Value : exception;

    end From_JSON;

    procedure Load
      (Forms_Dict           : out Forms_Dictionary_Type;
       Serialized_File_Name : in  String;
       Data_Folder_Name     : in  String);

private

    use ARColl;
    use ARColl.Strings;

    type Morpho_Pattern_Type is record
        PosTag      : POS_Type_Array    := Null_POS;
        Mood        : Mood_Type         := NONE;
        Tense       : Tense_Type        := NONE;
        Person      : Person_Type       := NONE;
        Number      : Number_Type       := NONE;
        Gender      : Gender_Type       := NONE;
        GCase       : GCase_Type_Array  := (others => NONE);
        Degree      : Degree_Type       := NONE;
        Person_Poss : Person_Type       := NONE;
        Number_Poss : Number_Type       := NONE;
    end record;

    function To_String
      (Pattern : Morpho_Pattern_Type) return String with Inline;

    function Hash
      (Pattern : Morpho_Pattern_Type) return Ada.Containers.Hash_Type with Inline;

    pragma Suppress (Tampering_Check);
    package Morpho_Pattern_Id_Bimaps is new
      ARColl.Containers.Bimaps.Generic_ID_Bimaps
        (ID_Type      => Index_Type,
         Element_Type => Morpho_Pattern_Type,
         Hash         => Hash);

    type Single_Analysis_Type is
        record
            Lemma_ID   : ARColl.Containers.Bimaps.String_Id_Bimaps.Extended_ID;
            Pattern_ID : Morpho_Pattern_Id_Bimaps.Extended_ID;
        end record;

    type Form_ID_Array is array (Index_Type range <>)
      of ARColl.Containers.Bimaps.String_Id_Bimaps.Extended_ID;

    type Analysis_Array is array (Index_Type range <>) of Single_Analysis_Type;

    type Morpho_Data_Type
      (Last_Form     : Index_Type;
       Last_Analysis : Index_Type) is
        record
            Form_IDs : Form_ID_Array
              (Index_Type'First .. Last_Form) := (others => -1);

            Analysis : Analysis_Array
              (Index_Type'First .. Last_Analysis) := (others => (Lemma_ID => -1, Pattern_ID => -1));
        end record;

    pragma Suppress (Tampering_Check);
    package Dictionary_Forms_Vectors is
      new Ada.Containers.Indefinite_Vectors
        (Index_Type   => Natural,
         Element_Type => Morpho_Data_Type);

    pragma Suppress (Tampering_Check);
    package Dict_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => Dictionary_Forms_Vectors.Vector,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=",
         "="             => Dictionary_Forms_Vectors."=");
    -- Main dictionary vector of Dictionary_Forms_Vectors elements.

    type Forms_Dictionary_Type is
        record
            Initialized     : Boolean := False;
            Dict            : Dict_Maps.Map;

            Forms           : ARColl.Containers.Bimaps.String_Id_Bimaps.Bimap_Type;
            Lemma           : ARColl.Containers.Bimaps.String_Id_Bimaps.Bimap_Type;
            Morpho_Patterns : Morpho_Pattern_Id_Bimaps.Bimap_Type;
        end record;

    package Dict_Serialization is new ARColl.IO.Serialization (Forms_Dictionary_Type);

    Empty_Forms_Dictionary : constant Forms_Dictionary_Type
      := (Initialized            => False,
          Dict                   => Dict_Maps.Empty_Map,
          Lemma                  => ARColl.Containers.Bimaps.String_Id_Bimaps.Empty_Bimap,
          Forms                  => ARColl.Containers.Bimaps.String_Id_Bimaps.Empty_Bimap,
          Morpho_Patterns        => Morpho_Pattern_Id_Bimaps.Empty_Bimap);

    procedure Finalize
      (Forms_Dictionary : in out Forms_Dictionary_Type);

    function Get_Morphological_Interpretations
      (Form_List : in String_Vectors.Vector) return Morphological_Analysis_Vectors.Vector;

end NLColl.Corpus.Dictionary_Of_Forms;

