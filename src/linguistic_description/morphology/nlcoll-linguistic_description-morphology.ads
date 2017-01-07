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

with Ada.Strings.Hash;

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;

package NLColl.Linguistic_Description.Morphology is

    pragma Elaborate_Body;

    ------------------------------
    --- Morphological Descriptions
    ------------------------------

    type Morphological_Features_Type is record
        Lemma : Unbounded_String;
        -- Lemma

        PosTag : POS_Type_Array := Null_POS;
        -- Fine-grained part-of-speech tag

        Mood  : Mood_Type := NONE;
        -- Temporal verb feature

        Tense : Tense_Type := NONE;
        -- Temporal verb feature

        Person : Person_Type := NONE;
        -- Grammatical person

        Number : Number_Type := NONE;
        -- Grammatical numbers

        Gender : Gender_Type := NONE;
        -- Grammatical genders

        GCase : GCase_Type_Array := (others => NONE);
        -- Grammatical case

        Degree : Degree_Type := NONE;
        -- Adjective degree (positive, comparative, superlative)

        Person_Poss : Person_Type := NONE;
        -- Grammatical person of possessor (in ADJ-POSS)

        Number_Poss : Number_Type := NONE;
        -- Grammatical number of possessor (in ADJ-POSS)

        Voice : Voice_Type := NONE;
        -- Diathesis features

        MWE_Range : MWE_Range_Type := Null_MWE_Range;
        -- Multi-word expression range

        Source : Source_Type := NONE;
        -- Source
    end record;

    Null_Morphological_Features : constant Morphological_Features_Type
      := (Lemma       => Null_Unbounded_String,
          PosTag      => Null_POS,
          Mood        => NONE,
          Tense       => NONE,
          Person      => NONE,
          Number      => NONE,
          Gender      => NONE,
          GCase       => Null_GCase,
          Degree      => NONE,
          Person_Poss => NONE,
          Number_Poss => NONE,
          Voice       => NONE,
          MWE_Range   => Null_MWE_Range,
          Source      => NONE);

    package Morphological_Features_Vectors is
      new Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Morphological_Features_Type);

    package Morphological_Analysis_Vectors is
      new Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Morphological_Features_Vectors.Vector,
         "="          => Morphological_Features_Vectors."=");

    type Morphological_Description_Type is record

        Morphological_Interpretations : Morphological_Analysis_Vectors.Vector;
        -- Interpretations level: this is a vector of 'ambiguous' morphological
        -- analysis (vectors of concatenated elements).

    end record;

    Empty_Morphology : constant Morphological_Description_Type
      := (Morphological_Interpretations => Morphological_Analysis_Vectors.Empty_Vector);

    -----
    -- Match Functions
    -----

    function Match_POS
      (Source_PosTag     : POS_Type_Array;
       Target_First_Pos  : Misc_POS_Type;
       Target_Second_Pos : Misc_POS_Type;
       Strict_Match      : Boolean := False) return Boolean;

    function Match_POS
      (Source_PosTag     : POS_Type_Array;
       Target_First_Pos  : Misc_POS_Type;
       Strict_Match      : Boolean := False) return Boolean;

    function Match_POS
      (Source_PosTag : POS_Type_Array;
       Target_PosTag : POS_Type_Array;
       Strict_Match  : Boolean := False) return Boolean;

    function Match_MWE_POS
      (First_PosTag  : POS_Type_Array;
       Second_PosTag : POS_Type_Array) return Boolean;

    function Is_Open_Class
      (POS : in POS_Type_Array) return Boolean;

    function Is_Close_Class
      (POS : in POS_Type_Array) return Boolean is (not Is_Open_Class (POS));

    ----------------
    -- Map Functions
    ----------------

    function Get_Gender
      (Gender : String) return Gender_Type with Inline;

    function Get_Number
      (Number : String) return Number_Type with Inline;

    function Get_Person
      (Person : String) return Person_Type with Inline;

    function Get_Mood
      (Mood   : String) return Mood_Type with Inline;

    function Get_Tense
      (Tense  : String) return Tense_Type with Inline;

    function Get_Degree
      (Degree : String) return Degree_Type with Inline;

    function Get_GCase
      (GCase   : String) return GCase_Type_Array with Inline;

    ------------------
    -- Match Functions
    ------------------

    function Gen_Match
      (A, B         : Gender_Type;
       Strict_Match : Boolean := True) return Boolean with Inline;

    function Num_Match
      (A, B         : Number_Type;
       Strict_Match : Boolean := True) return Boolean with Inline;

    function Pers_Match
      (A, B         : Person_Type;
       Strict_Match : Boolean := True) return Boolean with Inline;

    function GCase_Match
      (A            : GCase_Type_Array;
       B            : GCase_Type;
       Strict_Match : Boolean := True) return Boolean with Inline;

    --------------------
    -- Combine Functions
    --------------------

    function Combine_Genders
      (A, B : Gender_Type) return Gender_Type with Inline;

    function Combine_Person
      (A, B : Person_Type) return Person_Type with Inline;

    ----
    -- Encoding Functions
    ----

    function Get_POS
      (POS : String) return POS_Type_Array;

    function To_String
      (POS : POS_Type_Array) return String;

    function Create_String
      (GCases : GCase_Type_Array) return String;

    procedure Print
      (Features : in Morphological_Features_Type);

    ---------
    -- Errors
    ---------

    Wrong_POS,
    Wrong_Gender,
    Wrong_Number,
    Wrong_Person,
    Wrong_Mood,
    Wrong_Tense,
    Wrong_GCase,
    Wrong_Degree : exception;

private

    ---
    -- Basic Grammatical Combination Rules
    ---

    Combine_Genders_Array : constant array (Gender_Type, Gender_Type) of Gender_Type
      := (MASC => (MASC => MASC,  FEM => MASC,  BOTH => MASC,  NEUT => MASC,  NONE => NONE),
          FEM  => (MASC => MASC,  FEM => FEM,   BOTH => BOTH,  NEUT => MASC,  NONE => NONE),
          BOTH => (MASC => MASC,  FEM => BOTH,  BOTH => BOTH,  NEUT => MASC,  NONE => NONE),
          NEUT => (MASC => MASC,  FEM => MASC,  BOTH => MASC,  NEUT => MASC,  NONE => NONE),
          NONE => (MASC => NONE,  FEM => NONE,  BOTH => NONE,  NEUT => NONE,  NONE => NONE));

    Combine_Person_Array : constant array (Person_Type, Person_Type) of Person_Type
      := (FIRST  => (FIRST => FIRST,  SECOND => FIRST,   THIRD => FIRST,   ALLVAL => FIRST,   NONE => NONE),
          SECOND => (FIRST => FIRST,  SECOND => SECOND,  THIRD => SECOND,  ALLVAL => SECOND,  NONE => NONE),
          THIRD  => (FIRST => FIRST,  SECOND => SECOND,  THIRD => THIRD,   ALLVAL => THIRD,   NONE => NONE),
          ALLVAL => (FIRST => FIRST,  SECOND => SECOND,  THIRD => THIRD,   ALLVAL => ALLVAL,  NONE => NONE),
          NONE   => (FIRST => NONE,   SECOND => NONE,    THIRD => NONE,    ALLVAL => NONE,    NONE => NONE));

    ---
    -- Structures
    ---

    function Create_String
      (POS : POS_Type_Array) return String;

    function Hash
      (POS : POS_Type_Array) return Ada.Containers.Hash_Type with Inline;
    -- POS'Range         = 4; Maximum POS'Range = 10

    pragma Suppress (Tampering_Check);
    package POS_Array_To_String_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => POS_Type_Array,
         Element_Type    => String,
         Hash            => Hash,
         Equivalent_Keys => "=");

    pragma Suppress (Tampering_Check);
    package String_To_POS_Array_Maps is new
      Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => String,
         Element_Type    => POS_Type_Array,
         Hash            => Ada.Strings.Hash,
         Equivalent_Keys => "=");

    --------------------
    -- Internal Memories
    --------------------

    String_To_POS_Array : String_To_POS_Array_Maps.Map;
    POS_Array_To_String : POS_Array_To_String_Maps.Map;

end NLColl.Linguistic_Description.Morphology;

