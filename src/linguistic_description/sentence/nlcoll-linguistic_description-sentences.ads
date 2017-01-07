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

with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Unchecked_Deallocation;

with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;

with NLColl.Linguistic_Description.Morphology; use NLColl.Linguistic_Description.Morphology;

package NLColl.Linguistic_Description.Sentences is

    ---
    -- Tokens
    ---

    package Tokens is

        type Extended_Token_Index_Type is new Integer range -1 .. Integer'Last;
        -- Index type used for special tokens identifications; includes -1 and 0 values

        subtype Token_Index_Type is Extended_Token_Index_Type range
          1 .. Extended_Token_Index_Type'Last;
        -- Index type used for tokens identifications; only "valid" values > 0

        function Img(Token_Index : Extended_Token_Index_Type) return String is
          (Img(Integer(Token_Index))) with Inline;

        ROOT_ID : constant Extended_Token_Index_Type := 0;
        -- Representes the virtual root

        NONE_ID : constant Extended_Token_Index_Type := -1;
        -- Represents a non-existent token.

        type Token_Index_Pair_Type is record
            First  : Extended_Token_Index_Type  := NONE_ID;
            Second : Extended_Token_Index_Type  := NONE_ID;
        end record;

        None_Token_Index_Pair : constant Token_Index_Pair_Type
          := (NONE_ID, NONE_ID);

        type Token_Type is record

            ID   : Token_Index_Type;
            Word : Unbounded_String;
            POS  : Unbounded_String;

            Morphology : Morphological_Description_Type;

        end record;
        -- A single token: Word & POS.

        type Token_Access is access Token_Type;

        procedure Free is new
          Ada.Unchecked_Deallocation (Token_Type, Token_Access);

        type Token_Access_Array is array (Index_Type range <>) of Token_Access;

        function Get_Token_ID(Token : in Token_Access)return Extended_Token_Index_Type is
          (if Token /= null then Token.ID else -1) with Inline;

        package Token_Vectors is new
          Ada.Containers.Vectors
            (Index_Type   => Index_Type,
             Element_Type => Token_Access);
        -- Vectors of Token_Type elements

        procedure Finalize
          (Token_Vector : in Token_Vectors.Vector);

        function Get_Token
          (Tokens   : Token_Vectors.Vector;
           Token_ID : Token_Index_Type)
           return Token_Vectors.Cursor is
          (Tokens.To_Cursor (Index_Type (Token_ID) - Index_Type (1)))
        with Inline;

        function Is_Valid_ID
          (Tokens   : Token_Vectors.Vector;
           Token_ID : Extended_Token_Index_Type) return Boolean is
          (Token_ID > -1) with Inline;

          --(Tokens.First_Index >= Extended_Index_Type(Token_ID) - Index_Type(1)
          -- and then Tokens.Last_Index <= Extended_Index_Type(Token_ID) - Index_Type(1)) with Inline;

        function Get_Token
          (Tokens   : Token_Vectors.Vector;
           Token_ID : Extended_Token_Index_Type)
           return Token_Access is
          (if Is_Valid_ID(Tokens, Token_ID) then Tokens.Element (Index_Type (Token_ID) - Index_Type (1)) else null)
        with Inline;

        package Token_Index_Vectors is new
          Ada.Containers.Vectors
            (Index_Type   => Index_Type,
             Element_Type => Extended_Token_Index_Type);
        -- Vectors of Token_Type elements

        function Hash (Key : Token_Index_Type) return Ada.Containers.Hash_Type is
          (Ada.Containers.Hash_Type (Key));

        package Token_Index_Sets is new
          Ada.Containers.Indefinite_Ordered_Sets
            (Element_Type => Token_Index_Type,
             "<"          => ">",
             "="          => "=");

        function Pop
          (Token_Vector : in out Token_Index_Vectors.Vector) return Token_Index_Type with Inline,
          Pre => not Token_Vector.Is_Empty;

        procedure Pop
          (Token_Vector : in out Token_Index_Vectors.Vector) with Inline,
          Pre => not Token_Vector.Is_Empty;

    end Tokens;

    ---
    -- Sentence
    ---

    type Sentence_Type is record
        Elements : Tokens.Token_Vectors.Vector;
    end record;
    -- A sentence (i.e. a vector of Tokens)

    package Sentence_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Sentence_Type);
    -- Vectors of Sentence_Type elements

    function Dist_Normalize (Dist : Integer) return Integer is
      (if Dist < 5 then Dist elsif Dist <= 10 then 5 else 6) with Inline;
    -- Distances between 5 and 10 are mapped to 5 and those greater than 10 are mapped to 6.

    use Tokens.Token_Vectors;

    function Token_Distance
      (Sentence : in Sentence_Type;
       A        : in Tokens.Token_Vectors.Cursor;
       B        : in Tokens.Token_Vectors.Cursor) return Natural is
      (if A /= No_Element and then B /= No_Element
       then abs (Natural (Sentence.Elements (A).ID) - Natural (Sentence.Elements (B).ID))
       else 0) with Inline;

end NLColl.Linguistic_Description.Sentences;
