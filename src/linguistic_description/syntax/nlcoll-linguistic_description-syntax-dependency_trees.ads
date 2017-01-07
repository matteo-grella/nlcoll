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

with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
use type Ada.Containers.Count_Type;

with Ada.Finalization;
with ARColl.Numerics.Reals; 
with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;

with NLColl.Linguistic_Description.Sentences; use NLColl.Linguistic_Description.Sentences;

package NLColl.Linguistic_Description.Syntax.Dependency_Trees is

    use NLColl.Linguistic_Description.Sentences.Tokens;
    
    --pragma Assertion_Policy (Check);
    
    UNKNOWN_DEPREL : constant Unbounded_String 
      := To_Unbounded_String("__UNKNOWN__");
    
    NULL_DEPREL : constant Unbounded_String 
      := To_Unbounded_String("__NULL__");
    
    package Head_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Token_Index_Type,
         Element_Type => Extended_Token_Index_Type);

    package Deprel_Vectors is new
      Ada.Containers.Indefinite_Vectors
        (Index_Type   => Token_Index_Type,
         Element_Type => Unbounded_String);

    package POS_Vectors is new
      Ada.Containers.Indefinite_Vectors
        (Index_Type   => Token_Index_Type,
         Element_Type => Unbounded_String);
    
    type Dependents_Structure_Type is record
        Left  : Token_Index_Sets.Set;   
        Right : Token_Index_Sets.Set;
    end record;
    
    Empty_Dependents_Structure : constant Dependents_Structure_Type
      := (Left  => Token_Index_Sets.Empty_Set,
          Right => Token_Index_Sets.Empty_Set);
    
    package Dependents_Vectors is new
      Ada.Containers.Indefinite_Vectors
        (Index_Type   => Token_Index_Type,
         Element_Type => Dependents_Structure_Type);
    
    type Dependency_Tree_Type is new Ada.Finalization.Controlled with record
        Heads      : Head_Vectors.Vector;
        Deprels    : Deprel_Vectors.Vector;
        Dependents : Dependents_Vectors.Vector;
    end record;
    -- Represents a partial or complete dependency parse of a sentence,
    -- and provides convenience methods for analyzing the parse.
    
    overriding procedure Initialize
      (Dependency_Tree : in out Dependency_Tree_Type)
      with Inline;

    overriding procedure Finalize
      (Dependency_Tree : in out Dependency_Tree_Type)
      with Inline;

    procedure Clear
      (Dependency_Tree : in out Dependency_Tree_Type)
      with Inline;
    -- Removes all contents from the tree (laeves only root)

    procedure Set_Arc
      (Dependency_Tree : in out Dependency_Tree_Type;
       Governor        : in     Extended_Token_Index_Type;
       Dependent       : in     Token_Index_Type;
       Deprel          : in     Unbounded_String) with Inline,
      Pre => Dependency_Tree.Heads.Length = Dependency_Tree.Deprels.Length
      and then (Dependent > Dependency_Tree.Heads.Last_Index 
                or else Dependency_Tree.Heads.Element(Dependent) = -1);
    --
    -- Establish a labeled dependency relation between the two given tokens.
    --
    
    procedure Set_Dependent
      (Dependency_Tree       : in out Dependency_Tree_Type;
       Token_Index           : in     Extended_Token_Index_Type;
       Dependent_Token_Index : in     Token_Index_Type) with
      Pre => Token_Index <= Dependency_Tree.Dependents.Last_Index
      and then Dependent_Token_Index <= Dependency_Tree.Dependents.Last_Index;
       
    function Is_Resolved
      (ID                     : in Token_Index_Type;
       Dependency_Tree        : in Dependency_Tree_Type;
       Gold_Dependency_Tree   : in Dependency_Tree_Type) return Boolean;
        
    function Get_Tokens_Count
      (Dependency_Tree : Dependency_Tree_Type) return Token_Index_Type is
      (Dependency_Tree.Heads.Last_Index) with Inline;
    
    function Get_Head
      (Dependency_Tree : Dependency_Tree_Type;
       Token_ID        : Token_Index_Type) return Extended_Token_Index_Type is
      (Dependency_Tree.Heads.Element (Token_ID)) with Inline,
    Pre => Token_ID <= Dependency_Tree.Heads.Last_Index;

    function Get_Deprel
      (Dependency_Tree : Dependency_Tree_Type;
       Token_ID        : Token_Index_Type) return Unbounded_String is
      (Dependency_Tree.Deprels.Element (Token_ID)) with Inline, 
    Pre => Token_ID <= Dependency_Tree.Heads.Last_Index;
    
    function Get_Number_Of_Dependent
      (Dependency_Tree : Dependency_Tree_Type;
       Token_ID        : Token_Index_Type) return Natural is
      ((Natural (Dependency_Tree.Dependents.Element (Token_ID).Left.Length)
        + Natural (Dependency_Tree.Dependents.Element (Token_ID).Right.Length))) with Inline,
    Pre => Token_ID <= Dependency_Tree.Dependents.Last_Index;

    function Get_Left_Dependent
      (Token           : in Token_Access;
       Position        : in Positive;
       Sentence        : in Sentence_Type;
       Dependency_Tree : in Dependency_Tree_Type) return Token_Access;
    
    function Get_Right_Dependent
      (Token                : in Token_Access;
       Position             : in Positive;
       Sentence             : in Sentence_Type;
       Dependency_Tree      : in Dependency_Tree_Type) return Token_Access;
    
    function Has_Dependent_With_Deprel
      (Dependency_Tree : in Dependency_Tree_Type;
       Token           : in Token_Index_Type;
       Deprel          : in String) return Boolean;
        
    function Get_Top
      (Dependency_Tree : Dependency_Tree_Type)
       return Extended_Token_Index_Type with
      Pre => (for all Head of Dependency_Tree.Heads => Head /= -1);
    -- Get the index of the token which is the root of the parse
    -- (i.e., that token which has the ROOT token as its head).

    function Get_Governors
      (Dependency_Tree    : in Dependency_Tree_Type;
       Token_Index_Vector : in Token_Index_Vectors.Vector) return Token_Index_Vectors.Vector;
    
    function Is_Single_Top
      (Dependency_Tree : Dependency_Tree_Type)
       return Boolean with Inline,
      Pre => (for all Head of Dependency_Tree.Heads => Head /= -1);
    -- Check if this parse has only one root.

    function Is_Tree
      (Dependency_Tree : in Dependency_Tree_Type)
       return Boolean with Inline;
    -- Check if the tree is legal, O(n)

    function Is_Projective
      (Dependency_Tree : Dependency_Tree_Type)
       return Boolean with Inline,
      Pre => (for all Head of Dependency_Tree.Heads => Head /= -1);
    -- Check if the tree is projective, O(n^2)

    package Dependency_Tree_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Dependency_Tree_Type);
    -- Vectors of Dependency_Tree_Type elements

    package Statistics is
        
        use ARColl.Numerics.Reals;
        
        type Dependency_Trees_Stats_Type is record
            Total          : Natural := 0;
            Non_Tree       : Natural := 0;
            Non_Projective : Natural := 0;
            Multi_Top      : Natural := 0;
        end record;

        function Get_Dependency_Trees_Stats
          (Dependency_Trees : in  Dependency_Tree_Vectors.Vector) 
           return Dependency_Trees_Stats_Type;
        
        procedure Print_Dependency_Trees_Stats
          (Dependency_Trees_Display_Name : in String;
           Dependency_Trees_Stats        : in Dependency_Trees_Stats_Type);

        type Evaluation_Type is record
            Iter         : Positive := 1;

            UAS          : Real := -1.0;
            UAS_No_Punct : Real := -1.0;
            LAS          : Real := -1.0;
            LAS_No_Punct : Real := -1.0;

            UEM          : Real := -1.0;
            UEM_No_Punct : Real := -1.0;

            ROOT         : Real := -1.0;
        end record;
        -- Evaluation metrics

        package Evaluation_Vectors is
          new Ada.Containers.Vectors
            (Index_Type   => Natural,
             Element_Type => Evaluation_Type);

        procedure Evaluate_Dependency_Tree
          (Sentences      : in  Sentence_Vectors.Vector;
           Trees          : in  Dependency_Tree_Vectors.Vector;
           Gold_Trees     : in  Dependency_Tree_Vectors.Vector;
           Evaluation     : out Evaluation_Type);

        procedure Print_Evaluation
          (Evaluation : in Evaluation_Type);

        procedure Print_Evaluations
          (Evaluations : in Evaluation_Vectors.Vector);
        
    end Statistics;

end NLColl.Linguistic_Description.Syntax.Dependency_Trees;
