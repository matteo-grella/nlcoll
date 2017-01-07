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

with Ada.Containers;
use Ada.Containers;
with Ada.Containers.Vectors;

with ARColl; use ARColl;
with ARColl.Numerics.Reals; use ARColl.Numerics.Reals;
with ARColl.Strings; use ARColl.Strings;
with ARColl.Strings.Unbounded; use ARColl.Strings.Unbounded;

with NLColl.Linguistic_Description.Sentences; use NLColl.Linguistic_Description.Sentences;
with NLColl.Linguistic_Description.Syntax.Dependency_Trees; use NLColl.Linguistic_Description.Syntax.Dependency_Trees;

with ARColl.Containers.Bimaps.Generic_ID_Bimaps;
with Ada.Strings.Hash;

package NLColl.Transition_Systems.Arc_Spine.State is

    Parser_State_Error : exception;

    package Actions is

        use NLColl.Linguistic_Description.Sentences.Tokens;

        type Action_Name_Type is
          (SHIFT,
           ARC_LEFT,
           ARC_RIGHT,
           ROOT,
           NONE) with Default_Value => NONE;

        subtype Spine_Index_Type is Index_Type;

        type Spine_Direction_Type is (LEFT, RIGHT);

        type Transition_Type is
           record
               Name        : Action_Name_Type          := NONE;
               K           : Spine_Index_Type          := 0;
               Governor    : Extended_Token_Index_Type := NONE_ID;
               Dependent   : Extended_Token_Index_Type := NONE_ID;
           end record;

        package Transition_Type_Vectors is
          new Ada.Containers.Vectors
            (Index_Type   => Index_Type,
             Element_Type => Transition_Type);

        type Action_Base_Type is tagged
           record
               Name     : Action_Name_Type  := NONE;
               Deprel   : Unbounded_String  := Null_Unbounded_String;
           end record;

        function Equal
          (A : in Action_Base_Type;
           B : in Action_Base_Type) return Boolean is
          (A.Name = B.Name and then A.Deprel = B.Deprel)
        with Inline;

        function To_String
          (Action    : in Action_Base_Type) return String is
          (Action.Name'Img
           & ASCII.HT & ""
           & To_String (Action.Deprel)) with Inline;

        function Is_Arc_Action
          (Action : in Action_Base_Type) return Boolean is
          (Action.Name in ARC_LEFT | ARC_RIGHT) with Inline;

        function Hash (Action : in Action_Base_Type) return Ada.Containers.Hash_Type is
          (Ada.Strings.Hash(To_String(Action))) with Inline;

        package Action_Id_Bimaps is new
          ARColl.Containers.Bimaps.Generic_ID_Bimaps
            (ID_Type      => Index_Type,
             Element_Type => Action_Base_Type,
             Hash         => Hash);

        Empty_Action_Base : constant Action_Base_Type
          := (Name       => NONE,
              Deprel     => Null_Unbounded_String);

        SHIFT_Action_Base : constant Action_Base_Type
          := (Name       => SHIFT,
              Deprel     => Null_Unbounded_String);

        package Action_Base_Type_Vectors is
          new Ada.Containers.Vectors
            (Index_Type   => Index_Type,
             Element_Type => Action_Base_Type);

        type Action_Type is new Action_Base_Type with
           record
               K           : Spine_Index_Type          := 0;
               Governor    : Extended_Token_Index_Type := NONE_ID;
               Dependent   : Extended_Token_Index_Type := NONE_ID;
               Confidence  : Real                      := 0.0;
           end record;

        function To_String
          (Action : Action_Type) return String is
            (Action.Confidence'Img
             & ASCII.HT & ""
             & Action.Name'Img
             & ASCII.HT & ""
             & Action.K'Img
             & ASCII.HT & ""
             & "G" & Action.Governor'Img
             & ASCII.HT & ""
             & "D" & Action.Dependent'Img
             & ASCII.HT & ""
             & To_String (Action.Deprel));

        type Confidence_Label_Type is
          (HIGH,
           MEDIUM,
           LOW,
           VERY_LOW);

        function Get_Confidence
          (Action : in Action_Type) return Confidence_Label_Type is
          ((if Action.Confidence > 0.999
            then HIGH
            elsif Action.Confidence > 0.7
            then MEDIUM
            elsif Action.Confidence > 0.1
            then LOW
            else VERY_LOW));

        package Action_Type_Vectors is
          new Ada.Containers.Vectors
            (Index_Type   => Index_Type,
             Element_Type => Action_Type);

        SHIFT_Action : constant Action_Type
          := (Name           => SHIFT,
              K              => 0,
              Deprel         => Null_Unbounded_String,
              Governor       => NONE_ID,
              Dependent      => NONE_ID,
              Confidence     => 0.0);

        Empty_Action : constant Action_Type
          := (Name           => NONE,
              K              => 0,
              Deprel         => Null_Unbounded_String,
              Governor       => NONE_ID,
              Dependent      => NONE_ID,
              Confidence     => 0.0);

    end Actions;

    use Actions;
    use NLColl.Linguistic_Description.Sentences.Tokens;

    pragma Assertion_Policy (Check);

    -----
    -- Tree Tokens
    -----

    type Spine_Type is array (Spine_Direction_Type) of Token_Index_Vectors.Vector;

    type Tree_Token_Type is record
        Head_Token  : Token_Index_Type;
        Spine       : Spine_Type;
    end record;
    type Tree_Token_Access is access Tree_Token_Type;

    package Tree_Token_Vectors is
      new Ada.Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Tree_Token_Type);

    function Pop
      (Tree_Token_Vector : in out Tree_Token_Vectors.Vector) return Tree_Token_Type with Inline;

    function Make_Tree_Token
      (Token_ID : Token_Index_Type) return Tree_Token_Type;

    function Get_Head_Tokens
      (Tree_Token_Vector : in Tree_Token_Vectors.Vector) return Token_Index_Vectors.Vector;

    procedure Print_Tree_Token
      (Tree_Token      : in Tree_Token_Type;
       Print_New_Line  : in Boolean := False);

    procedure Print_Tree_Token_Vector
      (Tree_Token_Vector : in Tree_Token_Vectors.Vector;
       Print_Reverse     : in Boolean := False;
       Print_New_Line    : in Boolean := False);

    type State_Type is
       record
           Initialized       : Boolean := False;
           Buffer            : Tree_Token_Vectors.Vector;
           Stack             : Tree_Token_Vectors.Vector;
           Dependency_Tree   : Dependency_Tree_Type;
           Log_Prob          : Real := 0.0;
       end record;

    function Get_Next_Possible_Transitions
      (State                : in State_Type) return Transition_Type_Vectors.Vector;

    function Is_Correct
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type;
       Transition           : in Transition_Type;
       Flexibility          : in Boolean := False) return Boolean;

    procedure State_Initialize
      (Sentence : in Sentence_Type;
       State    : in out State_Type) with
      Pre => not State.Initialized;

    function State_Initialize
      (Sentence : in Sentence_Type) return State_Type;

    procedure State_Finalize
      (State : in out State_Type) with
      Pre => State.Initialized;

    procedure Print_State
      (State : in State_Type) with
      Pre => State.Initialized;

    function Is_Final_State
      (State : in State_Type) return Boolean is
      (State.Stack.Is_Empty and then State.Buffer.Is_Empty) with Inline;

    function Is_Second_To_Last_State
      (State : in State_Type) return Boolean is
      (State.Stack.Length = Count_Type(1) and then State.Buffer.Is_Empty) with Inline;

    function Is_Deterministic_State
      (State : in State_Type) return Boolean is
      (not State.Buffer.Is_Empty and then State.Stack.Length < Count_Type(2)) with Inline;

    function Is_Allowed_Shift
      (State  : in State_Type) return Boolean is
      (not State.Buffer.Is_Empty) with Inline;
    -- Returns True if a Shift action is allowed in the given parser state.

    function Is_Allowed_Root
      (State  : in State_Type) return Boolean is
      (State.Stack.Length = Count_Type(1) and then State.Buffer.Is_Empty) with Inline;
    -- Returns True if a Root action is allowed in the given parser state.

    function Is_Allowed_Arc_Left
      (State    : in State_Type) return Boolean is
      (State.Stack.Length > Count_Type(1)) with Inline;
    -- Returns True if an Arc_Left action is allowed in the given parser state.

    function Is_Allowed_Arc_Right
      (State    : in State_Type) return Boolean is
      (State.Stack.Length > Count_Type(1)) with Inline;
    -- Returns True if an Arc_Right action is allowed in the given parser state.

    function Is_Allowed
      (State  : in State_Type;
       Action : in Action_Type) return Boolean is
      (case Action.Name is
           when SHIFT          => Is_Allowed_Shift (State),
           when ROOT           => Is_Allowed_Root (State),
           when ARC_LEFT       => Is_Allowed_Arc_Left (State),
           when ARC_RIGHT      => Is_Allowed_Arc_Right (State),
           when NONE           => raise Parser_State_Error);
    -- Returns True if the action is allowed in the given parser state.

    function Second_To_Last
      (Vector : in Tree_Token_Vectors.Vector)
       return Tree_Token_Vectors.Constant_Reference_Type is
      (Vector.Constant_Reference(Vector.Last_Index - 1)) with Inline,
    Pre => Vector.Length > 1;

    function Get_Affected_Atoms
      (State       :  in State_Type;
       Transition  :  in Transition_Type) return Token_Index_Pair_Type is

      (case Transition.Name is
           when ROOT
             => (ROOT_ID, State.Stack.Last_Element.Head_Token),
           when ARC_LEFT
             => (State.Stack.Last_Element.Spine(LEFT).Element(Transition.K),
                 Second_To_Last(State.Stack).Head_Token),
           when ARC_RIGHT
             => (Second_To_Last(State.Stack).Spine(RIGHT).Element(Transition.K),
                 State.Stack.Last_Element.Head_Token),
           when SHIFT
             => (NONE_ID, NONE_ID),
           when NONE
             => raise Parser_State_Error);

    procedure Perform_Shift
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Shift (State);

    procedure Perform_Shift
      (State  : in out State_Type) with Inline,
      Pre => Is_Allowed_Shift (State);

    procedure Perform_Root
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Root (State) and then
      State.Stack.Last_Element.Head_Token = Action.Dependent;

    procedure Perform_Root
      (State  : in out State_Type) with Inline,
      Pre => Is_Allowed_Root (State);

    procedure Perform_Arc_Left
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Arc_Left (State);

    procedure Perform_Arc_Right
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Arc_Right (State);

    procedure Perform_Action
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Action.Name /= NONE;

    function Get_Token_From_State
      (Sentence          : in Sentence_Type;
       Tree_Token_Vector : in Tree_Token_Vectors.Vector;
       Index             : in Integer;
       Spine_Direction   : in Spine_Direction_Type;
       K                 : in Index_Type) return Token_Access;

    ---
    -- Context
    ---

    package State_Context is

        use Token_Vectors;

        type Token_Access_Array is array (Index_Type range <>) of Token_Access;

        type Context_Type is
           record
               Stack             : Token_Access_Array (0 .. 3) := (others => null);
               Buffer            : Token_Access_Array (0 .. 3) := (others => null);
               Grandparent       : Token_Access := null;
               Great_Grandparent : Token_Access := null;
               Governor          : Token_Access := null;
               Dependent         : Token_Access := null;
           end record;

        procedure Get_Context
          (Context          : in out Context_Type;
           State            : in     State_Type;
           Transition       : in     Transition_Type;
           Sentence         : in     Sentence_Type) with
          Pre => not (State.Stack.Is_Empty);

        procedure Print_Context
          (Context : in Context_Type);
    end State_Context;

end NLColl.Transition_Systems.Arc_Spine.State;
