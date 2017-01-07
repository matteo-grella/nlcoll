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

package NLColl.Transition_Systems.Arc_Relocate.State is

    Parser_State_Error : exception;

    package Actions is

        use NLColl.Linguistic_Description.Sentences.Tokens;

        type Action_Name_Type is
          (WRONG,
           SHIFT,
           WAIT,
           UNSHIFT,
           RELOCATE_LEFT,
           RELOCATE_RIGHT,
           ARC_LEFT,
           ARC_RIGHT,
           ROOT,
           NONE) with Default_Value => NONE;

        Max_Non_Projective_Distance : constant Natural := 5;

        type Distance_Type is new Natural range 0 .. Max_Non_Projective_Distance
          with Default_Value => 0;

        type Action_Base_Type is tagged
            record
                Name     : Action_Name_Type  := NONE;
                Distance : Distance_Type     := 0;
                Deprel   : Unbounded_String  := Null_Unbounded_String;
            end record;

        function Equal
          (A : in Action_Base_Type;
           B : in Action_Base_Type) return Boolean is
          (A.Name = B.Name and then A.Distance = B.Distance and then A.Deprel = B.Deprel)
        with Inline;

        function To_String
          (Action    : in Action_Base_Type) return String is
          (Action.Name'Img
           & ASCII.HT & ""
           & Action.Distance'Img
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
              Distance   => 0,
              Deprel     => Null_Unbounded_String);

        SHIFT_Action_Base : constant Action_Base_Type
          := (Name       => SHIFT,
              Distance   => 1,
              Deprel     => Null_Unbounded_String);

        WAIT_Action_Base : constant Action_Base_Type
          := (Name       => WAIT,
              Distance   => 1,
              Deprel     => Null_Unbounded_String);

        WRONG_Action_Base : constant Action_Base_Type
          := (Name       => WRONG,
              Distance   => 0,
              Deprel     => Null_Unbounded_String);

        package Action_Base_Type_Vectors is
          new Ada.Containers.Vectors
            (Index_Type   => Index_Type,
             Element_Type => Action_Base_Type);

        type Action_Type is new Action_Base_Type with
            record
                Governor    : Extended_Token_Index_Type := NONE_ID;
                Dependent   : Extended_Token_Index_Type := NONE_ID;
                Confidence  : Real                      := 0.0;
            end record;

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
              Distance       => 1,
              Deprel         => Null_Unbounded_String,
              Governor       => NONE_ID,
              Dependent      => NONE_ID,
              Confidence     => 0.0);

        Empty_Action : constant Action_Type
          := (Name           => NONE,
              Distance       => 0,
              Deprel         => Null_Unbounded_String,
              Governor       => NONE_ID,
              Dependent      => NONE_ID,
              Confidence     => 0.0);

    end Actions;

    use Actions;
    use NLColl.Linguistic_Description.Sentences.Tokens;

    pragma Assertion_Policy (Check);

    type State_Type is
        record
            Initialized       : Boolean := False;
            Buffer            : Token_Index_Vectors.Vector;
            Stack             : Token_Index_Vectors.Vector;
            Dependency_Tree   : Dependency_Tree_Type;
            Prev_Action       : Action_Type;
            Log_Prob          : Real := 0.0;
        end record;

    function Get_Next_Correct_Actions
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type_Vectors.Vector;

    function Get_Next_Correct_Action
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type) return Action_Type;

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
      (not State.Buffer.Is_Empty and then State.Stack.Is_Empty) with Inline;

    function Is_Allowed_Shift
      (State  : in State_Type) return Boolean is
      ((State.Buffer.Length > Count_Type (1) or else (not State.Buffer.Is_Empty and then State.Stack.Is_Empty))
       and then State.Prev_Action.Name not in UNSHIFT) with Inline;
    -- Returns True if a Shift action is allowed in the given parser state.

    function Is_Allowed_Unshift
      (State  : in State_Type;
       Times  : in Distance_Type) return Boolean is
      (State.Stack.Length > Count_Type (Times)
       and then State.Prev_Action.Name not in UNSHIFT) with Inline;
    -- Returns True if a Unshift action is allowed in the given parser state.
    -- TODO: not State.Buffer.Is_Empty ?

    function Is_Allowed_Unshift
      (State  : in State_Type) return Boolean is
      (Is_Allowed_Unshift(State, 1)) with Inline;

    function Is_Allowed_Wait
      (State  : in State_Type) return Boolean is
      (State.Buffer.Length > Count_Type (1) and then not State.Stack.Is_Empty
       and then State.Prev_Action.Name not in UNSHIFT) with Inline;
    -- Returns Wait if a Wait action is allowed in the given parser state.

    function Is_Allowed_Relocate
      (State    : in State_Type;
       Distance : in Distance_Type) return Boolean is
      (Distance >= 2
       and then not State.Buffer.Is_Empty
       and then State.Stack.Length >= Count_Type(Distance)
       and then State.Prev_Action.Name not in RELOCATE_LEFT | RELOCATE_RIGHT) with Inline;
    -- Returns True if a Relocate action is allowed in the given parser state.

    function Is_Allowed_Root
      (State  : in State_Type) return Boolean is
      (State.Stack.Length = Count_Type(1) and then State.Buffer.Is_Empty) with Inline;
    -- Returns True if a Root action is allowed in the given parser state.

    function Is_Allowed_Arc_Left
      (State    : in State_Type) return Boolean is
      (not State.Buffer.Is_Empty and then not State.Stack.Is_Empty) with Inline;
    -- Returns True if an Arc_Left action is allowed in the given parser state.

    function Is_Allowed_Arc_Right
      (State    : in State_Type) return Boolean is
      (not State.Buffer.Is_Empty and then not State.Stack.Is_Empty) with Inline;
    -- Returns True if an Arc_Right action is allowed in the given parser state.

    function Is_Allowed
      (State  : in State_Type;
       Action : in Action_Type) return Boolean is
      (case Action.Name is
           when SHIFT          => Is_Allowed_Shift (State),
           when UNSHIFT        => Is_Allowed_Unshift (State),
           when WAIT           => Is_Allowed_Wait (State),
           when ROOT           => Is_Allowed_Root (State),
           when ARC_LEFT       => Is_Allowed_Arc_Left (State),
           when ARC_RIGHT      => Is_Allowed_Arc_Right (State),
           when RELOCATE_LEFT  => Is_Allowed_Relocate (State, Action.Distance),
           when RELOCATE_RIGHT => Is_Allowed_Relocate (State, Action.Distance),
           when WRONG          => False,
           when NONE           => raise Parser_State_Error);
    -- Returns True if the action is allowed in the given parser state.

    function Get_Affected_Atoms
      (State     :  in State_Type;
       Action    :  in Action_Type) return Token_Index_Pair_Type is

      (case Action.Name is
           when ROOT           => (ROOT_ID, State.Stack.Last_Element),
           when ARC_LEFT       => (State.Buffer.Last_Element, State.Stack.Last_Element),
           when ARC_RIGHT      => (State.Stack.Last_Element, State.Buffer.Last_Element),
           when WAIT           => (State.Stack.Last_Element, State.Buffer.Last_Element),
           when RELOCATE_LEFT  => (State.Buffer.Last_Element,
                                   State.Stack.Element (State.Stack.Last_Index - Index_Type(Action.Distance) + 1)),
           when RELOCATE_RIGHT => (State.Stack.Element (State.Stack.Last_Index - Index_Type(Action.Distance) + 1),
                                   State.Buffer.Last_Element),

           when SHIFT | WRONG  | UNSHIFT => (NONE_ID, NONE_ID),
           when NONE           => raise Parser_State_Error);

    procedure Perform_Shift
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Shift (State);

    procedure Perform_Relocate
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Relocate (State, Action.Distance);

    procedure Perform_Wait
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Wait (State);

    procedure Perform_Unshift
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Unshift (State, Action.Distance);

    procedure Perform_Root
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Root (State) and then
      State.Stack.Last_Element = Action.Dependent;

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

    ---
    -- Context
    ---

    package State_Context is

        use Token_Vectors;

        type Token_Access_Array is array (Index_Type range <>) of Token_Access;

        type Context_Type is
            record
                Stack    : Token_Access_Array (0 .. 3) := (others => null);
                Buffer   : Token_Access_Array (0 .. 3) := (others => null);
            end record;

        procedure Get_Context
          (Context  : in out Context_Type;
           State    : in     State_Type;
           Sentence : in     Sentence_Type) with
          Pre => not (State.Stack.Is_Empty and then State.Buffer.Is_Empty);

        procedure Print_Context
          (Context : in Context_Type);

    end State_Context;

end NLColl.Transition_Systems.Arc_Relocate.State;
