------------------------------------------------------------------------------
--                               N L C O L L
--  N a t u r a l   L a n g u a g e   C o m p o n e n t   C o l l e c t i o n
--
--                       Copyright 2014 M. Grella
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

package NLColl.Transition_Systems.Easy_First.State is

    Parser_State_Error : exception;

    use NLColl.Linguistic_Description.Sentences.Tokens;

    -----
    -- Tree Tokens
    -----

    subtype Spine_Index_Type is Index_Type;

    type Spine_Direction_Type is (LEFT, RIGHT);

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

    -----
    -- Actions
    -----

    package Actions is

        type Action_Name_Type is
          (ARC_LEFT,
           ARC_RIGHT,
           ROOT,
           NONE) with Default_Value => NONE;

        type Action_Type is
           record
               Name        : Action_Name_Type          := NONE;
               I           : Extended_Index_Type       := -1;
               K           : Spine_Index_Type          := 0;
               Governor    : Extended_Token_Index_Type := NONE_ID;
               Dependent   : Extended_Token_Index_Type := NONE_ID;
               Score       : Real;
               Deprel      : Unbounded_String;
           end record;

        package Action_Type_Vectors is
          new Ada.Containers.Vectors
            (Index_Type   => Index_Type,
             Element_Type => Action_Type);

        function To_String
          (Action : Action_Type) return String is
            (Action.Name'Img
             & ASCII.HT & ""
             & Action.K'Img
             & ASCII.HT & ""
             & "G" & Action.Governor'Img
             & ASCII.HT & ""
             & "D" & Action.Dependent'Img
             & ASCII.HT & ""
             & "(" & Action.Score'Img & ")");

    end Actions;

    use Actions;

    pragma Assertion_Policy (Check);

    function Extract
      (Tree_Token_Vector : in out Tree_Token_Vectors.Vector;
       Index             : in     Index_Type) return Tree_Token_Type with Inline;

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
           Pending           : Tree_Token_Vectors.Vector;
           Dependency_Tree   : Dependency_Tree_Type;
           Log_Prob          : Real := 0.0;
       end record;

    function Get_Possible_Actions
      (State                : in State_Type) return Action_Type_Vectors.Vector;

    function Is_Correct
      (State                : in State_Type;
       Gold_Dependency_Tree : in Dependency_Tree_Type;
       Action               : in Action_Type;
       Top_Down_Strategy    : in Boolean := False) return Boolean with
      Pre => not State.Pending.Is_Empty
      and then ((State.Pending.Length = Count_Type(1) and then Action.Name = ROOT)
                or else (State.Pending.Length > Count_Type(1) and then Action.Name in ARC_LEFT | ARC_RIGHT));

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
      (State.Pending.Is_Empty) with Inline;

    function Is_Second_To_Last_State
      (State : in State_Type) return Boolean is
      (State.Pending.Length = Count_Type(1)) with Inline;

    function Is_Allowed_Root
      (State  : in State_Type) return Boolean is
      (State.Pending.Length = Count_Type(1)) with Inline;
    -- Returns True if a Root action is allowed in the given parser state.

    function Is_Allowed_Arc_Left
      (State    : in State_Type) return Boolean is
      (State.Pending.Length > Count_Type(1)) with Inline;
    -- Returns True if an Arc_Left action is allowed in the given parser state.

    function Is_Allowed_Arc_Right
      (State    : in State_Type) return Boolean is
      (State.Pending.Length > Count_Type(1)) with Inline;
    -- Returns True if an Arc_Right action is allowed in the given parser state.

    function Is_Allowed
      (State  : in State_Type;
       Action : in Action_Type) return Boolean is
      (case Action.Name is
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

--      function Get_Affected_Atoms
--        (State       :  in State_Type;
--         Transition  :  in Transition_Type) return Token_Index_Pair_Type is
--
--        (case Transition.Name is
--             when ROOT
--               => (ROOT_ID, State.Pending.Last_Element.Head_Token),
--             when ARC_LEFT
--               => (State.Stack.Last_Element.Spine(LEFT).Element(Transition.K),
--                   Second_To_Last(State.Stack).Head_Token),
--             when ARC_RIGHT
--               => (Second_To_Last(State.Stack).Spine(RIGHT).Element(Transition.K),
--                   State.Stack.Last_Element.Head_Token),
--             when NONE
--               => raise Parser_State_Error);

    procedure Perform_Root
      (State  : in out State_Type;
       Action : in     Action_Type) with Inline,
      Pre => Is_Allowed_Root (State) and then
      State.Pending.Last_Element.Head_Token = Action.Dependent;

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

        type Context_Type is
           record
               Grandparent       : Token_Access := null;
               Great_Grandparent : Token_Access := null;
               Governor          : Token_Access := null;
               Dependent         : Token_Access := null;
           end record;

        procedure Get_Context
          (Context          : in out Context_Type;
           State            : in     State_Type;
           Action           : in     Action_Type;
           Sentence         : in     Sentence_Type) with
          Pre => not (State.Pending.Is_Empty);

        procedure Print_Context
          (Context : in Context_Type);

    end State_Context;

end NLColl.Transition_Systems.Easy_First.State;
