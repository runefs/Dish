module Role
open System.Dynamic
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System 
open ImpromptuInterface

let mb = 
    let aName = AssemblyName("dish_runtim")
    let assemblyBuilder = 
        AssemblyBuilder.DefineDynamicAssembly(
            aName,
            AssemblyBuilderAccess.Run
        )
    assemblyBuilder.DefineDynamicModule("DishModule");


let defineType name = 
    mb.DefineType(
        name,
        TypeAttributes.Public
    )

type private RoleMetaObject<'playerType,'roleType>(expr : Expression, roleContainer : Role<'playerType>) = 
    inherit DynamicMetaObject(expr, BindingRestrictions.Empty, roleContainer)
    let playerType = typeof<'playerType>
    let roleType = roleContainer.RoleTypeObject.GetType()
    override this.BindSetMember(binder,value) =
        let restrictions =
             BindingRestrictions.GetTypeRestriction(this.Expression, this.LimitType)
        
        let propertyToSet = Expression.Property(roleContainer.PlayerExpression, binder.Name)
        let t = (propertyToSet.Member :?> System.Reflection.PropertyInfo).PropertyType
        let convert = Expression.Convert(value.Expression, t)
        let assignment = Expression.Assign(propertyToSet, convert)
        let returnVal = Expression.Convert(assignment, typeof<obj>)
        // Create a meta object to invoke Set later:
        DynamicMetaObject(returnVal,restrictions)
    override this.BindConvert(binder) =
        let restrictions = BindingRestrictions.GetTypeRestriction(this.Expression, this.LimitType);
        //if the type requested is compatible with the 
        //instance, there's no conversion to be done.
        
        binder.FallbackConvert(DynamicMetaObject(this.Expression, restrictions, this.Value))
        
    override this.BindGetMember(binder) =
        
        let restrictions =
             BindingRestrictions.GetTypeRestriction(this.Expression, this.LimitType)
        let property = Expression.Property(roleContainer.PlayerExpression, binder.Name)
        let returnVal = Expression.Convert(property, typeof<obj>)
        DynamicMetaObject(returnVal,restrictions)

    override this.BindInvokeMember(binder,args) =
        
        let method = roleType.GetMethod(binder.Name)
        let method = 
            if method |> isNull || method.IsAbstract then
                let methods = 
                    //doesn't work because the interfaces are not part of the newly created type
                    roleType.GetInterfaces()
                    |> Array.collect(fun t -> t.GetMethods())
                methods
                |> Array.filter(fun m -> m.Name = binder.Name)
                |> Array.tryHead
            else Some method
        let restrictions =
            BindingRestrictions.GetTypeRestriction(this.Expression, this.LimitType)
        let this,method = 
            match method with
            Some method ->
                roleContainer.RoleTypeObjectExpression,method
            | None ->
                roleContainer.PlayerExpression,playerType.GetMethod(binder.Name)
        assert(method |> isNull |> not)
        let args = 
            args
            |> Array.zip (method.GetParameters())
            |> Array.map(fun (p,arg) -> Expression.Convert(arg.Expression, p.ParameterType) :> Expression) 
        let call = Expression.Call(this,method,args)
        let returnVal = 
            if method.ReturnType = typeof<System.Void> then
                Expression.Block(
                    call,
                    Expression.Default(typeof<obj>)
                ) :> Expression
            else
                Expression.Convert(call, typeof<obj>) :> Expression
        DynamicMetaObject(returnVal,restrictions)
        
and Role<'playerType>(player : 'playerType, roleTypeObject : obj) as this = 
    [<DefaultValue>] val mutable private _player : 'playerType
    [<DefaultValue>] val mutable private _role : obj
    
    let playerExpr = 
        Expression.Field(Expression.Constant this, "_player") :> Expression
    let roleTypeObjectExpr = 
        Expression.Convert(Expression.Field(Expression.Constant this, "_role"),roleTypeObject.GetType()) :> Expression
    member internal __.PlayerExpression with get() = playerExpr
    member internal __.RoleTypeObjectExpression with get() = roleTypeObjectExpr
    member internal __.RoleTypeObject with get() = roleTypeObject
    interface IDynamicMetaObjectProvider with

      member this.GetMetaObject expr = 
          RoleMetaObject(expr, this) :> DynamicMetaObject
    do
       this._player <- player
       this._role <- roleTypeObject
type private Local =
    Address of int16
    | ShortAddress of byte
    | Builder of LocalBuilder
    | Ordinal of int16
type private This = 
    RoleSelf
    | Player
type private Value = 
      Byte of OpCode * byte
    | SByte of OpCode * sbyte
    | Int16 of OpCode * int16
    | Int of OpCode * int
    | Single of OpCode * single
    | Int64 of OpCode * int64
    | Float of OpCode * float
    | String of string
    | Local of Local
    | Field of OpCode * FieldInfo
    | This of This
type private Unary = 
    | StoreField of OpCode * FieldInfo
    | StoreLocal of Local
type private Binary =
    | Equal
    | Subtraction

type private Nnary = 
   MethodCall of OpCode * MethodInfo
   | CtorCall of OpCode * ConstructorInfo
type private Instruction =
    Simple of OpCode
    | Nnary of operandCount: int * Nnary
    | TypeOperation of OpCode * Type
    | Value of Value
    | Switch of Label []
    | Unary of Unary
    | Comparison of Binary
    | Computation of Binary
type private Operation =
   Leaf of Instruction
   | Operation of Instruction * Operation list
   | Nop
let isPlayerType (playerInterface : System.Type) (declaringType : System.Type) =
    declaringType.IsAssignableTo playerInterface
    
let private rewriteThis (playerType : System.Type) roleType instructions = 
    let rec usePlayer inst =
        match inst with
        Leaf(Value(This(RoleSelf))) -> Leaf(Value(This(Player)))
        | Operation(i,operands) -> 
            let i,operands = 
                match i with 
                Value(This(RoleSelf)) -> Value(This(Player)),operands |> List.map usePlayer
                | Nnary(count,MethodCall(opc,mi)) ->
                    
                    let mi,operands = 
                        if isPlayerType playerType mi.DeclaringType then
                            
                            let operands = 
                                operands
                                |> List.map usePlayer
                            playerType.GetMethod(mi.Name),operands
                        else
                            let instance = 
                                operands |> List.head
                            let arguments = 
                                operands
                                |> List.tail
                                |> List.map usePlayer
                            mi,instance::arguments
                    //assert(count = operands.Length)
                    Nnary(count,MethodCall(opc,mi)),operands
                | i -> i,operands |> List.map usePlayer
            Operation(i,operands)
        | op -> op
    
    let operations =
        let stack =  
            instructions
            |> List.ofSeq
            |> List.fold(fun (stack : Operation list) current ->
                let pop n =
                    let operands = 
                       stack
                       |> List.take n
                       |> List.rev
                    let stack =
                       stack
                       |> List.skip n
                    Operation(current,operands)::stack
                match current with
                Simple _  
                | Value _ -> (Leaf current)::stack
                | Nnary(c,_) -> 
                    pop c
                | Unary _ ->
                    pop 1
                | Comparison _
                | Computation _ -> 
                    pop 2
                | TypeOperation _ 
                | Switch _ -> failwith "Not implemented"
            ) []
        
        let operations = stack|> List.rev
        operations
        |> List.map usePlayer
    
    let rec flattenTree tree res =
        match tree with
        Leaf(i)::tree -> flattenTree tree (i::res)
        | Nop::tree-> flattenTree tree res
        | Operation(i,operands)::tree ->
            let operands = flattenTree operands []
            flattenTree tree (i::(operands@res))
        | [] -> res
    flattenTree operations []
    |> List.rev

let createRole<'final, 'a, 'player when 'final : not struct> (player : 'player) = 
   
    let roleType = typeof<'a>
    let playerType = typeof<'player>
    let createConstructor (field : FieldInfo) (t : TypeBuilder) = 
        let constructorArgs = [| typeof<'player> |]
        let ctor =
           t.DefineConstructor(MethodAttributes.Public,
                               CallingConventions.Standard, constructorArgs)
        ctor.DefineParameter(0,ParameterAttributes.None,"player") |> ignore
        let ilg = ctor.GetILGenerator()
        let objCtor = typeof<obj>.GetConstructor([||]);
        ilg.Emit(OpCodes.Ldarg_0)
        ilg.Emit(OpCodes.Call, objCtor)
        ilg.Emit(OpCodes.Ldarg_0)
        ilg.Emit(OpCodes.Ldarg_1)
        ilg.Emit(OpCodes.Stfld, field)
        ilg.Emit(OpCodes.Ret)
    let t = typeof<'a>
    let props = t.GetProperties()
    let copyType = defineType (t.Name + ".Dish")
    //copyType.AddInterfaceImplementation(typeof<'final>)
    let playerField = copyType.DefineField("__player__",typeof<'player>,FieldAttributes.Private)
    
    createConstructor playerField copyType
    let propBuilders = 
        props
        |> Array.map(fun p -> 
            copyType.DefineProperty(p.Name,p.Attributes,p.PropertyType,[||])
        )
    let readInstructions (instructions : seq<Mono.Reflection.Instruction>) = 
        instructions
        |> Seq.map(fun inst ->
            printfn "Inst: %A" inst.OpCode
            match inst.OpCode with
            | op when ((op = OpCodes.Call 
                       || op = OpCodes.Callvirt 
                       || op = OpCodes.Ldtoken
                       || op = OpCodes.Ldvirtftn
                       || op = OpCodes.Ldftn) && inst.Operand :? MethodInfo) ->
                let mi = inst.Operand :?> MethodInfo
                let argCount = 
                    if mi.IsStatic then 
                        mi.GetParameters().Length
                    else
                        mi.GetParameters().Length + 1
                Nnary(argCount,MethodCall(op,mi))
            | op when ((op = OpCodes.Call 
                       || op = OpCodes.Callvirt
                       || op = OpCodes.Newobj) && inst.Operand :? ConstructorInfo) ->
                let ci = inst.Operand :?> ConstructorInfo
                Nnary(ci.GetParameters().Length,CtorCall(op,ci))
            | op when op = OpCodes.Calli -> failwith "Calli not supported"
            | op when (op = OpCodes.Box
                       || op = OpCodes.Castclass 
                       || op = OpCodes.Cpobj
                       || op = OpCodes.Initobj
                       || op = OpCodes.Isinst
                       || op = OpCodes.Ldelem
                       || op = OpCodes.Ldelema
                       || op = OpCodes.Ldobj
                       || op = OpCodes.Ldtoken
                       || op = OpCodes.Mkrefany
                       || op = OpCodes.Newarr
                       || op = OpCodes.Refanyval
                       || op = OpCodes.Sizeof
                       || op = OpCodes.Stelem
                       || op = OpCodes.Stobj
                       || op = OpCodes.Unbox
                       || op = OpCodes.Unbox_Any) ->
                TypeOperation(op,inst.Operand :?> System.Type)
            | op when (op = OpCodes.Ldarga
                       || op = OpCodes.Starg) ->
                Value(Int16(op,inst.Operand |> unbox |> int16))
            | op when (op = OpCodes.Ldarga_S
                       || op = OpCodes.Starg_S) ->
                Value(Byte(op,inst.Operand |> unbox |> byte))
            | op when op = OpCodes.Ldc_I4 ->
                Value(Int(op,inst.Operand |> unbox |> int32))
            | op when (op = OpCodes.Ldc_I4_S && inst.Operand :? byte) ->
               Value(Byte(op,inst.Operand |> unbox |> byte))
            | op when (op = OpCodes.Ldc_I4_S) ->
                Value(SByte(op,inst.Operand |> unbox |> sbyte))
            | op when op = OpCodes.Ldc_I8 ->
                Value(Int64(op,inst.Operand |> unbox |> int64))
            | op when op = OpCodes.Ldc_R4 ->
                Value(Single(op,inst.Operand |> unbox |> single))
            | op when op = OpCodes.Ldc_R8 ->
                Value(Float(op,inst.Operand |> unbox |> float))
            | op when (op = OpCodes.Ldfld
                       || op = OpCodes.Ldflda 
                       || op = OpCodes.Ldsfld
                       || op = OpCodes.Ldsflda
                       || (op = OpCodes.Ldtoken && inst.Operand :? FieldInfo) ) -> 
                Value(Field(op,inst.Operand :?> FieldInfo))
            | op when (op = OpCodes.Stfld
                       || op = OpCodes.Stsfld ) -> 
                StoreField(op,inst.Operand :?> FieldInfo) |> Unary
            | op when op = OpCodes.Ldarg_0 ->
                RoleSelf |> This |> Value 
            | op when ((op = OpCodes.Ldloc) && inst.Operand :? int16) ->
                Value(Local(Ordinal(inst.Operand |> unbox |> int16)))
            | op when ((op = OpCodes.Ldloca) && inst.Operand :? int16) ->
                Value(Local(Address(inst.Operand |> unbox |> int16)))
            | op when ((op = OpCodes.Stloc) && inst.Operand :? int16) ->
                StoreLocal(Ordinal(inst.Operand |> unbox |> int16)) |> Unary
            | op when (op = OpCodes.Ldloc && inst.Operand :? LocalBuilder) ->
                Value(Local(Builder(inst.Operand :?> LocalBuilder)))
            | op when (op = OpCodes.Ldloca_S) ->
                Value(Local(ShortAddress(inst.Operand |> unbox |> byte)))
            | op when (op = OpCodes.Stloc_S) ->
                StoreLocal(ShortAddress(inst.Operand |> unbox |> byte)) |> Unary
            | op when op = OpCodes.Ldstr ->
                Value(String(inst.Operand :?> string))
            | op when op = OpCodes.Switch ->
                Switch(inst.Operand :?> Label [])
            | op when op = OpCodes.Sub ->
                Computation(Subtraction)
            | op -> Simple op 
        ) 
    
    let emit (ilg : ILGenerator)= function 
        Simple(op) -> 
            printfn "%A" op
            ilg.Emit(op)
        | Nnary(_,operation) ->
            match operation with
            MethodCall(op,mi) -> 
                printfn "%A - %s.%s" op mi.DeclaringType.Name mi.Name
                ilg.Emit(op,mi)
            | CtorCall(op,operand) -> ilg.Emit(op,operand)
        | Comparison operation -> 
             match operation with
             Equal -> ilg.Emit(OpCodes.Ceq)
             | o -> failwithf "Not a comparison operator (%A)" o
        | Computation operation -> 
             match operation with
             Subtraction -> 
                printfn "OpCodes.Sub"
                ilg.Emit(OpCodes.Sub)
             | o -> failwithf "Not a computation operator (%A)" o
        | Unary operation ->
            match operation with
            StoreField(op,operand) -> 
                printfn "%A" op
                ilg.Emit(op,operand)
            | StoreLocal local ->
                match local with
                Address _ -> failwith "Should be ordinal"
                | ShortAddress a -> ilg.Emit(OpCodes.Stloc_S,a)
                | Builder b -> ilg.Emit(OpCodes.Stloc,b)
                | Ordinal o -> ilg.Emit(OpCodes.Stloc,o)
        | TypeOperation(op,operand) -> 
            printfn "%A" op
            ilg.Emit(op,operand)
        | Value value ->
            match value with
            Byte(op,operand) -> 
                printfn "%A" op
                ilg.Emit(op,operand)
            | SByte(op,operand) -> 
                printfn "%A" op
                ilg.Emit(op,operand)
            | Int16(op,operand) -> 
                printfn "%A" op
                ilg.Emit(op,operand)
            | Int(op,operand) -> 
                printfn "%A" op
                ilg.Emit(op,operand)
            | Single(op,operand) -> 
                printfn "%A" op
                ilg.Emit(op,operand)
            | Int64(op,operand) -> 
                printfn "%A" op
                ilg.Emit(op,operand)
            | Float(op,operand) -> 
                printfn "%A" op
                ilg.Emit(op,operand)
            | String(str) -> 
                printfn "OpCodes.Ldstr"
                ilg.Emit(OpCodes.Ldstr,str)
            | Local(loc) ->
                match loc with
                Address a -> ilg.Emit(OpCodes.Ldloca,a)
                | ShortAddress a -> ilg.Emit(OpCodes.Ldloca_S,a)
                | Builder b -> ilg.Emit(OpCodes.Ldloc,b)
                | Ordinal o -> ilg.Emit(OpCodes.Ldloc,o)
            | Field(op,field) -> 
                printfn "%A, %s.%s" op field.DeclaringType.Name field.Name
                ilg.Emit(op,field)
            | This RoleSelf -> 
                printfn "OpCodes.Ldarg_0" 
                ilg.Emit(OpCodes.Ldarg_0)
            | This Player ->
                printfn "OpCodes.Ldarg_0"
                printfn "OpCodes.Ldfld (this), %s.%s" playerField.DeclaringType.Name playerField.Name
                ilg.Emit(OpCodes.Ldarg_0)
                ilg.Emit(OpCodes.Ldfld, playerField)
        | Switch(labels) -> ilg.Emit(OpCodes.Switch,labels)
    let methodBuilders = 
        t.GetMethods()
        |> Array.append (t.GetMethods(BindingFlags.NonPublic))
        |> Array.filter(fun meth -> 
            let isConcreteMethod = (meth.IsAbstract || meth.IsConstructor) |> not
            isConcreteMethod && meth.DeclaringType = t
        ) |> Array.map(fun meth ->
            let parameterTypes = meth.GetParameters() |> Array.map(fun p -> p.ParameterType)
            let s = 
                System.String.Join(" -> ",parameterTypes
                                          |> Array.map(fun t -> t.FullName))

            let returnType = meth.ReturnType
            printfn "Copying %s : %s -> %s" meth.Name s returnType.Name
            let m = copyType.DefineMethod(meth.Name, 
                                          meth.Attributes, 
                                          meth.CallingConvention, 
                                          returnType, 
                                          parameterTypes)
                                          
            let ilg = m.GetILGenerator()
            let instructions = 
                Mono.Reflection.Disassembler.GetInstructions meth
                |> Seq.filter(fun i -> i.OpCode <> OpCodes.Nop)
                |> readInstructions

            let methodBody = 
                instructions
                |> rewriteThis playerType roleType
            assert(instructions |> Seq.length = methodBody.Length)
            methodBody
            |> Seq.iter(emit ilg)
            m       
    )
    let newT = copyType.CreateType()
    let args : obj[] = [|player|]
    let roleObj = System.Activator.CreateInstance(newT,args)
   
    Role(player,roleObj).ActLike<'final>()