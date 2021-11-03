module Role
open System.Dynamic
open System.Reflection
open System.Reflection.Emit
open System.Linq.Expressions
open System 
open ImpromptuInterface
open Microsoft.CSharp.RuntimeBinder

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

type private RoleMetaObject<'player,'role>(expr : Expression, roleContainer : Role<'player,'role>) = 
    inherit DynamicMetaObject(expr, BindingRestrictions.Empty, roleContainer)
    let rewritter (exp : Expression) = ExpressionTreeRewritter(roleContainer).Visit exp
    let playerType = typeof<'player>
    let roleType = typeof<'role>
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
        
        let method = 
            match roleType.GetMethod(binder.Name) with
            method when (method |> isNull || method.IsAbstract) ->
                let methods = 
                    roleType.GetInterfaces()
                    |> Array.collect(fun t -> t.GetMethods())
                methods
                |> Array.filter(fun m -> 
                    m.Name = binder.Name
                    && not method.IsAbstract)
                |> Array.tryHead
            | method -> Some method
        let restrictions =
            BindingRestrictions.GetTypeRestriction(this.Expression, this.LimitType)
        let returnVal = 
            match method with
            Some method ->
                let expressionTree,_,variables = ExpressionTree.methodToExpressionTree true method
                let thisName = sprintf "%s____param__this" method.Name

                let assignments = 
                    Expression.Assign(variables.[thisName],Expression.Convert(Expression.Constant(null),roleType)) :> Expression //should be dynmically bound to role, needs rewrite of tree
                    ::(args
                       |> Array.zip (method.GetParameters())
                       |> Array.map(fun (p,arg) -> Expression.Assign(variables.[sprintf "%s____param__%s" method.Name p.Name], 
                                                                  Expression.Convert(arg.Expression, p.ParameterType)) :> Expression)
                       |> List.ofArray)
                
                let methodBody = 
                    match rewritter expressionTree.Expression with
                    :? BlockExpression as be -> 
                        let exprs = be.Expressions |> Seq.toList
                        if be.Type = typeof<System.Void> || be.Type = typeof<unit> then
                            exprs@[Expression.Default(typeof<obj>)]
                        else
                            let reversed = exprs |> List.rev
                            (Expression.Convert(reversed.Head, typeof<obj>) :> Expression)
                            ::(reversed.Tail)
                            |> List.rev

                    | e -> [e]
                
                Expression.Block(
                    (variables |>Map.toList |> List.map snd),
                    assignments@methodBody
                ) :> Expression
            | None ->
                let method = playerType.GetMethod(binder.Name)
                let args = 
                   args
                    |> Array.zip (method.GetParameters())
                    |> Array.map(fun (p,arg) -> Expression.Convert(arg.Expression, p.ParameterType) :> Expression) 
                let expr = Expression.Call(roleContainer.PlayerExpression,method,args) :> Expression
                if method.ReturnType = typeof<System.Void> || method.ReturnType = typeof<unit> then
                    Expression.Block(
                        expr,
                        Expression.Default(typeof<obj>)
                    ) :> Expression
                else
                    Expression.Convert(expr, typeof<obj>) :> Expression
       
        DynamicMetaObject(returnVal,restrictions)
        
and Role<'player,'role>(player : 'player) as this = 
    [<DefaultValue>] val mutable private _player : 'player
    
    
    let playerExpr = 
        Expression.Field(Expression.Constant this, "_player") :> Expression
    member internal __.PlayerExpression with get() = playerExpr
    
    interface IDynamicMetaObjectProvider with

      member this.GetMetaObject expr = 
          RoleMetaObject(expr, this) :> DynamicMetaObject
    do
       this._player <- player

and ExpressionTreeRewritter<'player, 'role>(roleContainer : Role<'player, 'role>) = 
    inherit ExpressionVisitor()
    let playerExpression = roleContainer.PlayerExpression
    let roleType = typeof<'role>
    let isPlayer (exp : Expression) = 
        exp.Type = playerExpression.Type
        || exp.Type.IsAssignableFrom playerExpression.Type
    let isRole (exp : Expression) = 
        //TODO: this fails when two roles have the same type
        exp.Type = roleType
    
    override  __.VisitMethodCall methodCall =   
        let args = 
            methodCall.Arguments
            |> Seq.map(function
                arg when arg |> isRole -> 
                   playerExpression
                | arg -> arg
            ) |> List.ofSeq
        match methodCall.Object with
        | instance when instance |> isRole ->
            let args = (Expression.Constant(roleContainer) :> Expression)::args
            let binder = Binder.InvokeMember(
                             CSharpBinderFlags.None,
                             methodCall.Method.Name, 
                             [||],
                             roleType,
                             args
                             |> List.map(fun _ -> CSharpArgumentInfo.Create(CSharpArgumentInfoFlags.None, null))
            )
            Expression.Convert(Expression.Dynamic(binder, typeof<obj>, args),methodCall.Method.ReturnType) :> Expression
        | instance -> 
            let instance = if instance |> isPlayer then playerExpression else instance
            Expression.Call(instance, methodCall.Method,args) :> Expression
        
        
   
    
let createRole<'final, 'role, 'player when 'final : not struct> (player : 'player) = 
    Role<'player,'role>(player).ActLike<'final>()