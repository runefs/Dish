namespace Dish
open FSharp.Interop.Dynamic
open System.Dynamic
open ImpromptuInterface
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open System.Reflection
open System
open System.Runtime.CompilerServices
open Microsoft.CSharp.RuntimeBinder

[<AutoOpen>]
module Main =

    type Player<'playerContract, 'roleContract,'computationState when 'playerContract : equality> = 
        {
            ActualPlayer : 'playerContract option
            Methods : ('playerContract -> 'roleContract -> 'computationState) option
        }
        with static member Empty with get() = { ActualPlayer = None; Methods = None}
             override this.GetHashCode() = this.ActualPlayer.GetHashCode()
             override this.Equals(o) = this.ActualPlayer.Equals o

    type ITestType = 
        abstract member A : int -> int
    
    type Role<'contract>() = 
        
        member __.Self with get() = (null :> obj :?> 'contract)

        member this.Yield(_) = this.Zero()
        member __.Zero() = Player<_,'contract,_>.Empty
        
        member __.Run(state : Player<_,'contract,_>) = 
            state
        
        [<CustomOperation("define")>]
        member __.DefineRoleMethod(player : Player<'player,'contracts,_>, methods: ('player -> 'contracts -> _) )  =
            {player with Methods = Some methods }
        [<CustomOperation("bind")>]
        member __.Bind<'playerContract, 'roleContract,'computationState when 'playerContract : equality>(state : Player<_,'roleContract,_>,player : 'playerContract) : Player<'playerContract,'roleContract,_> =
            {state with ActualPlayer = Some player}

    let inline (?) (self:Player<_,'contract,'computationState>) (method:'contract) : 'computationState =
           self.Methods.Value self.ActualPlayer.Value method