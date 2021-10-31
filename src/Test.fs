namespace Dish

open FSharp.Interop.Dynamic
open System.Dynamic
open ImpromptuInterface
open Microsoft.FSharp.Quotations
open FSharp.Quotations.Patterns
open System.Reflection

module Program = 
    type LedgerEntry =
        Deposit of message: string * amount: decimal
        | Withdraw of message: string * amount: decimal

    type ISourcePlayer = 
        abstract Withdraw : (string * decimal) -> Account
    and ISinkPlayer = 
        abstract Deposit : (string * decimal) -> Account
    and Account =
        {
            Name : string
            Ledger : (System.DateTime * LedgerEntry) list
        }
        with 
            member this.Deposit(msgAndamount) = { this with Ledger = (System.DateTime.Now,Deposit msgAndamount)::this.Ledger }
            member this.Withdraw(msgAndamount) = { this with Ledger = (System.DateTime.Now,Withdraw msgAndamount)::this.Ledger }
            member this.Balance 
                with get() = 
                    this.Ledger
                    |> List.sumBy(function
                        _, Deposit (_,amount) -> amount
                        | _, Withdraw (_,amount) -> -amount
                    )
            static member New name amount = 
                {
                    Name = name
                    Ledger = []
                }.Deposit("Initial funds", amount)
            interface ISourcePlayer with
                member this.Withdraw(msgAndamount) = 
                    this.Withdraw(msgAndamount)
            interface ISinkPlayer with
                member this.Deposit(msgAndamount) = 
                    this.Deposit(msgAndamount)

    type TransferState =
        | Initial
        | Ready of string * decimal
        | InsufficientFunds
        | TransferCompleted of Account * Account
        | PartialTransfer of Account * decimal
        | Failed of string

    type ISinkRole =
        abstract Accept : string -> decimal -> Account
    type ISourceRole =
        abstract Send : string -> decimal -> Account
        
    type ISource = 
        interface
            inherit ISourcePlayer
            inherit ISourceRole
        end
    type ISink = 
        interface
            inherit ISinkPlayer
            inherit ISinkRole
        end
    
    [<AbstractClass>]
    type Source() = 
        member this.Send msg amount =
            (this :> ISourcePlayer).Withdraw((msg,amount))
        abstract member Withdraw :  (string * decimal) -> Account
        interface ISourcePlayer with
            member this.Withdraw(w) = this.Withdraw w 
        interface ISourceRole with
            member this.Send msg amount = this.Send msg amount  
    
    [<AbstractClass>]
    type Sink() = 
        member this.Accept (msg:string) (amount:decimal) =
            (this :> ISinkPlayer).Deposit((msg,amount))
        abstract member Deposit : (string * decimal) -> Account
        interface ISinkPlayer with
            member this.Deposit(d) = this.Deposit d
        interface ISinkRole with
            member this.Accept msg amount = this.Accept msg amount  
    
    
    type MoneyTransferContext(source,sink) =
         let _source = Role.createRole<ISource,Source,_> source
         let _sink = Role.createRole<ISink,Sink,_> sink
         member __.Transfer sourceMessage sinkMessage amount = 
             let sourceUpdated = _source.Send sourceMessage amount
             let sinkUpdated = _sink.Accept sinkMessage amount
             sourceUpdated,sinkUpdated

    [<EntryPoint>]
    let main _ = 
        let source = 
            {
                Name = "Source"
                Ledger = []
            }.Deposit("Initial deposit", 1000m)
        let sink = 
            {
                Name = "Source"
                Ledger = []
            }.Deposit("Initial deposit", 0m)
        let ctx = MoneyTransferContext(source,sink)
        let src,snk = ctx.Transfer "Monday night dinner" "Payment for Dinner from me" 250m
        printfn "Source account %A" src
        printfn "Sink account %A" snk
        0