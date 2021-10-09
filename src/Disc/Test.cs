namespace Disc {

    public interface ISourcePlayer { 
        void Withdraw(string message, decimal amount);
    }
    public interface ISinkPlayer {
        void Deposit(string message, decimal amount);
    }

    public class Account : ISinkPlayer, ISourcePlayer {
        decimal _balance;
        public Account(decimal initialBalance){
            _balance = initialBalance;
        }

        public decimal Balance {
            get { return _balance;}
        }

        public void Deposit(string msg, decimal amount){
            _balance += amount;
        }

        public void Withdraw(string msg, decimal amount){
            _balance -= amount;
        }

    }

    public static class Program {
        public class MoneyTransferContext {
            public interface ISinkRole{
                void Accept(string message,decimal amount);
            }
                
            public interface ISourceRole {
                void Send(string message,decimal amount);
            }
                
            public interface ISource : ISourcePlayer, ISourceRole {}
            public interface ISink : ISinkPlayer, ISinkRole {}

            private abstract class Source : ISource { 
                public void Send(string message,decimal amount) {
                    this.Withdraw(message,amount);
                }
                public abstract void Withdraw(string message,decimal amount);
            }

            private abstract class Sink : ISink { 
                public void Accept(string message,decimal amount) {
                    this.Deposit(message,amount);
                }
                public abstract void Deposit(string message,decimal amount);
            }
            private readonly ISink _sink;
            private readonly ISource _source;
            public MoneyTransferContext(ISourcePlayer source, ISinkPlayer sink) {
                _source = Role.createRole<ISource,Source,ISourcePlayer>(source);
                _sink = Role.createRole<ISink,Sink,ISinkPlayer>(sink);
            }
            public void Transfer(string sourceMessage, string sinkMessage, decimal amount){
                _source.Send(sourceMessage, amount);
                _sink.Accept(sinkMessage, amount);
            }
        }

        public static int Main(string[] args) {
            var source = new Account(1000m);
            var sink = new Account(0m);
            var ctx = new MoneyTransferContext(source,sink);
            ctx.Transfer("Monday night dinner", "Payment for Dinner from me", 250m);
            System.Console.WriteLine("Source: " + source.Balance.ToString());
            System.Console.WriteLine("Sink: " + sink.Balance.ToString());
            return 0;
        }
    }
}