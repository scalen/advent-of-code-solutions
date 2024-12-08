namespace _2024.Utilities
{
    public class Coordinates<T>(T X, T Y)
    {
        public T X { get; set; } = X;
        public T Y { get; set; } = Y;

        public Coordinates<T> Copy() => new(X, Y);
    }
}
