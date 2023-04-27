int outer(int z) {
    int inner(int y, string message) {
        int z = 420;
        Print(message);
        Print(z);
        return z + y;
    }
    string s = "i am inner and z is:";
    Print("outer: z is");
    Print(z);
    
    return z + inner(69, s);
}

Print(outer(17));