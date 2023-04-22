int x = 12;
int y = 12;
if (x == y) {
    Print("pierwszy case");
    x--;
    if ((x + 10) == y) {
        Print("to nie powinno sie tu pojawic");
    } else {
        Print("jest dobrze");
    }
} 
Print("po ifie");
Print(x);