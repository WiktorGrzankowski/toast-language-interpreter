while (true) {
    Print("break incoming...");
    break;
    Print("hopefully i am not printed");
}

int a = 3;
while (a > 0) {
    Print("continue eventually");
    a--;
    continue;
    a = -10;    
}
Print(a); // 0