string fun(string byValue, int &ByReference) {
    byValue = "s2";
    ByReference++;
    return byValue;
}

string s = "s1";
int toBeChanged = 10;
string s2 = fun(s, &toBeChanged);
Print(s2); // s2
Print(s); // s1
Print(toBeChanged); // 9