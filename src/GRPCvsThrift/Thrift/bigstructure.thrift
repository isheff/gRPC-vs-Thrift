
union BigStructureValue {
  1: i64 i,
  2: bool b,
  3: string s,
  4: binary y
}

struct BigStructure {
  1: BigStructureValue v,
  2: list<BigStructure> children
}

service ThriftBigStructure {

   void ping(),

   void bigstructurecall(1:BigStructure input)

}