//
// This unit is part of the GLScene Project, http://glscene.org
//
{
   Custom Fast ThreadSafe Random Number Generator for Freepascal
   Implementation of XorShift algorithm for random numbers generation.
   (based on the "castle game engine"'s random number generator : https://castle-engine.sourceforge.io)
   In some cases it works 2 to 3 times faster than native FPC random function.

  Infos :
    - https://en.wikipedia.org/wiki/Linear_congruential_generator
    - https://en.wikipedia.org/wiki/Xorshift
    - https://en.wikipedia.org/wiki/Multiply-with-carry

   History :
       17/11/17 - J.Delauney (BeanzMaster) - Creation

}
Unit GLRandomGenerator;


Interface

{$I ../GLScene.inc}

Uses
  Classes, Sysutils;

Type
   { Methode aléatoire :
      - XorShif64 : Très rapide
      - rngtMWC256 : Moins rapide mais plus précis
   }
   TGLRandomNumGeneratorType = (rngtXorShift64, rngtMWC256); //rngtXorShift128, rngtXorShift1024,
   TGLRandomNumGenerator = class
    strict private
//      cSeed, iSeed : DWord;
//      QSeed : Array[0..$FF] of DWord;
      FRandSeed:Longint;
      FConstantSeed:Longint;
      FUseConstantSeed : Boolean;
      FRNGType : TGLRandomNumGeneratorType;

      function GetSeed: Longint; inline;
      procedure SetSeed(AValue: Longint);inline;
      procedure Initialize(const ASeed:Cardinal);inline;

      procedure XorShift;inline;
      //class function GetRandomSeed: LongInt;


    public
      constructor create;
      procedure Randomize;inline;
      function Random: extended; overload;inline;
      function RandomInt: LongWord; inline;
      function Random(range:longint):longint;overload;inline;
      function Random(range:int64):int64;overload;inline;
      procedure ResetSeed;
      property RandSeed:Longint Read GetSeed write SetSeed;
      //property UseConstantSeed : Boolean read FUseConstantSeed write FUseConstantSeed;
    end;

Var
    GLS_RNG : TGLRandomNumGenerator;

Implementation
Const
  //CONVERT_MWC256_TO_FLOAT =  2.32830643653869E-10; //1/MaxInt; // Extended (1.0/int64(1 shl 32)); //  0..1
  CONVERT_TO_FLOAT = 1/MaxInt;

//  CONVERT_SIGNED   = Extended (2.0/int64(1 shl 32)); // -1..1


//Threadvar
//  GLRNG_RandSeed:Longint;
 // GLRNG_OldRandSeed:Cardinal;
var
   store_64bit_seed: QWord = 0; //this variable stores 64 bit seed for reusing
   wait_for_seed: boolean = false;

function TGLRandomNumGenerator.GetSeed: Longint;
begin
  Result := FConstantSeed; //GLRNG_RandSeed;
end;

procedure TGLRandomNumGenerator.SetSeed(AValue: Longint);
begin
  FConstantSeed := AValue;
  FRandseed := AValue;
  //Initialize(GLRNG_Randseed);
end;

procedure TGLRandomNumGenerator.Initialize(const ASeed: Cardinal);
Var I:Integer;
begin
  if ASeed = 0 then
  begin
    //For I:=0 to $FF do
    //begin
      Randomize;
      //QSeed[i]:=GLRNG_randseed; //RandSeed;
    //End;
   // GLRNG_RandSeed := GetRandomSeed;
   // Randomize;
  end
  else
  begin
    FRandSeed := LongInt(ASeed);
    FConstantSeed := FRandSeed;
  end;
end;

procedure TGLRandomNumGenerator.ResetSeed;
begin
  FRandSeed := FConstantSeed;
End;

constructor TGLRandomNumGenerator.create;
begin
  initialize(0);
//  iSeed := 0;
//  cSeed:=0;
  //GLRNG_OldRandSeed := 0;
end;

(*RandSeed := ((RandSeed shl 8) or GetCurrentProcessID) xor
     GetTickCount; *)
procedure TGLRandomNumGenerator.Randomize;
const
  date_multiplier: QWord = 30000000;   //  approximative de la date
  date_order: QWord = 80000 * 30000000; // ordre : "now*date_multiplier" variable
  {p.s. date_order sera juste jusqu'à l'année ~ 2119}

var c64: QWord; // graine actuelle;
    b64: QWord; // graine supplémentaire pour la sécurité multi-threading
    a64: QWord; // une autre graine supplémentaire

    hours, mins, secs, msecs : Word;
  procedure xorshift64;
  begin
    c64:=c64 xor (c64 shl 12);
    c64:=c64 xor (c64 shr 25);
    c64:=c64 xor (c64 shl 27);
  end;
begin
  {Nous ajoutons une variable semi-aléatoire supplémentaire basée sur la variable locale c64 :
   son adresse 64 bits. Le seul bénéfice que nous avons ici est que cette adresse sera
   différent pour les différents threads, donc  2 threads ne peuvent être initialisés
   avec des graines égales, même si elles sont absolument simultanées }

  c64 := QWORD(@(c64));
  DecodeTime(Now,Hours,mins,Secs,msecs);
  {$R-}
  Secs := Secs * 100;
  mins := mins * 60;
  Hours := Hours * 3600;
  {$R+}
  a64:=Hours+ Mins + Secs + msecs;


  while wait_for_seed do
  begin
    //DecodeTime(Now,Hour,mins,Secs,msecs);
    //Secs := Secs * 100;
    //mins := mins * 60;
   // Hour := Hour * 3600;
    a64:=a64+QWord(now);
    xorshift64; //En attendant, on fait quelque chose
  End;

  wait_for_seed := true;     // Empêche une autre randomisation de commencer jusqu'à ce que celui-ci est fini

  c64 := (c64 + a64) shr 1;
  c64 :=  ((c64 shl 8) or a64) xor GetTickCount64;

  b64 := c64;   // notre autre graine aléatoire basée sur l'allocation de la mémoire de thread en cours

  {fondamentalement, nous ne nous soucions pas si les threads passeront accidentellement
  'wait_for_seed' le verrouille.
   Grâce à b64 nous aurons des valeurs aléatoires différentes, mais ce n'est peut être pas optimal }

  if store_64bit_seed = 0 then
  begin //1ere randomization


   (* DecodeTime(Now,Hour,mins,Secs,msecs);
    Secs := Secs * 100;
    mins := mins * 60;
    Hour := Hour * 3600;
    a64:=Hour + Mins + Secs + msecs;
    c64 := (c64 + a64) shr 1; *)

    { Maintenant, nous devons nous assurer que l'ajout de 'a64' ne débordera pas
      Nous ajoutons quelques xorshift64 juste pour le plaisir au cas où}
    while (c64 > high(QWord)-date_order) do xorshift64;

    { Pour tuer la valeur discrette aléatoire introduit par gettickcount64 nous ajoutons 'Now'.
      'now' et 'gettickcount64' ne sont pas indépendants mais changent synchroniquement.
      Après plusieurs xorshift64, c64 n'a plus aucune information
      laissé par gettickcount64 et nous introduisons un changement semi-indépendant dans la graine aléatoire}
    c64 := c64+ QWord(round(now*date_multiplier));

    { Un autre cycle xorshift de 64 bits pour tuer tout ce qui reste 'Now' }
    xorshift64;
    { Maintenant nous sommes sûrs d'obtenir une graine aléatoire différente même
      dans le cas où nous lancons la procédure  exactement à la même milliseconde depuis
      le démarrage de l'OS.
      Une date et heure différentes donneront une autre graine aléatoire ...
      A moins de fixer délibérément la date et l'heure }


  end
  else
    c64 := store_64bit_seed; //On reprend juste la graine déja générer

 // c64 := c64 shr 1;  // note: nous jetons 1 bit de précision pour gagner de la vitesse
  { Maintenant, nous faisons juste un autre xorshift64, car nous avons une variable c64 aléatoire correcte }
  xorshift64;
  {On fusionne une autre variable aléatoire basée sur le thread en cours }
  c64 := c64 xor b64;

  {et pour finir, afin d"éviter d'avoir une graine à ZERO}
  repeat
    {Quelques xorshift64 de plus}
    xorshift64;
    {On garde les 32-bits haut de c64 pour avoir une veribale graine 64bits}
    FConstantSeed := longint(c64 shr 32);
  until FConstantSeed<>0;
 // FConstantSeed := FConstantSeed shr 1;
  FRandSeed := FConstantSeed;
  { On sauvegarde notre graine pour une réutilisation ultérieur au cas ou }
  store_64bit_seed := c64;
  {On passe la main au prochain thread}
  wait_for_seed := false;
end;

procedure  TGLRandomNumGenerator.XorShift;  inline;
begin
 // FRandSeed := FRandSeed shr 1;
  { Ffonctionne un peu plus vite (+ 4%) en raison d'une meilleure optimisation
    par compilateur (utilise des registres de CPU au lieu d'une variable) }
  FRandSeed := ((FRandSeed xor (FRandSeed shl 1)) xor ((FRandSeed xor (FRandSeed shl 1)) shr 15)) xor
         (((FRandSeed xor (FRandSeed shl 1)) xor ((FRandSeed xor (FRandSeed shl 1)) shr 15)) shl 4);

  (* FRandSeed:= FRandSeed xor (FRandSeed shl 1);
    FRandSeed:= FRandSeed xor (FRandSeed shr 15);
    FRandSeed :=FRandSeed xor (FRandSeed shl 4); *)

end;



function TGLRandomNumGenerator.Random: extended;  Inline;
//var tSeed : qword;
begin
  // MWC256 from Usenet posting by G. Marsaglia - Period 2^8222
 (* iSeed := (iSeed+1) AND $FF;
  tSeed := qword (809430660) * QSeed[iSeed] + cSeed;
  cSeed        :=  hi (tSeed);
  QSeed[iSeed] := lo (tSeed);
  result := CONVERT_TO_FLOAT*(QSeed[iSeed] shr 1); *)

  XorShift;
  result := CONVERT_TO_FLOAT*Longint(FRandSeed shr 1);  // note: nous jetons 1 bit de précision pour gagner de la vitesse
end;

function TGLRandomNumGenerator.Random(range: longint): longint;
begin
  XorShift;
  if range>1 then
    result := LongInt((int64(LongWord(FRandSeed))*range) shr 32) // Plus rapide que FRandSeed Mod Range
  else
    result := 0
end;

function TGLRandomNumGenerator.RandomInt: LongWord;
begin
  XorShift;
  result := LongWord(FRandSeed);
end;

function TGLRandomNumGenerator.Random(range: int64): int64;
var c64: QWord;
  procedure xorshift64; inline;
  begin
    c64:=c64 xor (c64 shl 12);
    c64:=c64 xor (c64 shr 25);
    c64:=c64 xor (c64 shl 27);
  end;
begin
  {Même si N = 0..1 pour faire un cycle de semences aléatoires de 32 bits nous devons le faire deux fois}
  c64 := qword(RandomInt) or (qword(RandomInt) shl 32);
  if range > 1 then
  begin
    {l'ajout d'un cycle xorshift64 nous garantit que c64 est vraiment aléatoire
     dans la plage 1..high (QWORD) mais ralentit l'exécution de ~ 10%}
    xorshift64;
    {Contrairement à SysUtils nous en faisons un vrai nombre aléatoire de 64-bit et non pas un faux de 63 bits :)
     Il ne peut pas y avoir de débordement ici, parce que N est int64 et il ne peut pas être
     plus grand que (Hi(QWORD) div 2)
     C'est-à-dire que nous ne pourrons jamais obtenir un résultat 'négatif' car le premier bit du résultat sera toujours zéro }
    result := int64(qword(c64) mod qword(Range))
  end
  else
    result := 0;
end;

initialization
  GLS_RNG := TGLRandomNumGenerator.create;
finalization
  FreeAndNil(GLS_RNG);
End.

