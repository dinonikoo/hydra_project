// Note: this is an automatically generated file. Do not edit.

/**
 * This type generated from Java
 */
public class Person {
  public final String name;

  public final Integer age;
}

// Note: this is an automatically generated file. Do not edit.

/**
 * mainModule generated code
 */
public interface Testing {
  Boolean example = hydra.lib.equality.EqualString.apply(
    "hello",
    "hi");
  
  Boolean isLowerCase = hydra.lib.chars.IsLower.apply(97);
  
  Boolean is_ = hydra.lib.strings.IsEmpty.apply(hydra.lib.strings.ToLower.apply("hello"));
  
  Integer add_ = hydra.lib.math.Add.apply(
    hydra.lib.math.Add.apply(
      1,
      1),
    1);
  
  Integer arifm = hydra.lib.math.Add.apply(
    1,
    hydra.lib.math.Mul.apply(
      hydra.lib.math.Neg.apply(1),
      5));
  
  Integer sub = hydra.lib.math.Sub.apply(
    2,
    3);
  
  Integer mul = hydra.lib.math.Mul.apply(
    2,
    5);
  
  Integer div = hydra.lib.math.Div.apply(
    5,
    2);
  
  Integer rem = hydra.lib.math.Rem.apply(
    5,
    1);
  
  Integer mod = hydra.lib.math.Mod.apply(
    1,
    2);
  
  Integer len = hydra.lib.strings.Length.apply("Hello");
  
  Boolean toLower = hydra.lib.equality.EqualString.apply(
    hydra.lib.strings.ToLower.apply("Hello"),
    "hello");
  
  Boolean eq = hydra.lib.equality.EqualInt32.apply(
    5,
    6);
  
  Boolean gr = hydra.lib.equality.GtInt32.apply(
    5,
    6);
  
  Boolean gre = hydra.lib.equality.GteInt32.apply(
    5,
    6);
  
  Boolean le = hydra.lib.equality.LtInt32.apply(
    5,
    6);
  
  Boolean lee = hydra.lib.equality.LteInt32.apply(
    5,
    6);
  
  String tern = hydra.lib.logic.IfElse.apply(
    hydra.lib.equality.GtInt32.apply(
      6,
      5),
    "yes",
    "no");
  
  java.util.List<Integer> listOfLists = java.util.Arrays.asList(
    1,
    2,
    3);
  
  static Boolean check(Integer x) {
    return hydra.lib.strings.IsEmpty.apply(hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.GtInt32.apply(
        (x),
        5),
      "yes",
      "no"));
  }
}
