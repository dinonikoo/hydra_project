// Note: this is an automatically generated file. Do not edit.

/**
 * Basic logic functions
 */
public interface Logic {
  static Boolean and(Integer x) {
    return hydra.lib.logic.And.apply(
      hydra.lib.equality.EqualInt32.apply(
        (x),
        0),
      hydra.lib.equality.EqualInt32.apply(
        (x),
        1));
  }
  
  static Boolean ifElse(Integer x) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.EqualInt32.apply(
        hydra.lib.math.Mod.apply(
          (x),
          2),
        0),
      true,
      false);
  }
  
  static Boolean not(Integer x) {
    return hydra.lib.logic.Not.apply(hydra.lib.equality.EqualInt32.apply(
      (x),
      0));
  }
  
  static Boolean or(Integer x) {
    return hydra.lib.logic.Or.apply(
      hydra.lib.equality.EqualInt32.apply(
        (x),
        0),
      hydra.lib.equality.EqualInt32.apply(
        (x),
        1));
  }
}
