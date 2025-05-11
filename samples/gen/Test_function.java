// Note: this is an automatically generated file. Do not edit.

/**
 * A module with a mod function
 */
public interface Test_function {
  static String isEven(Integer x) {
    return hydra.lib.logic.IfElse.apply(
      hydra.lib.equality.EqualInt32.apply(
        hydra.lib.math.Mod.apply(
          (x),
          2),
        0),
      "even",
      "odd");
  }
}
