// Note: this is an automatically generated file. Do not edit.

/**
 * Basic string functions
 */
public interface String {
  static String fromList(java.util.List<Integer> codes) {
    return hydra.lib.strings.FromList.apply((codes));
  }
  
  static Boolean isEmpty(String str) {
    return hydra.lib.strings.IsEmpty.apply((str));
  }
  
  static Integer length(String s) {
    return hydra.lib.strings.Length.apply((s));
  }
  
  static java.util.List<Integer> toList(String s) {
    return hydra.lib.strings.ToList.apply((s));
  }
  
  static String toLower(String s) {
    return hydra.lib.strings.ToLower.apply((s));
  }
  
  static String toUpper(String s) {
    return hydra.lib.strings.ToUpper.apply((s));
  }
}
