public class Test_class {
    public final String field1;
    public final int field2;
    public final short field3;
    public final long field4;
    public final java.util.List<Integer> field5;
    public final java.util.List<Short> field6;
    public final java.util.List<Long> field7;
    public final java.util.List<String> field8;
}
public interface Test_interface {
    static boolean check(Integer x) {
        return ((x>5) ? "yes" : "no").isEmpty();
    }

    boolean example = "hello".equals("hi");
    boolean isLowerCase = Character.isLowerCase('a');
    boolean is_ = "hello".toLowerCase().isEmpty();
    int add_ = 1 + 1 + 1;
    int arifm = 1 + (-1)*5;
    int sub = 2 - 3;
    int mul = 2 * 5;
    int div = 5 / 2;
    int rem = 5 % 1;
    int mod = Math.floorMod(1,2);
    int len = "Hello".length();
    boolean toLower = "Hello".toLowerCase().equals("hello");
    boolean eq = 5 == 6;
    boolean gr = 5 > 6;
    boolean gre = 5 >= 6;
    boolean le = 5 < 6;
    boolean lee = 5 <= 6;
    String tern = (6>5) ? "yes" : "no";

    java.util.List<Integer> listOfLists = java.util.Arrays.asList(
            1,
            2,
            3
    );
}