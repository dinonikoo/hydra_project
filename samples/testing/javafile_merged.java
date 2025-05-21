public class Person {
    public final String name;
    public final int age;
}

public interface Testing {
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
