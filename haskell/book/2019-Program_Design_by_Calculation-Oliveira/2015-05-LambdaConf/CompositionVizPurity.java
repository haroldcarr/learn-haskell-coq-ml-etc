import java.io.*;

public class CompositionVizPurity {
    public static void main(String[] av) throws IOException {
        final FG fg = new FG();
        System.out.println(fg.f(fg.g(1)));
    }

    static class FG {
        public int g(int a) {
            try {
                BufferedReader br = new BufferedReader(new FileReader("/tmp/JUNK"));
                final int result = a + Integer.valueOf(br.readLine());
                br.close();
                return result;
            } catch (Throwable t) { return -1; }
        }
        public int f(int b) {
            try {
                Integer result = b + 3;
                BufferedWriter bw = new BufferedWriter(new FileWriter("/tmp/JUNK"));
                bw.write(result.toString(), 0, result.toString().length());
                bw.close();
                return result;
            } catch (Throwable t) { return -1; }
        }
    }
}

/*
javac CompositionVizPurity.java
java  CompositionVizPurity
 */

// End
