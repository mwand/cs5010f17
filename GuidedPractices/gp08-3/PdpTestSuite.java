import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.BiFunction;
import java.util.function.Supplier;

/**
 * A PdpTestSuite represents a collection of test cases that may be run
 * so as to produce final output similar to that of Rackunit.
 * 
 * @author justin
 *
 */
public class PdpTestSuite {

    public PdpTestSuite(int defaultTimeoutSeconds) {
        this.defaultTimeoutSeconds = defaultTimeoutSeconds;
        this.testCases = new ArrayList<>();
    }

    public <T> void addTestCase(String name, Supplier<T> actualFn, T expected) {
        addTestCase(name, actualFn, expected, null);
    }

    public <T> void addTestCase(String name, Supplier<T> actualFn, T expected, int timeoutSeconds) {
        addTestCase(name, actualFn, expected, null, timeoutSeconds);
    }

    public <T> void addTestCase(String name, Supplier<T> actualFn, T expected, BiFunction<T, T, Boolean> equalsFn) {
        addTestCase(name, actualFn, expected, equalsFn, defaultTimeoutSeconds);
    }

    public <T> void addTestCase(String name, Supplier<T> actualFn, T expected, BiFunction<T, T, Boolean> equalsFn, int timeoutSeconds) {
        testCases.add(new TestCase<T>(name, actualFn, expected, equalsFn, timeoutSeconds));
    }

    public void addExternalTestCase(String name, String cmd, String expectedOutput) {
        addExternalTestCase(name, cmd, expectedOutput, defaultTimeoutSeconds);
    }

    public void addExternalTestCase(String name, String cmd, String expectedOutput, int timeoutSeconds) {
        int fakeTimeoutSeconds = timeoutSeconds + 10;
        testCases.add(new TestCase<String>(name, () -> {
            ProcessBuilder pb = new ProcessBuilder(
                    "/usr/bin/timeout", "-k", "1",
                    Integer.toString(timeoutSeconds),
                    "/bin/bash", "-c", cmd);
            pb.redirectErrorStream(true);
            Process proc = null;
            try {
                proc = pb.start();
                int r = proc.waitFor();
                if (r == 124 || r == 137) {
                    throw new RuntimeException("timed out");
                }
                BufferedReader in = new BufferedReader(new InputStreamReader(proc.getInputStream()));
                String output = in.readLine();
                in.close();
                return output;
            } catch (IOException | InterruptedException e) {
                if (proc != null) {
                    proc.destroyForcibly();
                }
                throw new RuntimeException(e);
            }
        }, expectedOutput, null, fakeTimeoutSeconds));
    }

    public void runTests() {
        ExecutorService executor = Executors.newCachedThreadPool();
        int passes = 0, failures = 0, errors = 0;
        for (TestCase<?> tc : testCases) {
            TestResult r = tc.run(executor);
            switch (r) {
            case PASS:
                passes++; break;
            case FAIL:
                failures++; break;
            case ERROR:
                errors++; break;
            }
        }
        executor.shutdownNow();
        System.out.format("%d success(es) %d failure(s) %d error(s) %d test(s) run",
                passes, failures, errors, testCases.size());
    }

    private int defaultTimeoutSeconds;
    private List<TestCase<?>> testCases;

    private static enum TestResult { PASS, FAIL, ERROR }

    private static class TestCase<T> {
        String name;
        Supplier<T> actualFn;
        T expected;
        BiFunction<T, T, Boolean> equalsFn;
        int timeoutSeconds;

        public TestCase(String name, Supplier<T> actualFn, T expected,
                BiFunction<T, T, Boolean> equalsFn, int timeoutSeconds) {
            super();
            this.name = name;
            this.actualFn = actualFn;
            this.expected = expected;
            this.equalsFn = equalsFn;
            this.timeoutSeconds = timeoutSeconds;
        }

        public TestResult run(ExecutorService executor) {
            System.out.print("Running test: " + name + "... ");
            Future<T> future = executor.submit(() -> actualFn.get());
            try {
                T actual = future.get(timeoutSeconds, TimeUnit.SECONDS);
                boolean passed;
                if (actual == null && expected == null) {
                    passed = true;
                } else if (actual == null || expected == null) {
                    passed = false;
                } else if (equalsFn != null) {
                    passed = equalsFn.apply(actual, expected);
                } else {
                    passed = expected.equals(actual);
                }
                if (passed) {
                    System.out.println("PASS");
                    return TestResult.PASS;
                } else {
                    System.out.println("FAIL");
                    System.out.println("  actual:   " + actual);
                    System.out.println("  expected: " + expected);
                    return TestResult.FAIL;
                }
            } catch (Exception e) { // includes Timeout and ExecutionException
                System.out.println("ERROR");
                e.printStackTrace();
                future.cancel(true);
            }
            System.err.flush();
            return TestResult.ERROR;
        }
    }
}
