package analysis;

public class FileCreator {

	final static String FILE_NAME = "C:\\SearchLogs\\logs\\user-ct-test-collection-01.txt";
	final static String OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\outputx.csv";
	final static String MATRIX_OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\matrixx.csv";
	final static String INTERACTIONS_OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\interactionsx.csv";

	public static void main(String[] args) {
		FileProcessor processor = new FileProcessor(FILE_NAME, OUTPUT_FILE_NAME, MATRIX_OUTPUT_FILE_NAME, INTERACTIONS_OUTPUT_FILE_NAME);
	}

}
