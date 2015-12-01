package analysis;

import java.io.IOException;

public class FileCreator {

	final static String FILE_NAME = "C:\\SearchLogs\\logs\\user-ct-test-collection-01.txt";
	final static String OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\output.csv";
	final static String MATRIX_OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\matrix.csv";
	final static String INTERACTIONS_OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\interactions.csv";

	public static void main(String[] args) {
		FileProcessor processor = new FileProcessor(FILE_NAME, OUTPUT_FILE_NAME);
		try {
			processor.startProcessing();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
