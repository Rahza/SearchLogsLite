package analysis;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class FileCreator {

	private final static String INPUT_FOLDER = "C:\\SearchLogs\\logs";	
	private final static String OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\outputx.csv";
	private final static String MATRIX_OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\matrix.csv";
	private final static String INTERACTIONS_OUTPUT_FILE_NAME = "C:\\SearchLogs\\output\\interactions.csv";
	
	private static List<String> inputFiles;
	
	public static void main(String[] args) {
		inputFiles = new ArrayList<String>();
		
		// Create a list of the paths to all of the files contained in the input folder
		try {
			Files.walk(Paths.get(INPUT_FOLDER)).forEach(filePath -> {
			    if (Files.isRegularFile(filePath)) {
			        inputFiles.add(filePath.toString());
			    }
			});
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		FileProcessor processor = new FileProcessor(inputFiles, OUTPUT_FILE_NAME);
		
		try {
			processor.startProcessing();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
