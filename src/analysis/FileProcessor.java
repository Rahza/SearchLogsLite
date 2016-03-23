package analysis;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

public class FileProcessor {
	// Separator in the output file
	private static final String SEPARATOR = ",";
	
	// If a query or URL contains the separator, replace it with the following character
	private static final String SEPARATOR_REPLACEMENT = ".";
	
	// The header of the CSV file
	private static final String HEADER = "sessionId" + SEPARATOR + "userId" + SEPARATOR + "query" + SEPARATOR + "position" + SEPARATOR + "url" + SEPARATOR + "rawdate" + SEPARATOR + "javaDate" + SEPARATOR + "lastInteraction" + SEPARATOR + "epoc\n";

	// Length of a session in milliseconds (if the user has not issued a query after this time, the session is considered to be over)
	private static final int SESSION_LENGTH = 1800000;
	
	private SimpleDateFormat dateFormat;
	
	private List<String> input;
	private String output;
	
	private FileWriter writer;
	
	// A "switch" to make sure the first line (header) of each input file is not being read
	private boolean active = false;
	
	public FileProcessor(List<String> input, String output) {
		this.input = input;
		this.output = output;
		
		dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
	}
	
	public void startProcessing() throws FileNotFoundException, IOException, ParseException {
		System.out.println("--- PROCESSING STARTED ---");
		
		long time = System.currentTimeMillis();
		
		// Initialize the FileWriter and add the header
		writer = new FileWriter(output);
		writer.append(HEADER);

		// Go through the list of files and read each file
		for (int i = 0; i < input.size(); i++) {
			readFiles(i);
			float elapsedTime = ((float) (System.currentTimeMillis() - time)) / 1000;
			System.out.println("Finished reading " + (i+1) + "/" + input.size() + " files after " + elapsedTime + " seconds (" + Math.round((((double) (i+1))/input.size())*100) + "%).");
			active = false;
		}
		
		// Flush and close the FileWriter aftter processing has finished
		writer.flush();
		writer.close();
		
		long elapsedTime = System.currentTimeMillis() - time;
		System.out.println("--- FINISHED AFTER " + ((float) elapsedTime / 1000) + " SECONDS");
	}
	
	private void readFiles(int fileIndex) throws FileNotFoundException, IOException, ParseException {
		// Create a new BufferedReader
		try (BufferedReader br = new BufferedReader(new FileReader(input.get(fileIndex)))) {
			// Reset all the variables
			int previousUser = -1;
			long lastTime = -1;
			int sessionId = -1;
			
			// Go through the file line by line
			for(String line; (line = br.readLine()) != null;) {
				// Check if the header line has already been skipped
				if (active) {
					boolean skip = false;
					
					// Split the input line into an array 
			        String[] parts = line.split("\\t");
			        int userId = -1;
			        
			        // Get the user id, skip the line if it is not parsable
			        if (isParsable(parts[0])) userId = Integer.parseInt(parts[0]); else skip = true;
			        
			        String query = parts[1];
			        
			        int position = -1;
			        String url = "na";
			        
			        // If parts has more than 3 entries, the query resulted in a click
			        if (parts.length > 3) {
			        	url = parts[4];
			        	// Get the position of the click, skip the line if it is not parsable
			        	if (isParsable(parts[3])) position = Integer.parseInt(parts[3]); else skip = true;
					}
			        
			        // Get the date and convert it to different formats
			        String dateString = parts[2];
			        Date date = dateFormat.parse(dateString);
			        String javaDate = date.toString();
			        long epoc = date.getTime();
			        
			        // Calculate the time between the current query and the previous query
			        long timeDifference = epoc - lastTime;
			        
			        // If the query is from a new user
			        if (userId != previousUser) {
			        	// Set new values for previousUser and lastTime
			        	previousUser = userId;
			        	lastTime = epoc;
			        	timeDifference = -1; // Set timeDifference to -1 if it's a new user
			        	
			        	// The previous session must have ended because there is a new user
			        	sessionId++;
			        } else if (timeDifference > SESSION_LENGTH) {
			        	// If it's still the same user, but timeDifference is greater than the length of a session, increase sessionId
			        	sessionId++;
			        }
			        
			        // Set the time of the last query to the time of the current query
			        lastTime = epoc;
			        
			        // Replace occurences of the separator in the query and URL
			        query = checkSeparator(query);
			        url = checkSeparator(url);
			        
			        // Create the result string and append it to the file
			        String result = sessionId + SEPARATOR + userId + SEPARATOR + query + SEPARATOR + position + SEPARATOR + url + SEPARATOR + dateString + SEPARATOR + javaDate + SEPARATOR + timeDifference + SEPARATOR + epoc + "\n";
			        if (!skip) writer.append(result);
				} else {
					// Header line has been skipped, set active to true
					active = true;
				}
			}
		}
	}
	
	// Check if a String containts the separator and replaces it
	private String checkSeparator(String input) {
        if (input.contains(SEPARATOR)) {
        	return input.replace(SEPARATOR, SEPARATOR_REPLACEMENT);
        }
        
        return input;
	}
	
	// Check if a String is parsable to int
	private static boolean isParsable(String input){
	    boolean parsable = true;
	    try{
	        Integer.parseInt(input);
	    }catch(NumberFormatException e){
	        parsable = false;
	    }
	    return parsable;
	}

}
