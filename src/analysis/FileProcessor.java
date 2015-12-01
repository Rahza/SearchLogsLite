package analysis;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class FileProcessor {
	
	private static final String HEADER = "sessionId,userId,query,position,url,rawdate,javaDate,lastInteraction,epoc\n";
	private static final int SESSION_LENGTH = 1800000;
	
	private SimpleDateFormat dateFormat;
	
	private String input;
	private String output;
	
	private FileWriter writer;
	
	private boolean active = false;
	
	public FileProcessor(String input, String output) {
		this.input = input;
		this.output = output;
		
		dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
	}
	
	public void startProcessing() throws FileNotFoundException, IOException, ParseException {
		System.out.println("--- PROCESSING STARTED ---");
		
		long time = System.currentTimeMillis();
		
		writer = new FileWriter(output);
		writer.append(HEADER);
		
		readFiles();
		
		writer.flush();
		writer.close();
		
		long elapsedTime = System.currentTimeMillis() - time;
		System.out.println("--- FINISHED AFTER " + ((float) elapsedTime / 1000) + " SECONDS");
	}
	
	private void readFiles() throws FileNotFoundException, IOException, ParseException {
		try (BufferedReader br = new BufferedReader(new FileReader(input))) {
			int previousUser = -1;
			long lastTime = -1;
			int sessionId = -1;
			
			for(String line; (line = br.readLine()) != null;) {
				if (active) {
			        String[] parts = line.split("\\t");
			        
			        int userId = Integer.parseInt(parts[0]);
			        
			        String query = parts[1];
			        
			        int position = -1;
			        String url = "";
			        
			        if (parts.length > 3) {
						position = Integer.parseInt(parts[3]);
						url = parts[4];
					}	        
			        
			        String dateString = parts[2];
			        Date date = dateFormat.parse(dateString);
			        String javaDate = date.toString();
			        long epoc = date.getTime();
			        
			        if (userId != previousUser) {
			        	previousUser = userId;
			        	lastTime = epoc;
			        	sessionId = -1;
			        }
			        
			        long timeDifference = epoc - lastTime;
			        lastTime = epoc;
			        
			        if (timeDifference > SESSION_LENGTH || sessionId < 0) {
			        	sessionId++;
			        }
			        
			        String result = sessionId + "," + userId + "," + query + "," + position + "," + url + "," + dateString + "," + javaDate + "," + timeDifference + "," + epoc + "\n";
			        writer.append(result);
				} else {
					active = true;
				}
			}
		}
	}

}
