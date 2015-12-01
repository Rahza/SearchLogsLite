package analysis;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Scanner;
import java.util.TimeZone;
import java.util.concurrent.TimeUnit;

public class FileProcessorx {
	
	final static Charset ENCODING = StandardCharsets.UTF_8;
	
	private String file;
	private String output;
	private String matrixOutput;
	private String interactionsOutput;
	
	private boolean active = false;
	
	private long startTime;
	
	private FileWriter writer;
	private FileWriter matrixWriter;
	private FileWriter interactionsWriter;
	
	private Date startDay;
	private Date endDay;

	public FileProcessorx(String file, String output, String matrixOutput, String interactionsOutput) {	
		startTime = System.currentTimeMillis();

		this.file = file;
		this.output = output;
		this.matrixOutput = matrixOutput;
		this.interactionsOutput = interactionsOutput;
		
		try {
			startProcessing();
		} catch (IOException | ParseException e) {
			e.printStackTrace();
		}
	}
	
	private void startProcessing() throws IOException, ParseException {
		System.out.println("start processing...");

		writer = new FileWriter(output);
		String header = "sessionId,userId,query,position,url,rawdate,javaDate,lastInteraction,epoc\n";
		writer.append(header);
		
		matrixWriter = new FileWriter(matrixOutput);
		String matrixHeader = createMatrixHeader();
		matrixWriter.append(matrixHeader);
		
		interactionsWriter = new FileWriter(interactionsOutput);
		String interactionsHeader = "userId,timeDifference\n";
		interactionsWriter.append(interactionsHeader);
		
		readFiles();
		
		writer.flush();
	    writer.close();
	    
	    matrixWriter.flush();
	    matrixWriter.close();
	    
	    interactionsWriter.flush();
	    interactionsWriter.close();
	    
	    long stopTime = System.currentTimeMillis();
	    long elapsedTime = (stopTime - startTime) / 1000;
	    
	    System.out.println("finished after " + elapsedTime + " seconds!");
	}
	
	private String createMatrixHeader() {
		String header = "user,";
		
		// Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		Calendar cal = Calendar.getInstance();
		cal.set(2006, 2, 1);
		
		cal.set(Calendar.HOUR_OF_DAY,0);
		cal.set(Calendar.MINUTE,0);
		cal.set(Calendar.SECOND,0);
		cal.set(Calendar.MILLISECOND,0);
		
		startDay = cal.getTime();
		
		for (int i = 0; i < 90; i++) {
			header += cal.getTimeInMillis() + ",";
			
			cal.add(Calendar.DATE, 1);
		}
		
		endDay = cal.getTime();
		header += cal.getTimeInMillis() + "\n";
		
		return header;
	}
	
	private void readFiles() throws IOException, ParseException {
		Path path = Paths.get(file);
		try (Scanner scanner =  new Scanner(path, ENCODING.name())){
			int previousUser = -1;
			int sessionId = 0;
			Date lastDate = null;
			
			Date lastDay = startDay;
			Date previousDay = startDay;
			
			int dayCount = 0;

			while (scanner.hasNextLine()){
				if (active) {
					String line = scanner.nextLine();
					
					String[] parts = line.split("\\t");
					
					int userId = Integer.parseInt(parts[0]);
					
					String query = parts[1];
					
					String dateString = parts[2];
					SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
					Date date = sdf.parse(dateString);
					String javaDate = date.toString();
					long epoc = date.getTime();
					
					int position = -1;
					String url = "";
					
					if (parts.length > 3) {
						position = Integer.parseInt(parts[3]);
						url = parts[4];
					}
					
					Date day = getDay(date);
					
					if (previousUser != userId) {
						if (previousUser != -1) {
							int index = getMatrixIndex(lastDay, previousDay);

							for (int i = 0; i < index; i++) {
								matrixWriter.append(",0");
							}
							
							matrixWriter.append("," + dayCount);
							
							index = getMatrixIndex(endDay, lastDay) + 1;
							
							for (int i = 0; i < index; i++) {
								matrixWriter.append(",0");
							}
						}
						
						previousUser = userId;
						lastDate = date;
						lastDay = startDay;
						previousDay = startDay;
						dayCount = 0;
						sessionId = 0;
						matrixWriter.append("\n");
						matrixWriter.append(userId + "");
					}
					
					long dayDifference = day.getTime() - lastDay.getTime();
					long seconds = dayDifference / 1000;
					
					if (seconds >= 86400) {
						int index = getMatrixIndex(lastDay, previousDay);

						for (int i = 0; i < index; i++) {
							matrixWriter.append(",0");
						}
						
						matrixWriter.append("," + dayCount);
						dayCount = 0;
						previousDay = lastDay;
						lastDay = day;
					}
					
					dayCount++;
					
					long timeDifference = date.getTime() - lastDate.getTime();
					
					if (timeDifference > 1800000) {
						sessionId++;
					} else if (timeDifference > 0) {
						interactionsWriter.append(userId + "," + timeDifference + "\n");
					} else {
						interactionsWriter.append(userId + "," + "0" + "\n");
					}
					
					String result = sessionId + "," + userId + "," + query + "," + position + "," + url + "," + dateString + "," + javaDate + "," + timeDifference + "," + epoc + "\n";
					
			    	writer.append(result);	
			    	
			    	lastDate = date;
				} else {
					scanner.nextLine();
					active = true;
				}
			}      
			int index = getMatrixIndex(lastDay, previousDay);

			for (int i = 0; i < index; i++) {
				matrixWriter.append(",0");
			}
			
			matrixWriter.append("," + dayCount);
			
			index = getMatrixIndex(endDay, lastDay) + 1;
			
			for (int i = 0; i < index; i++) {
				matrixWriter.append(",0");
			}
		}
	}
	
	private int getMatrixIndex(Date day, Date lastDay) {
		long diff = day.getTime() - lastDay.getTime();
	    return (int) (TimeUnit.DAYS.convert(diff, TimeUnit.MILLISECONDS) - 1);
	}
	
	private Date getDay(Date date) {
		//Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
		Calendar cal = Calendar.getInstance();
		cal.setTime(date);
		cal.set(Calendar.HOUR_OF_DAY,0);
		cal.set(Calendar.MINUTE,0);
		cal.set(Calendar.SECOND,0);
		cal.set(Calendar.MILLISECOND,0);

		return cal.getTime();
	}
}