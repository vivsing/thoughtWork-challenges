package com.thoughtworks.guide.to.galaxy.main;

import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.thoughtworks.guide.to.galaxy.build.BuildFromPattern;
import com.thoughtworks.guide.to.galaxy.exceptions.ApplicationLevelException;
import com.thoughtworks.guide.to.galaxy.input.parser.ParseInputFromStream;
import com.thoughtworks.guide.to.galaxy.reader.InputReader;
import com.thoughtworks.guide.to.galaxy.rules.ConversionRuleParser;
import com.thoughtworks.guide.to.galaxy.rules.GalaxyRules;
import com.thoughtworks.guide.to.galaxy.service.interfaces.Parser;

/**
 * Main class
 * 
 * @author viveksingh
 */
public class MerchantOutputerMain {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(MerchantOutputerMain.class);
	
	private GalaxyRules _galaxyRules;
	
	/**
	 * Default constructor
	 */
	public MerchantOutputerMain() {
		this._galaxyRules = ConversionRuleParser.getInstance().getGalaxyRules();
	}
	
	// ~~ main method
	public static void main(String...strings) {
		String _fileName = "";
		// check if any thing is passed from the console
		// the file path should be absolute to enable the reading
		if (strings.length > 0) {
			_fileName = strings[0];
		}
		
		if (_fileName == "") {
			_fileName = "input.txt";
		}
		
		MerchantOutputerMain _mainOutputter = new MerchantOutputerMain();
		try {
			
			// create the stream object
			InputStream _inputStream = InputReader.getStream(_fileName);
			Parser _parser = new ParseInputFromStream(_inputStream, _mainOutputter.getGalaxyRules());
			
			BuildFromPattern _buildFromPattern = _parser.parse();
			_mainOutputter.writeToConsole(_buildFromPattern);
			
		} catch (ApplicationLevelException _appException) {
			LOGGER.error(_appException.getMessage());
		} finally {
			
		}
	}
	
	/**
	 * Write out put to the console
	 * 
	 * @param _buildFromPattern
	 */
	private void writeToConsole(BuildFromPattern _buildFromPattern) {
		PrintWriter _writer = new PrintWriter(System.out);
		Map<String, Object> _answer = _buildFromPattern.getAnswerMapper();
		
		for (Map.Entry<String, Object> _entry : _answer.entrySet()) {
			if (_entry.getKey() != null) {
				_writer.println(_entry.getKey() + " is " + (Float) _entry.getValue());
			} else {
				_writer.println(_entry.getValue());
			}
		}
		_writer.flush();
		_writer.close();
	}
	
	private GalaxyRules getGalaxyRules() {
		return this._galaxyRules;
	}
}