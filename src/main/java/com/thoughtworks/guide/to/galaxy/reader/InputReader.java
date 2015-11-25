package com.thoughtworks.guide.to.galaxy.reader;

import java.io.FileInputStream;
import java.io.InputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.thoughtworks.guide.to.galaxy.errorcode.ErrorCode;
import com.thoughtworks.guide.to.galaxy.exceptions.ApplicationLevelException;
import com.thoughtworks.guide.to.galaxy.input.parser.ParseInputFromStream;
import com.thoughtworks.guide.to.galaxy.rules.ConversionRuleParser;
import com.thoughtworks.guide.to.galaxy.service.interfaces.Parser;
/**
 * Read the input file to extract the rules and questions
 * 
 * @author viveksingh
 */
public final class InputReader {

	private final String _inputFileName;
	private static final Logger LOGGER = LoggerFactory.getLogger(InputReader.class);
	private static InputStream _stream;
	
	/**
	 * Constructor with one argument
	 * 
	 * @param _inputFileName
	 * @throws ApplicationLevelException 
	 */
	private InputReader(String _inputFileName) throws ApplicationLevelException {
		this._inputFileName = _inputFileName;
		
		read();
	}
	
	public static InputStream getStream(String _inputFileName) throws ApplicationLevelException {
		new InputReader(_inputFileName);
		return _stream;
	}
	
	/**
	 * start reading the input file
	 * @throws ApplicationLevelException 
	 */
	private void read() throws ApplicationLevelException {
		LOGGER.info("Start reading the input file \"" +this._inputFileName+"\"");
	    _stream = InputReader.class.getClassLoader().getResourceAsStream(this._inputFileName);
		if (_stream == null) {
			try {
				_stream = new FileInputStream(this._inputFileName);
			} catch (Exception _exception) {
				throw new ApplicationLevelException(ErrorCode.TER001, _exception.getMessage(), _exception);
			}
		}
	}
}