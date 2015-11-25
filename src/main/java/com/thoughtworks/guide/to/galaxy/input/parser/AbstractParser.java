package com.thoughtworks.guide.to.galaxy.input.parser;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.thoughtworks.guide.to.galaxy.errorcode.ErrorCode;
import com.thoughtworks.guide.to.galaxy.exceptions.ApplicationLevelException;
import com.thoughtworks.guide.to.galaxy.rules.GalaxyRules;
import com.thoughtworks.guide.to.galaxy.service.ConvertRomanToNumeral;
import com.thoughtworks.guide.to.galaxy.service.interfaces.Convert;
import com.thoughtworks.guide.to.galaxy.service.interfaces.Parser;

/**
 * Abstract parser class which parses the data from the given {@link InputStream}
 * 
 * @author viveksingh
 */
public abstract class AbstractParser implements Parser {
	
	protected static final Logger LOGGER = LoggerFactory.getLogger(AbstractParser.class);
	
	/**
	 * galaxy rules
	 */
	protected final GalaxyRules _galaxyRules;
	
	/**
	 * make sure the compiler optimizer creates an instance of _inputStream
	 */
	protected final InputStream _inputStream;
	
	/**
	 * _convert Object
	 */
	protected Convert _convert;
	
	/**
	 * holds the data from the input file
	 */
	protected ArrayList<String> _inputLines = new ArrayList<String>();
	
	/**
	 * Default constructor
	 * 
	 * @param _inputStream
	 * @param _galaxyRules
	 * @throws ApplicationLevelException
	 */
	public AbstractParser(final InputStream _inputStream, final GalaxyRules _galaxyRules) throws ApplicationLevelException {
		this._inputStream = _inputStream;
		this._galaxyRules = _galaxyRules;
		_convert = new ConvertRomanToNumeral(_galaxyRules);
		readFromStream();
	}
	
	/**
	 * method that reads the data from the stream object
	 * 
	 * @throws ApplicationLevelException
	 */
	private void readFromStream() throws ApplicationLevelException {
		LOGGER.info("Start reading from the stream...");
		BufferedReader _bufferedReader = new BufferedReader(new InputStreamReader(_inputStream));
		
		String _line = null;
		try {
			while ((_line = _bufferedReader.readLine()) != null) {
				_inputLines.add(_line);
			}
		} catch (Exception _exception) {
			throw new ApplicationLevelException(ErrorCode.TER001, _exception.getMessage(), _exception);
		} finally {
			closeResources(_bufferedReader);
		}
	}
	
	/**
	 * close all open resources
	 * 
	 * @param _bufferedReader
	 */
	private void closeResources(BufferedReader _bufferedReader) {
		try {
			if (_bufferedReader != null) {
				_bufferedReader.close();
			}
			
			if (this._inputStream != null) {
				this._inputStream.close();
			}
		} catch (IOException _ioexception) {
			LOGGER.error("Error occurred while closing the resources " +_ioexception.getMessage());
		}
	}
	
	/**
	 * Returns the {@link PatternMapper} associated with the current line
	 * 
	 * @param _line
	 * @return
	 */
	protected PatternMapper checkPattern(String _line) {
		PatternMapper _patternMapper = null;
		if (Parser._ISAPATTERN.matcher(_line).find()) {
			_patternMapper = new PatternMapper(Parser._ISAPATTERN, Mapper.IS_A_PATTERN, _line);
		} else if (Parser._CREDITPATTERN.matcher(_line).find()) {
			_patternMapper = new PatternMapper(Parser._CREDITPATTERN, Mapper.CREDITPATTERN, _line);
		} else if (Parser._QUESTIONPATTERN.matcher(_line).find()) {
			_patternMapper = new PatternMapper(Parser._QUESTIONPATTERN, Mapper.QUESTIONPATTERN, _line);
		} else {
			_patternMapper = new PatternMapper(null, Mapper.NOVALIDPATTERN, _line);
		}
		return _patternMapper;
	}
	
	/**
	 * Mapper enum to map the input data with the Pattern
	 * 
	 * @author viveksingh
	 */
	protected enum Mapper {
		IS_A_PATTERN {
			@Override
			public HashMap<Object, Object> getData(String _line, Pattern _pattern) {
				HashMap<Object, Object> _data = new HashMap<Object, Object>();
				java.util.regex.Matcher _matcher = _pattern.matcher(_line);
				if (_matcher.find()) {
					_data.put(_matcher.group(1).trim(), (Character)_matcher.group(2).trim().charAt(0));
				}
				return _data;
			}
		},
		CREDITPATTERN {
			@Override
			public HashMap<Object, Object> getData(String _line, Pattern _pattern) {
				HashMap<Object, Object> _data = new HashMap<Object, Object>();
				java.util.regex.Matcher _matcher = _pattern.matcher(_line);
				if (_matcher.find()) {
					String[] _sequence = (_matcher.group(1) + _matcher.group(2)).split(" ");
					List<String> _sequenceLst = Arrays.asList(_sequence);
					_data.put(_sequenceLst, _matcher.group(3).trim());
				}
				return _data;
			}
		},
		QUESTIONPATTERN {
			@Override
			public HashMap<Object, Object> getData(String _line, Pattern _pattern) {
				HashMap<Object, Object> _data = new HashMap<Object, Object>();
				java.util.regex.Matcher _matcher = _pattern.matcher(_line);
				if (_matcher.find()) {
					_data.put(_line, _matcher.group(3).trim());
				}
				return _data;
			}
		},
		
		NOVALIDPATTERN {
			@Override
			public HashMap<Object, Object> getData(String _line, Pattern _pattern) {
				HashMap<Object, Object> _data = new HashMap<Object, Object>();
				_data.put(_line, null);
				return _data;
			}
		};
		
		public abstract HashMap<Object, Object> getData(String _line, Pattern _pattern);
	}
	
	/**
	 * static inner class that maps the Pattern associated with the current line
	 * 
	 * @author viveksingh
	 */
	protected static class PatternMapper {
		private Pattern _pattern;
		private Mapper _mapper;
		private HashMap<Object, Object> _data;
		
		public PatternMapper(Pattern _pattern, Mapper _mapper, String _line) {
			this._pattern = _pattern;
			this._mapper = _mapper;
			
			this._data = this._mapper.getData(_line, this._pattern);
		}
		
		public Mapper getMapper() {
			return this._mapper;
		}
		
		public HashMap<Object, Object> getData() {
			return this._data;
		}
	}
}