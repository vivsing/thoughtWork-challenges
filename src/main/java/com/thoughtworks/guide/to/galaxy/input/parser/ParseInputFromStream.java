package com.thoughtworks.guide.to.galaxy.input.parser;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.thoughtworks.guide.to.galaxy.build.BuildFromPattern;
import com.thoughtworks.guide.to.galaxy.exceptions.ApplicationLevelException;
import com.thoughtworks.guide.to.galaxy.exceptions.InvalidRomanException;
import com.thoughtworks.guide.to.galaxy.rules.GalaxyRules;

/**
 * Parse the input file from the stream object and extract the data from the file
 * 
 * @author viveksingh
 */
public class ParseInputFromStream extends AbstractParser {
	
	/**
	 * Default constructor using {@link InputStream} instance object
	 * 
	 * @param _inputStream
	 */
	public ParseInputFromStream(final InputStream _inputStream, final GalaxyRules _galaxyRules) throws ApplicationLevelException {
		super(_inputStream, _galaxyRules);
	}

	/**
	 * parses the data
	 */
	public BuildFromPattern parse() {
		final ArrayList<String> _lines = this._inputLines;
		
		// start building from pattern
		BuildFromPattern _buildFromPattern = BuildFromPattern.getInstance();
		for (String _line : _lines) {
			parse (_line, _buildFromPattern);
		}
		return _buildFromPattern;
	}
	
	private void parse(String _lines, BuildFromPattern _buildFromPattern) {
		PatternMapper _patternMapper = checkPattern(_lines);
		switch (_patternMapper.getMapper()) {
			case IS_A_PATTERN:
				_buildFromPattern.add(_patternMapper.getData());
				break;
			case CREDITPATTERN:
				buildFromCredit(_patternMapper.getData(), _buildFromPattern);
				break;
			case QUESTIONPATTERN:
				buildAnswer(_patternMapper.getData(), _buildFromPattern);
				break;
			case NOVALIDPATTERN:
				buildAnswer(_patternMapper.getData(), _buildFromPattern);
				break;
		}
	}
	
	@SuppressWarnings({ "unchecked" })
	private void buildFromCredit(HashMap<Object, Object> _data, BuildFromPattern _buildFromPattern) {
		for (Map.Entry<Object, Object> _entry  : _data.entrySet()) {
			List<String> _tokens = ((List<String>) _entry.getKey());
			Integer _value = Integer.parseInt((String)_entry.getValue());
			
			findMissingIfAny(_tokens, _buildFromPattern, _value);
		}
	}
	
	private void findMissingIfAny(List<String> _tokens, BuildFromPattern _buildFromPattern, Integer _value) {
		String _roman = "";
		for (String _token : _tokens) {
			if (_buildFromPattern.getIsAMapper().containsKey(_token)) {
				_roman += _buildFromPattern.getIsAMapper().get(_token);
			} else {
				try {
					float _convertedValue = _convert.convert(_roman);
					// value of the missing token
					float value = (float) (_value / _convertedValue);
					_buildFromPattern.getIsAMapper().put(_token, value);
				} catch (InvalidRomanException _exception) {
					LOGGER.error(_exception.getMessage());
				}
			}
		}
	}
	
	private void buildAnswer(HashMap<Object, Object> _data, BuildFromPattern _buildFromPattern) {
		for (Map.Entry<Object, Object> _entryData : _data.entrySet()) {
			String _value = (String) _entryData.getValue();
			find(_value, _buildFromPattern);				
		}
	}
	
	private void find(String _value, BuildFromPattern _buildFromPattern) {
		String _roman = "";
		float _multiplier = 1;
		String[] _tokens = _value.split(" ");
		
		for (String _token : _tokens) {
			Object _isAValue = _buildFromPattern.getIsAMapper().get(_token);
			
			if (_isAValue instanceof Character) {
				_roman += _isAValue;
			} else if (_isAValue instanceof Float) {
				_multiplier = (Float) _isAValue;
			}
		}
		try {
			if (_roman != "") {
				float _convertedValue = this._convert.convert(_roman);
				float _answer = _convertedValue * _multiplier;
				
				_buildFromPattern.addAnswer(_value, _answer);
			} else {
				_buildFromPattern.addAnswer(null, this._galaxyRules.getUnknowQuestionAnswer());
			}
		} catch(InvalidRomanException _exception) {
			LOGGER.error(_exception.getMessage());
		}
	}
}