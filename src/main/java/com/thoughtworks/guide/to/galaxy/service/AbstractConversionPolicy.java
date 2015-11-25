package com.thoughtworks.guide.to.galaxy.service;

import java.util.ArrayList;
import java.util.HashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.thoughtworks.guide.to.galaxy.errorcode.ErrorCode;
import com.thoughtworks.guide.to.galaxy.exceptions.InvalidRomanException;
import com.thoughtworks.guide.to.galaxy.rules.GalaxyRules;

/**
 * @author viveksingh
 */
public abstract class AbstractConversionPolicy {
	
	protected static final Logger LOGGER = LoggerFactory.getLogger(AbstractConversionPolicy.class);
	
	/**
	 * _galaxyRules instance object
	 */
	protected final GalaxyRules _galaxyRules;
	
	/**
	 * Current roman value
	 */
	protected String _roman;
	
	/**
	 * @param _galaxyRules
	 */
	public AbstractConversionPolicy(final GalaxyRules _galaxyRules) {
		this._galaxyRules = _galaxyRules;
	}
	
	/**
	 * Convert the given roman
	 * 
	 * @param _romanNumerals
	 * @return
	 */
	protected float convert(char[] _romanNumerals) {
		ArrayList<Pair> _pairs = null;
		try {
			_pairs = ValidateAndfindPairs(_romanNumerals);
			LOGGER.info("Conversion of \"" +_roman +"\" to its corresponding numeric representation is successfull");
		} catch (InvalidRomanException _exception) {
			_pairs = null;
			LOGGER.error(_exception.getMessage());
		}
		return calculateValue (_pairs);
	}
	
	/**
	 * validate the given roman based on the rules and find the logical pair
	 * 
	 * @param _romanNumerals
	 * @return
	 * @throws InvalidRomanException
	 */
	private ArrayList<Pair> ValidateAndfindPairs(char[] _romanNumerals) throws InvalidRomanException {
		ArrayList<Pair> _pairs = new ArrayList<Pair>();
		
		int i = 0;
		HashMap<Character, Integer> _manageOccurrances = new HashMap<Character, Integer>(1);
		
		while (i < _romanNumerals.length - 1) {
			Pair _pair = createPair(_romanNumerals[i], _romanNumerals[i + 1], _manageOccurrances);
			_pairs.add(_pair);
			i = _pair.getInitial() > 0 ? (i + 2) : (i + 1);
		}
		return _pairs;
	}
	
	/**
	 * Create a pair object
	 * 
	 * @param _initialRoman
	 * @param _finalRoman
	 * @param _manageOccurrances
	 * @return
	 * @throws InvalidRomanException
	 */
	private Pair createPair(char _initialRoman, char _finalRoman, HashMap<Character, Integer> _manageOccurrances) throws InvalidRomanException {
		Pair _pair = null;
		
		if (!_manageOccurrances.containsKey(_initialRoman)) {
			_manageOccurrances.clear();
		} else if (_manageOccurrances.get(_initialRoman) >= this._galaxyRules.getMAX_REPETITION_ALLOWED().get(_initialRoman)) {
			throw new InvalidRomanException(ErrorCode.MGG001, new String[] {this._roman, String.valueOf(_initialRoman)});
		}
		
		int _initial = this._galaxyRules.getROMAN_TO_NUMERAL_MAPPER().get(_initialRoman);
		int _final   = this._galaxyRules.getROMAN_TO_NUMERAL_MAPPER().get(_finalRoman);

		if (_initial >= _final) {
			if (checkValidOccurance(_manageOccurrances, _initialRoman, _finalRoman)) {
				if (_initial == _final) {
					_pair = new Pair(_final, _initial);
				} else {
					_pair = new Pair(0, _initial);
				}
			}
		} else {
			if (checkValidSubtraction(_initialRoman, _finalRoman)) {
				_pair = new Pair(_initial, _final);
			}
			_manageOccurrances.clear();
		}
		return _pair;
	}
	
	/**
	 * Check the number of times a Roman appears in the given roman and validate it against the galaxy rule
	 * 
	 * @param _manageOccurrances
	 * @param _initialRoman
	 * @return
	 * @throws InvalidRomanException
	 */
	private boolean checkValidOccurance(HashMap<Character, Integer> _manageOccurrances, char _initialRoman, char _finalRoman) throws InvalidRomanException {
		boolean _isValid = Boolean.FALSE.booleanValue();
		int _increamentBy = (_initialRoman == _finalRoman) ? 2 : 1;
		
		if (_manageOccurrances.get(_initialRoman) == null) {
			_manageOccurrances.put(_initialRoman, _increamentBy);
		} else {
			_manageOccurrances.put(_initialRoman, _manageOccurrances.get(_initialRoman) + _increamentBy);
		}
		
		if (_manageOccurrances.get(_initialRoman) <= this._galaxyRules.getMAX_REPETITION_ALLOWED().get(_initialRoman)) {
			_isValid = Boolean.TRUE.booleanValue();
		} else {
			throw new InvalidRomanException(ErrorCode.MGG001, new String[] {this._roman, String.valueOf(_initialRoman)});
		}
		
		return _isValid;
	}
	
	/**
	 * validate the given Roman following the subtraction rule as per the galaxy rules
	 * 
	 * @param _initialRoman
	 * @param _finalRoman
	 * @return
	 * @throws InvalidRomanException
	 */
	private boolean checkValidSubtraction(char _initialRoman, char _finalRoman) throws InvalidRomanException {
		boolean _isValid = Boolean.FALSE.booleanValue();
		
		if (this._galaxyRules.getROMANS_SUBTRACTION_POLICY().get(_initialRoman).contains(_finalRoman)) {
			_isValid = Boolean.TRUE.booleanValue();
		} else {
			throw new InvalidRomanException(ErrorCode.MGG002, new String[] {this._roman, String.valueOf(_initialRoman), String.valueOf(_finalRoman)});
		}
		
		return _isValid;
	}
	
	/**
	 * calculate the numeric representation of the given roman
	 * 
	 * @param _pairs
	 * @return
	 */
	private float calculateValue (ArrayList<Pair> _pairs) {
		float _value = 0;
		if (_pairs != null) {
			for (Pair _pair : _pairs) {
				_value += _pair.getSubtractedResult();
			}
		}
		return _value;
	}
}