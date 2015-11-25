package com.thoughtworks.guide.to.galaxy.service;

import com.thoughtworks.guide.to.galaxy.exceptions.InvalidRomanException;
import com.thoughtworks.guide.to.galaxy.rules.GalaxyRules;
import com.thoughtworks.guide.to.galaxy.service.interfaces.Convert;

/**
 * Converts a given valid Roman value to its corresponding numeric representation. If the given Roman representation is 
 * not valid then the conversion will fail with {@link InvalidRomanException}. 
 * 
 * @author viveksingh
 */
public class ConvertRomanToNumeral extends AbstractConversionPolicy implements Convert {

	/**
	 * Default constructor with one argument _galaxyRules
	 * 
	 * @param _galaxyRules
	 */
	public ConvertRomanToNumeral(final GalaxyRules _galaxyRules) {
		super(_galaxyRules);
	}
	
	/**
	 * method that converts the given number to its corresponding numeric representation
	 * If the given roman form is not valid according to rule then the method will throw
	 * {@link InvalidRomanException} 
	 *  
	 * @param roman
	 * @return
	 * @throws InvalidRomanException
	 */
	public float convert(String _roman) throws InvalidRomanException {
		LOGGER.info("Ready to convert the roman \"" +_roman +"\" to its corresponding numeric representation");
		this._roman = _roman;
		
		char[] _romanNumerals = _roman.toUpperCase().toCharArray();
		
		float _value = super.convert(_romanNumerals);
		return _value;
	}
}