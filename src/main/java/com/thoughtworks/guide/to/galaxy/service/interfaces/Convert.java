package com.thoughtworks.guide.to.galaxy.service.interfaces;

import com.thoughtworks.guide.to.galaxy.exceptions.InvalidRomanException;

/**
 * A simple contract for conversion of Roman to numeral. 
 * 
 * <h1>
 * {@link #convert(String)}
 * </h1>
 * 
 * @author viveksingh
 */
public interface Convert {

	/**
	 * method that converts the given number to its corresponding numeric representation
	 * If the given roman form is not valid according to rule then the method will throw
	 * {@link InvalidRomanException} 
	 *  
	 * @param roman
	 * @return
	 * @throws InvalidRomanException
	 */
	public float convert(String roman) throws InvalidRomanException;
}