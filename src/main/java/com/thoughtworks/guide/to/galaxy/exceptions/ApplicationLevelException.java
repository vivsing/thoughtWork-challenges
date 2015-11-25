package com.thoughtworks.guide.to.galaxy.exceptions;

import com.thoughtworks.guide.to.galaxy.errorcode.ErrorCode;

/**
 * Wraps all checked exception to this application level exception with the relevant message to back track the exception
 * 
 * @author viveksingh
 */
public class ApplicationLevelException extends Exception {

	/**
	 * default serial id
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * default constructor
	 */
	public ApplicationLevelException() {
		super();
	}
	
	/**
	 * One argument constructor. 
	 * 
	 * @param _message
	 */
	public ApplicationLevelException(String _message) {
		super(_message);
	}
	
	/**
	 * Three argument constructor
	 * 
	 * @param _errorCode
	 * @param _value
	 * @param _exception
	 */
	public ApplicationLevelException (ErrorCode _errorCode, Object _value, Exception _exception) {
		super (String.format(_errorCode.getDescription(), _value), _exception);
	}
}