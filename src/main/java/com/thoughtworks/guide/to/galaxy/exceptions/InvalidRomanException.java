package com.thoughtworks.guide.to.galaxy.exceptions;

import com.thoughtworks.guide.to.galaxy.errorcode.ErrorCode;

/**
 * Throws {@link InvalidRomanException} when a non-valid Roman representation is being attempted to be converted
 * to the numeric value. This exception class will wrap the {@link ErrorCode} for the underlying error
 * 
 * @author viveksingh
 */
public class InvalidRomanException extends Exception {

	/**
	 * default serial version id
	 */
	private static final long serialVersionUID = 1L;
	
	/**
	 * _errorCode instance
	 */
	private ErrorCode _errorCode;
	
	/**
	 * Construct a {@link InvalidRomanException} object without any message
	 */
	public InvalidRomanException() {
		super();
	}
	
	/**
	 * Construct a {@link InvalidRomanException} object for the given message
	 * 
	 * @param _message
	 */
	public InvalidRomanException(String _message) {
		super(_message);
	}
	
	/**
	 * Construct a {@link InvalidRomanException} object for the given errorcode and value for which the
	 * exception occurred
	 * 
	 * @param _errorCode
	 * @param _value
	 */
	public InvalidRomanException(ErrorCode _errorCode, Object[] _value) {
		super(String.format(_errorCode.getDescription(), _value));
		this._errorCode = _errorCode;
	}
	
	/**
	 * @return _errorCode
	 */
	public ErrorCode getErrorCode() {
		return this._errorCode;
	}
}