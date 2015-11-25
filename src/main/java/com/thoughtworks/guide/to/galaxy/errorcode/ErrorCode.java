package com.thoughtworks.guide.to.galaxy.errorcode;

/**
 * Defines the application level error code with the description of the error.
 * 
 * @author viveksingh
 */
public enum ErrorCode {
	
	MGG001 ("MGG001", "Conversion is not possible for \"%s\" as \"%s\" cannot be repeated more than 3 times"),
	
	MGG002 ("MGG002", "Conversion is not possible for \"%s\" as \"%s\" cannot be subtracted from \"%s\""),
	
	TER001 ("TER001", "Some technical error occurred in the application with message \"%s\"");
	
	/**
	 * Description of the underlying error code
	 */
	private final String _description;
	private final String _errorCode;
	
	/**
	 * private constructor with one argument initializing the string value
	 * 
	 * @param _description
	 */
	private ErrorCode(final String _errorCode, final String _description) {
		this._errorCode = _errorCode;
		this._description = _description;
	}
	
	/**
	 * return the description of the underlying error code
	 * 
	 * @return
	 * 		_description
	 */
	public String getDescription() {
		return this._errorCode + ":: " + this._description;
	}
}