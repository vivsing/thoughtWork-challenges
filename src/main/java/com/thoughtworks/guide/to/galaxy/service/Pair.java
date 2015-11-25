package com.thoughtworks.guide.to.galaxy.service;

/**
 * Paired roman value to be subtracted
 * 
 * @author viveksingh
 */
public final class Pair {

	/**
	 * the _initial value to be subtracted from the _final value
	 */
	private int _initial;
	
	/**
	 * the _final value
	 */
	private int _final;
	
	/**
	 * Construct a Pair object using the _initial and the _final value
	 * 
	 * @param _initial
	 * @param _final
	 */
	public Pair (final int _initial, final int _final) {
		this._initial = _initial;
		this._final   = _final;
	}
	
	/**
	 * return the result of the subtraction if valid
	 * 
	 * @return
	 */
	public int getSubtractedResult() {
		if (this._final == this._initial) {
			return this._final + this._initial;
		}
		return this._final - this._initial;
	}
	
	/**
	 * @return _initial
	 */
	public int getInitial() {
		return this._initial;
	}
	
	/**
	 * @return _final
	 */
	public int getFinal() {
		return this._final;
	}
	
	@Override public boolean equals(Object obj) {
		boolean isEqual = Boolean.FALSE.booleanValue();
		if (obj instanceof Pair) {
			Pair _pair = (Pair) obj;
			if (this._initial == _pair._initial && this._final == _pair._final) {
				isEqual = Boolean.TRUE.booleanValue();
			}
		}
		return isEqual;
	}
	
	@Override public int hashCode() {
		return System.identityHashCode(this);
	}
}