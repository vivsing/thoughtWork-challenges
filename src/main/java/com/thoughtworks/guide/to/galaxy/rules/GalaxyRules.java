package com.thoughtworks.guide.to.galaxy.rules;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Holds the Galaxy rules for converting the Roman representation to the numeric form 
 * 
 * @author viveksingh
 */
public final class GalaxyRules {
	
	/**
	 * map the roman representation with its number
	 */
	private Map<Character, Integer> ROMAN_TO_NUMERAL_MAPPER;
	
	/**
	 * map the rules for subtracting any roman with the other roman. 
	 */
	private Map<Character, List<Character>> ROMANS_SUBTRACTION_POLICY;

	/**
	 * map the rules for maximum number of occurrences in the roman representation
	 */
	private Map<Character, Integer> MAX_REPETITION_ALLOWED;
	
	/**
	 * answer to the unknown question
	 */
	private String unknowQuestionAnswer;
	
	/** Do not allow the creation of {@link GalaxyRules} object anywhere in the application **/
	private GalaxyRules() {}
	
	/**
	 * @return ROMANS_SUBTRACTION_POLICY
	 */
	public Map<Character, List<Character>> getROMANS_SUBTRACTION_POLICY() {
		return Collections.unmodifiableMap(ROMANS_SUBTRACTION_POLICY);
	}

	/**
	 * @return ROMAN_TO_NUMERAL_MAPPER
	 */
	public Map<Character, Integer> getROMAN_TO_NUMERAL_MAPPER() {
		return Collections.unmodifiableMap(ROMAN_TO_NUMERAL_MAPPER);
	}
	
	/**
	 * @return MAX_REPETITION_ALLOWED
	 */
	public Map<Character, Integer> getMAX_REPETITION_ALLOWED() {
		return Collections.unmodifiableMap(MAX_REPETITION_ALLOWED);
	}
	
	/**
	 * @return unknowQuestionAnswer
	 */
	public String getUnknowQuestionAnswer() {
		return unknowQuestionAnswer;
	}
}