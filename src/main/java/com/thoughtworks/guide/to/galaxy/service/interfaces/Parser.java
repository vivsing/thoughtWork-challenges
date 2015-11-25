package com.thoughtworks.guide.to.galaxy.service.interfaces;

import java.util.regex.Pattern;

import com.thoughtworks.guide.to.galaxy.build.BuildFromPattern;

/**
 * @author viveksingh
 */
public interface Parser {

	/**
	 * Pattern for the type glob is I or prok is V
	 */
	public static final Pattern _ISAPATTERN = Pattern.compile("^([a-z]+) is ([I|V|X|L|C|D|M])$");
	
	/**
	 * Pattern for the type glob glob Silver is 34 Credits or glob prok Gold is 57800 Credits
	 */
	public static final Pattern _CREDITPATTERN = Pattern.compile("((?:[a-z]+ )+)([A-Z]\\w+) is (\\d+) ([A-Z]\\w+)$");
	
	/**
	 * pattern for the type how much is pish tegj glob glob ? or how many Credits is glob prok Gold ?
	 */
	public static final Pattern _QUESTIONPATTERN = Pattern.compile("([h|H]ow\\s\\b(much|many Credits))\\s[a-zA-Z]*\\s([a-zA-Z\\s]{1,})");
	
	/**
	 * Parses the data
	 */
	public BuildFromPattern parse();
}