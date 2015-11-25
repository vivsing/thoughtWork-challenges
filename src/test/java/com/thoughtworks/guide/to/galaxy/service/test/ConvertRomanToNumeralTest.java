package com.thoughtworks.guide.to.galaxy.service.test;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.thoughtworks.guide.to.galaxy.rules.ConversionRuleParser;
import com.thoughtworks.guide.to.galaxy.rules.GalaxyRules;
import com.thoughtworks.guide.to.galaxy.service.interfaces.Convert;
import com.thoughtworks.guide.to.galaxy.service.ConvertRomanToNumeral;

/**
 * Test the service for converting the given Roman to its corresponding Numeric representation
 * 
 * @author viveksingh
 */
public class ConvertRomanToNumeralTest {

	private Convert _convert;
	
	@Before
	public void beforeClass() {
		GalaxyRules _galaxyRules = ConversionRuleParser.getInstance().getGalaxyRules();
		this._convert = new ConvertRomanToNumeral(_galaxyRules);
	}
	
	/**
	 * Test 1 for the valid Roman representation
	 *  
	 * @throws Exception
	 */
	@Test
	public void validRomanConversionTest_1() throws Exception {
		String roman = "MCMXLIV";
		Float value = this._convert.convert(roman);
		Assert.assertEquals(new Float(1944), value);
	}
	
	/**
	 * Test 2 for the valid Roman representation
	 * 
	 * @throws Exception
	 */
	@Test
	public void validRomanConversionTest_2() throws Exception {
		String roman = "XXXIX";
		Float value = this._convert.convert(roman);
		Assert.assertEquals(new Float(39), value);
	}
	
	/**
	 * Test 1 for the roman representation not following the subtraction rule
	 * 
	 * @throws Exception
	 */
	@Test
	public void invalidSubtractionRuleTest_1() throws Exception {
		String roman = "CDXMIL";
		Float value = this._convert.convert(roman);
		Assert.assertEquals(new Float(0), value);
	}
	
	/**
	 * Test 2 for the roman representation not following the subtraction rule
	 * 
	 * @throws Exception
	 */
	@Test
	public void invalidSubtractionRuleTest_2() throws Exception {
		String roman = "VXIL";
		Float value = this._convert.convert(roman);
		Assert.assertEquals(new Float(0), value);
	}
	
	/**
	 * Test 1 for the roman representation not following the rules of the galaxy as the number of times
	 * one roman should appear
	 * 
	 * @throws Exception
	 */
	@Test
	public void invalidOccuranceRuleTest_1() throws Exception {
		String roman = "XXXXI";
		Float value = this._convert.convert(roman);
		Assert.assertEquals(new Float(0), value);
	}
	
	/**
	 * Test 2 for the roman representation not following the rules of the galaxy as the number of times
	 * one roman should appear
	 * 
	 * @throws Exception
	 */
	@Test
	public void invalidOccuranceRuleTest_2() throws Exception {
		String roman = "CCCCM";
		Float value = this._convert.convert(roman);
		Assert.assertEquals(new Float(0), value);
	}
}