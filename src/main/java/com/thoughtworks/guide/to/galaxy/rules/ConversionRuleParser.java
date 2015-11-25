package com.thoughtworks.guide.to.galaxy.rules;

import java.nio.file.Files;
import java.nio.file.Paths;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.GsonBuilder;
import com.thoughtworks.guide.to.galaxy.errorcode.ErrorCode;
import com.thoughtworks.guide.to.galaxy.exceptions.ApplicationLevelException;
/**
 * This class initializes the galazy rules for conversion of Roman representation to its corresponding human readable numeric representation.
 * 
 * The class may throw an {@link ApplicationLevelException} if any form of technical exception occurred.
 * 
 * @author viveksingh
 */
public final class ConversionRuleParser {
	
	private static final ConversionRuleParser _MAPPER_INSTANCE = new ConversionRuleParser();
	private static final Logger LOGGER = LoggerFactory.getLogger(ConversionRuleParser.class);
	
	private GalaxyRules _galaxyRules;
	
	public static final ConversionRuleParser getInstance() {
		try {
			initialize("galaxy-rules.json");
		} catch (ApplicationLevelException _appException) {
			LOGGER.error(_appException.getMessage());
		}
		return _MAPPER_INSTANCE;
	}
	
	private static void initialize(String _fileName) throws ApplicationLevelException {
		com.google.gson.Gson _gson = new GsonBuilder().setPrettyPrinting().create();
		
		LOGGER.info("Start initializing the galaxy rules for conversion..");
		
		try {
			// read json file data as string
			String _fileData = new String(Files.readAllBytes(Paths.get("src/main/resources/" + _fileName)));
			_MAPPER_INSTANCE._galaxyRules = _gson.fromJson(_fileData, GalaxyRules.class);
		} catch (Exception _exception) {
			throw new ApplicationLevelException(ErrorCode.TER001, _exception.getMessage(), _exception);
		}
		
		LOGGER.info("Initialization of galaxy rule for conversion is finished..");
	}
	
	/**
	 * @return _galaxyRules
	 */
	public GalaxyRules getGalaxyRules() {
		return _MAPPER_INSTANCE._galaxyRules;
	}
}