package com.thoughtworks.guide.to.galaxy.build;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

public final class BuildFromPattern {
	
	private Map<Object, Object> _isAMapper = new HashMap<Object, Object>();
	private Map<String, Object> _questionAnswerMapper = new LinkedHashMap<String, Object>();
	
	private BuildFromPattern() {}
	
	public static BuildFromPattern getInstance() {
		return new BuildFromPattern();
	}
	
	public void add(Map<Object, Object> _data) {
		for (Map.Entry<Object, Object> _entryObj : _data.entrySet()) {
			_isAMapper.put(_entryObj.getKey(), _entryObj.getValue());
		}
	}
	
	public void addAnswer(String _key, Object _answer) {
		this._questionAnswerMapper.put(_key, _answer);
	}
	
	public Map<Object, Object> getIsAMapper() {
		return this._isAMapper;
	}
	
	public Map<String, Object> getAnswerMapper() {
		return this._questionAnswerMapper;
	}
}