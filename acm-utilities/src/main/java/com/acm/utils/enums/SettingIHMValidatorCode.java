/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link SettingIHMValidatorCode} enum.
 *
 * @author HaythemBenizid
 * @since 1.1.4
 */
public enum SettingIHMValidatorCode {

	/** The required. */
	REQUIRED("required"),

	/** The pattern string. */
	PATTERN_STRING("patternString"),

	/** The pattern reg exp. */
	PATTERN_REG_EXP("patternRegExp"),

	/** The email. */
	EMAIL("email"),

	/** The min. */
	MIN("min"),

	/** The max. */
	MAX("max");

	/** The code. */
	private String code;

	/**
	 * Instantiates a new setting IHM VALIDATOR code.
	 *
	 * @param code the code
	 */
	SettingIHMValidatorCode(String code) {

		this.code = code;
	}

	/**
	 * Code.
	 *
	 * @return the string
	 */
	public String code() {

		return code;
	}
}
