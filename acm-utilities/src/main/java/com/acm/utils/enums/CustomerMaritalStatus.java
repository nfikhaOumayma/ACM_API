/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link CustomerMaritalStatus} enum.
 *
 * @author HaythemBenizid
 * @since 1.1.5
 */
public enum CustomerMaritalStatus {

	/** The single. */
	SINGLE("S"),

	/** The married. */
	MARRIED("M");

	/** The short name. */
	private String shortName;

	/**
	 * Instantiates a new customer marital status.
	 *
	 * @param shortName the short name
	 */
	CustomerMaritalStatus(String shortName) {

		this.shortName = shortName;
	}

	/**
	 * Short name.
	 *
	 * @return the string
	 */
	public String shortName() {

		return shortName;
	}
}
