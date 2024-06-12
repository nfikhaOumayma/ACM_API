/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link CustomerType} enum.
 *
 * @author HaythemBenizid
 * @since 1.0.15
 */
public enum CustomerType {

	/** The individual ID ABACUS=1. */
	INDIV(1),

	/** The joint accounts ID ABACUS=2. */
	JOINT_ACCOUNTS(2),

	/** The organisation ID ABACUS=4. */
	ORG(4),

	/** The community solidarity ID ABACUS=8. */
	GRP(8);

	/** The type id. */
	private Integer typeId;

	/**
	 * Instantiates a new customer type.
	 *
	 * @param typeId the type id
	 */
	CustomerType(Integer typeId) {

		this.typeId = typeId;
	}

	/**
	 * Type id.
	 *
	 * @return the integer
	 */
	public Integer typeId() {

		return typeId;
	}

	/**
	 * return Customer type NAME by given type ID and return NULL if ID not exist in ENUM.
	 * 
	 * @author HaythemBenizid
	 * @param typeId the type id
	 * @return the string
	 */
	public static String typeName(Integer typeId) {

		CustomerType[] customerTypes = CustomerType.values();
		for (int i = 0; i < customerTypes.length; i++) {
			if (customerTypes[i].typeId.equals(typeId)) {
				return customerTypes[i].name();
			}
		}
		return null;
	}
}
