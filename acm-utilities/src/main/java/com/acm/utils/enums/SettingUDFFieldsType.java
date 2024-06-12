/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link SettingUDFFieldsType} enum (mapping with ABACUS).
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public enum SettingUDFFieldsType {

	/** The 1 : Texte. */
	TEXTE(1),

	/** The 2 : Numerique. */
	NUMERIQUE(2),

	/** The 4 : Date. */
	DATE(4),

	/** The 5 : liste. */
	LIST(5);

	/** The type id. */
	private Integer typeId;

	/**
	 * Instantiates a new setting UDF fields type.
	 *
	 * @param typeId the type id
	 */
	SettingUDFFieldsType(Integer typeId) {

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
	 * return Setting UDF Fields type NAME by given type ID and return NULL if ID not exist in ENUM.
	 * 
	 * @author HaythemBenizid
	 * @param typeId the type id
	 * @return the string
	 */
	public static String typeName(Integer typeId) {

		SettingUDFFieldsType[] customerTypes = SettingUDFFieldsType.values();
		for (int i = 0; i < customerTypes.length; i++) {
			if (customerTypes[i].typeId.equals(typeId)) {
				return customerTypes[i].name();
			}
		}
		return null;
	}
}
