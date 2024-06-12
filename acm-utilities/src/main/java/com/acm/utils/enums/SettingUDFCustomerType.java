/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

import java.util.ArrayList;
import java.util.List;

/**
 * The {@link SettingUDFCustomerType} enum.
 *
 * @author HaythemBenizid
 * @since 1.0.15
 */
public enum SettingUDFCustomerType {

	/** The 1 : Individuel. */
	INDIV(1),

	/** The 4 : Organisation. */
	ORG(4),

	/** The 5 : Individuel & organisation. */
	INDIV_ORG(5),

	/** The 8 : Groupe. */
	GRP(8),

	/** The 9 : Individuel & groupe. */
	INDIV_GRP(9),

	/** The 12 : organisation et groupe. */
	ORG_GRP(12),

	/** The 13 : All types. */
	ALL(13),

	/** The UDF for LOAN. */
	LOAN(0);

	/** The type id. */
	private Integer typeId;

	/**
	 * Instantiates a new setting UDF customer type.
	 *
	 * @param typeId the type id
	 */
	SettingUDFCustomerType(Integer typeId) {

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
	 * return Setting UDF Customer type NAME by given type ID and return NULL if ID not exist in
	 * ENUM.
	 * 
	 * @author HaythemBenizid
	 * @param typeId the type id
	 * @return the string
	 */
	public static String typeName(Integer typeId) {

		SettingUDFCustomerType[] customerTypes = SettingUDFCustomerType.values();
		for (int i = 0; i < customerTypes.length; i++) {
			if (customerTypes[i].typeId.equals(typeId)) {
				return customerTypes[i].name();
			}
		}
		return null;
	}

	/**
	 * return Setting UDF Customer type IDs if contains given type NAME and return empty
	 * {@link List} if name not exist in ENUM.
	 *
	 * @author HaythemBenizid
	 * @param type the type
	 * @return the list
	 */
	public static List<Integer> typeIds(String type) {

		SettingUDFCustomerType[] customerTypes = SettingUDFCustomerType.values();
		List<Integer> ids = new ArrayList<>();
		ids.add(13);
		for (int i = 0; i < customerTypes.length; i++) {
			if (customerTypes[i].name().contains(type)) {
				ids.add(customerTypes[i].typeId());
			}
		}
		return ids;
	}
}
