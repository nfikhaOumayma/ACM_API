/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link SettingUDFTable} enum.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public enum SettingUDFTable {

	/** The user defined field list values. */
	USER_DEFINED_FIELD_LIST_VALUES("UserDefinedFieldListValues"),

	/** The user defined field lists. */
	USER_DEFINED_FIELD_LISTS("UserDefinedFieldLists"),

	/** The user defined fields. */
	USER_DEFINED_FIELDS("UserDefinedFields"),

	/** The user defined field group. */
	USER_DEFINED_FIELD_GROUP("UserDefinedFieldGroup");

	/** The table name. */
	private String tableName;

	/**
	 * Instantiates a new setting address table.
	 *
	 * @param tableName the table name
	 */
	SettingUDFTable(String tableName) {

		this.tableName = tableName;
	}

	/**
	 * Table name.
	 *
	 * @return the string
	 */
	public String tableName() {

		return tableName;
	}
}
