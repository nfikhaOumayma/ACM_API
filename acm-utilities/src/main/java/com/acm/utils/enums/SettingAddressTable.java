/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link SettingAddressTable} enum.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public enum SettingAddressTable {

	/** The address type. */
	ADDRESS_TYPE("AddressType"),

	/** The address list. */
	ADDRESS_LIST("AddressList"),

	/** The address list value. */
	ADDRESS_LIST_VALUE("AddressListValue"),

	/** The settings address. */
	SETTINGS_ADDRESS("SettingsAddress");

	/** The table name. */
	private String tableName;

	/**
	 * Instantiates a new setting address table.
	 *
	 * @param tableName the table name
	 */
	SettingAddressTable(String tableName) {

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
