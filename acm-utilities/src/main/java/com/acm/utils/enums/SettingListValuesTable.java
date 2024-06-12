/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.enums;

/**
 * The {@link SettingListValuesTable} enum.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public enum SettingListValuesTable {

	/** The cu product loan reasons -- Loan Details | Loan Reason. */
	CU_PRODUCT_LOAN_REASONS("CUProductLoanReasons"),

	/** The cu loan guarantor source -- Loan Details | Guarantor Source. */
	CU_LOAN_GUARANTOR_SOURCE("CULoanGuarantorSource"),

	/** The cu loan source of funds -- Loan Details | Source of Funds. */
	CU_LOAN_SOURCE_OF_FUNDS("CULoanSourceOfFunds"),

	/** The cu loan refinance reason -- Loan Details | Refinance Reason. */
	CU_LOAN_REFINANCE_REASON("CULoanRefinanceReason"),

	/** The branches. */
	BRANCHES("Branches"),

	/** The relationship. */
	RELATIONSHIP("Relationship"),

	/** The sector (Industry). */
	SECTOR("Industry"),

	/** The cu role. */
	CU_ROLE("CURole"),

	/** The deferred period type. */
	DEFERRED_PERIOD_TYPE("DeferredPeriodType"),

	/** The cu collateral type. */
	CU_COLLATERAL_TYPE("CULoanCollateralType"),
	/** The cu collateral type. */
	FEES("CUFee");

	/** The table name. */
	private String tableName;

	/**
	 * Instantiates a new setting list values table.
	 *
	 * @param tableName the table name
	 */
	SettingListValuesTable(String tableName) {

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
