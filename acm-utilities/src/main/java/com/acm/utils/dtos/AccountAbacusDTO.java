/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link AccountAbacusDTO } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class AccountAbacusDTO {

	/** The Account ID. */
	private long AccountID;

	/** The Number. */
	private String Number;

	/**
	 * Gets the account ID.
	 *
	 * @return the accountID
	 */
	public long getAccountID() {

		return AccountID;
	}

	/**
	 * Sets the account ID.
	 *
	 * @param accountID the accountID to set
	 */
	public void setAccountID(long accountID) {

		AccountID = accountID;
	}

	/**
	 * Gets the number.
	 *
	 * @return the number
	 */
	public String getNumber() {

		return Number;
	}

	/**
	 * Sets the number.
	 *
	 * @param number the number to set
	 */
	public void setNumber(String number) {

		Number = number;
	}

}
