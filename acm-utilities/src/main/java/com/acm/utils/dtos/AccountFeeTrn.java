/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.math.BigDecimal;

/**
 * {@link AccountFeeTrn } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class AccountFeeTrn {

	/** The cu account ID. */
	public Long cuAccountID;

	/** The cu fee ID. */
	public Long cuFeeID;

	/** The description. */
	public String description;

	/** The amount. */
	public BigDecimal amount;

	/**
	 * Gets the cu account ID.
	 *
	 * @return the cuAccountID
	 */
	public Long getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the cuAccountID to set
	 */
	public void setCuAccountID(Long cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the cu fee ID.
	 *
	 * @return the cuFeeID
	 */
	public Long getCuFeeID() {

		return cuFeeID;
	}

	/**
	 * Sets the cu fee ID.
	 *
	 * @param cuFeeID the cuFeeID to set
	 */
	public void setCuFeeID(Long cuFeeID) {

		this.cuFeeID = cuFeeID;
	}

	/**
	 * Gets the description.
	 *
	 * @return the description
	 */
	public String getDescription() {

		return description;
	}

	/**
	 * Sets the description.
	 *
	 * @param description the description to set
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public BigDecimal getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the amount to set
	 */
	public void setAmount(BigDecimal amount) {

		this.amount = amount;
	}

}
