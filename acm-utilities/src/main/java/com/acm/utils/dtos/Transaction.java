/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link Transaction } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class Transaction {

	/** The amount. */
	public String amount;

	/** The transaction type. */
	public int transactionType;

	/**
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public String getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the amount to set
	 */
	public void setAmount(String amount) {

		this.amount = amount;
	}

	/**
	 * Gets the transaction type.
	 *
	 * @return the transactionType
	 */
	public int getTransactionType() {

		return transactionType;
	}

	/**
	 * Sets the transaction type.
	 *
	 * @param transactionType the transactionType to set
	 */
	public void setTransactionType(int transactionType) {

		this.transactionType = transactionType;
	}

}
