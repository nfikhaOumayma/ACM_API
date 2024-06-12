/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.List;

/**
 * {@link AccountChildTrn } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class AccountChildTrn {

	/** The cu account ID. */
	public Long cuAccountID;

	/** The expected amount CR. */
	public double expectedAmountCR;

	/** The expected amount DR. */
	public double expectedAmountDR;

	/** The transactions. */
	public List<Transaction> transactions;

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
	 * Gets the expected amount CR.
	 *
	 * @return the expectedAmountCR
	 */
	public double getExpectedAmountCR() {

		return expectedAmountCR;
	}

	/**
	 * Sets the expected amount CR.
	 *
	 * @param expectedAmountCR the expectedAmountCR to set
	 */
	public void setExpectedAmountCR(double expectedAmountCR) {

		this.expectedAmountCR = expectedAmountCR;
	}

	/**
	 * Gets the expected amount DR.
	 *
	 * @return the expectedAmountDR
	 */
	public double getExpectedAmountDR() {

		return expectedAmountDR;
	}

	/**
	 * Sets the expected amount DR.
	 *
	 * @param expectedAmountDR the expectedAmountDR to set
	 */
	public void setExpectedAmountDR(double expectedAmountDR) {

		this.expectedAmountDR = expectedAmountDR;
	}

	/**
	 * Gets the transactions.
	 *
	 * @return the transactions
	 */
	public List<Transaction> getTransactions() {

		return transactions;
	}

	/**
	 * Sets the transactions.
	 *
	 * @param transactions the transactions to set
	 */
	public void setTransactions(List<Transaction> transactions) {

		this.transactions = transactions;
	}

}
