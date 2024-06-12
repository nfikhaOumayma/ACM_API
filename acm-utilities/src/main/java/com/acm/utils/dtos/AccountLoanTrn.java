/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.List;

/**
 * {@link AccountLoanTrn } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class AccountLoanTrn {

	/** The account child trn. */
	public List<AccountChildTrn> accountChildTrn;

	/** The account fee trn. */
	public List<AccountFeeTrn> accountFeeTrn;

	/** The transactions. */
	public List<Transaction> transactions;

	/** The parent account time stamp. */
	public Object parentAccountTimeStamp;

	/** The cu account ID. */
	public Long cuAccountID;

	/** The expected amount CR. */
	public double expectedAmountCR;

	/** The expected amount DR. */
	public double expectedAmountDR;

	/**
	 * Gets the account child trn.
	 *
	 * @return the accountChildTrn
	 */
	public List<AccountChildTrn> getAccountChildTrn() {

		return accountChildTrn;
	}

	/**
	 * Sets the account child trn.
	 *
	 * @param accountChildTrns the accountChildTrn to set
	 */
	public void setAccountChildTrn(List<AccountChildTrn> accountChildTrns) {

		this.accountChildTrn = accountChildTrns;
	}

	/**
	 * Gets the account fee trn.
	 *
	 * @return the accountFeeTrn
	 */
	public List<AccountFeeTrn> getAccountFeeTrn() {

		return accountFeeTrn;
	}

	/**
	 * Sets the account fee trn.
	 *
	 * @param accountFeeTrns the accountFeeTrn to set
	 */
	public void setAccountFeeTrn(List<AccountFeeTrn> accountFeeTrns) {

		this.accountFeeTrn = accountFeeTrns;
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
	 * @param transactionsLoan the transactions to set
	 */
	public void setTransactions(List<Transaction> transactionsLoan) {

		this.transactions = transactionsLoan;
	}

	/**
	 * Gets the parent account time stamp.
	 *
	 * @return the parentAccountTimeStamp
	 */
	public Object getParentAccountTimeStamp() {

		return parentAccountTimeStamp;
	}

	/**
	 * Sets the parent account time stamp.
	 *
	 * @param parentAccountTimeStamp the parentAccountTimeStamp to set
	 */
	public void setParentAccountTimeStamp(Object parentAccountTimeStamp) {

		this.parentAccountTimeStamp = parentAccountTimeStamp;
	}

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

}
