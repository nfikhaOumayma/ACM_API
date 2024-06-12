/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.ArrayList;
import java.util.List;

/**
 * {@link DisburseDto } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class DisburseDTO {

	/** The customer ID. */
	public Long customerID;

	/** The account loan trn. */
	public List<AccountLoanTrn> accountLoanTrn;

	/** The transaction overrides. */
	public ArrayList<TransactionOverride> transactionOverrides;

	/**
	 * Gets the customer ID.
	 *
	 * @return the customerID
	 */
	public Long getCustomerID() {

		return customerID;
	}

	/**
	 * Sets the customer ID.
	 *
	 * @param customerID the customerID to set
	 */
	public void setCustomerID(Long customerID) {

		this.customerID = customerID;
	}

	/**
	 * Gets the account loan trn.
	 *
	 * @return the accountLoanTrn
	 */
	public List<AccountLoanTrn> getAccountLoanTrn() {

		return accountLoanTrn;
	}

	/**
	 * Sets the account loan trn.
	 *
	 * @param accountLoanTrns the accountLoanTrn to set
	 */
	public void setAccountLoanTrn(List<AccountLoanTrn> accountLoanTrns) {

		this.accountLoanTrn = accountLoanTrns;
	}

	/**
	 * Gets the transaction overrides.
	 *
	 * @return the transactionOverrides
	 */
	public ArrayList<TransactionOverride> getTransactionOverrides() {

		return transactionOverrides;
	}

	/**
	 * Sets the transaction overrides.
	 *
	 * @param transactionOverrides the transactionOverrides to set
	 */
	public void setTransactionOverrides(ArrayList<TransactionOverride> transactionOverrides) {

		this.transactionOverrides = transactionOverrides;
	}

}
