/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;

/**
 * {@link CustomerAbacusAPIModelCustomerLoanLimit} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class CustomerAbacusAPIModelCustomerLoanLimit implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4687960373572004200L;

	/** The customer loan limit ID. */
	private int customerLoanLimitID;

	/** The customer ID. */
	private int customerID;

	/** The currency ID. */
	private int currencyID;

	/** The maximum amount. */
	private double maximumAmount;

	/**
	 * Gets the customer loan limit ID.
	 *
	 * @return the customerLoanLimitID
	 */
	public int getCustomerLoanLimitID() {

		return customerLoanLimitID;
	}

	/**
	 * Sets the customer loan limit ID.
	 *
	 * @param customerLoanLimitID the customerLoanLimitID to set
	 */
	public void setCustomerLoanLimitID(int customerLoanLimitID) {

		this.customerLoanLimitID = customerLoanLimitID;
	}

	/**
	 * Gets the customer ID.
	 *
	 * @return the customerID
	 */
	public int getCustomerID() {

		return customerID;
	}

	/**
	 * Sets the customer ID.
	 *
	 * @param customerID the customerID to set
	 */
	public void setCustomerID(int customerID) {

		this.customerID = customerID;
	}

	/**
	 * Gets the currency ID.
	 *
	 * @return the currencyID
	 */
	public int getCurrencyID() {

		return currencyID;
	}

	/**
	 * Sets the currency ID.
	 *
	 * @param currencyID the currencyID to set
	 */
	public void setCurrencyID(int currencyID) {

		this.currencyID = currencyID;
	}

	/**
	 * Gets the maximum amount.
	 *
	 * @return the maximumAmount
	 */
	public double getMaximumAmount() {

		return maximumAmount;
	}

	/**
	 * Sets the maximum amount.
	 *
	 * @param maximumAmount the maximumAmount to set
	 */
	public void setMaximumAmount(double maximumAmount) {

		this.maximumAmount = maximumAmount;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModelCustomerLoanLimit [customerLoanLimitID=" + customerLoanLimitID
				+ ", customerID=" + customerID + ", currencyID=" + currencyID + ", maximumAmount="
				+ maximumAmount + "]";
	}

}
