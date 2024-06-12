/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_abacus.model;

import java.io.Serializable;
import java.util.List;

/**
 * {@link CustomerAbacusAPIModelPaymentAllocation} class.
 *
 * @author MoezMhiri
 * @since 1.11.5
 */
public class CustomerAbacusAPIModelPaymentAllocation implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3142143060332324372L;

	/** The account number. */
	public String accountNumber;

	/** The product. */
	public String product;

	/** The date. */
	public String date;

	/** The balance. */
	public String balance;

	/** The active. */
	public boolean active;

	/** The loan schedule. */
	public List<CustomerAbacusAPIModelLoanSchedule> loanSchedule;

	/** The account branch. */
	public String accountBranch;

	/** The cu account ID. */
	public int cuAccountID;

	/** The currency ID. */
	public int currencyID;

	/**
	 * Gets the account number.
	 *
	 * @return the account number
	 */
	public String getAccountNumber() {

		return accountNumber;
	}

	/**
	 * Sets the account number.
	 *
	 * @param accountNumber the new account number
	 */
	public void setAccountNumber(String accountNumber) {

		this.accountNumber = accountNumber;
	}

	/**
	 * Gets the product.
	 *
	 * @return the product
	 */
	public String getProduct() {

		return product;
	}

	/**
	 * Sets the product.
	 *
	 * @param product the new product
	 */
	public void setProduct(String product) {

		this.product = product;
	}

	/**
	 * Gets the date.
	 *
	 * @return the date
	 */
	public String getDate() {

		return date;
	}

	/**
	 * Sets the date.
	 *
	 * @param date the new date
	 */
	public void setDate(String date) {

		this.date = date;
	}

	/**
	 * Gets the balance.
	 *
	 * @return the balance
	 */
	public String getBalance() {

		return balance;
	}

	/**
	 * Sets the balance.
	 *
	 * @param balance the new balance
	 */
	public void setBalance(String balance) {

		this.balance = balance;
	}

	/**
	 * Checks if is active.
	 *
	 * @return true, if is active
	 */
	public boolean isActive() {

		return active;
	}

	/**
	 * Sets the active.
	 *
	 * @param active the new active
	 */
	public void setActive(boolean active) {

		this.active = active;
	}

	/**
	 * Gets the loan schedule.
	 *
	 * @return the loan schedule
	 */
	public List<CustomerAbacusAPIModelLoanSchedule> getLoanSchedule() {

		return loanSchedule;
	}

	/**
	 * Sets the loan schedule.
	 *
	 * @param loanSchedule the new loan schedule
	 */
	public void setLoanSchedule(List<CustomerAbacusAPIModelLoanSchedule> loanSchedule) {

		this.loanSchedule = loanSchedule;
	}

	/**
	 * Gets the account branch.
	 *
	 * @return the account branch
	 */
	public String getAccountBranch() {

		return accountBranch;
	}

	/**
	 * Sets the account branch.
	 *
	 * @param accountBranch the new account branch
	 */
	public void setAccountBranch(String accountBranch) {

		this.accountBranch = accountBranch;
	}

	/**
	 * Gets the cu account ID.
	 *
	 * @return the cu account ID
	 */
	public int getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the new cu account ID
	 */
	public void setCuAccountID(int cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the currency ID.
	 *
	 * @return the currency ID
	 */
	public int getCurrencyID() {

		return currencyID;
	}

	/**
	 * Sets the currency ID.
	 *
	 * @param currencyID the new currency ID
	 */
	public void setCurrencyID(int currencyID) {

		this.currencyID = currencyID;
	}

	/**
	 * To string.
	 *
	 * @return the string
	 */
	@Override
	public String toString() {

		return "CustomerAbacusAPIModelPaymentAllocation [accountNumber=" + accountNumber
				+ ", product=" + product + ", date=" + date + ", balance=" + balance + ", active="
				+ active + ", loanSchedule=" + loanSchedule + ", accountBranch=" + accountBranch
				+ ", cuAccountID=" + cuAccountID + ", currencyID=" + currencyID + "]";
	}

}
