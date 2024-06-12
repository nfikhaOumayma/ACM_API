/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.List;

/**
 * The Class LoanSchedulesApiDTO.
 */
public class LoanSchedulesApiDTO {

	/** The account number. */
	private String accountNumber;

	/** The product. */
	private String product;

	/** The date. */
	private String date;

	/** The balance. */
	private String balance;

	/** The active. */
	private Boolean active;

	/** The loan schedule. */
	private List<ScheduleApiDTO> loanSchedule;

	/** The account branch. */
	private String accountBranch;

	/** The cu account ID. */
	private Long cuAccountID;

	/** The currency ID. */
	private Long currencyID;

	/**
	 * Instantiates a new loan schedules api DTO.
	 */
	public LoanSchedulesApiDTO() {

	}

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
	 * Gets the active.
	 *
	 * @return the active
	 */
	public Boolean getActive() {

		return active;
	}

	/**
	 * Sets the active.
	 *
	 * @param active the new active
	 */
	public void setActive(Boolean active) {

		this.active = active;
	}

	/**
	 * Gets the loan schedule.
	 *
	 * @return the loan schedule
	 */
	public List<ScheduleApiDTO> getLoanSchedule() {

		return loanSchedule;
	}

	/**
	 * Sets the loan schedule.
	 *
	 * @param loanSchedule the new loan schedule
	 */
	public void setLoanSchedule(List<ScheduleApiDTO> loanSchedule) {

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
	public Long getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the new cu account ID
	 */
	public void setCuAccountID(Long cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the currency ID.
	 *
	 * @return the currency ID
	 */
	public Long getCurrencyID() {

		return currencyID;
	}

	/**
	 * Sets the currency ID.
	 *
	 * @param currencyID the new currency ID
	 */
	public void setCurrencyID(Long currencyID) {

		this.currencyID = currencyID;
	}

}
