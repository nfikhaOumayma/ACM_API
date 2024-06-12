/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

/**
 * {@link TransactionOverride } class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class TransactionOverride {

	/** The cu transaction override ID. */
	public int cuTransactionOverrideID;

	/** The receipt no. */
	public int receiptNo;

	/** The cu account ID. */
	public int cuAccountID;

	/** The override type. */
	public int overrideType;

	/** The user id. */
	public int userId;

	/** The terminal ID. */
	public int terminalID;

	/** The account info. */
	public String accountInfo;

	/** The currency ID. */
	public int currencyID;

	/** The amount. */
	public double amount;

	/** The description. */
	public String description;

	/** The split header ID. */
	public int splitHeaderID;

	/** The message. */
	public String message;

	/** The override type text. */
	public String overrideTypeText;

	/** The checked. */
	public boolean checked;

	/** The authorised. */
	public boolean authorised;

	/**
	 * Gets the cu transaction override ID.
	 *
	 * @return the cuTransactionOverrideID
	 */
	public int getCuTransactionOverrideID() {

		return cuTransactionOverrideID;
	}

	/**
	 * Sets the cu transaction override ID.
	 *
	 * @param cuTransactionOverrideID the cuTransactionOverrideID to set
	 */
	public void setCuTransactionOverrideID(int cuTransactionOverrideID) {

		this.cuTransactionOverrideID = cuTransactionOverrideID;
	}

	/**
	 * Gets the receipt no.
	 *
	 * @return the receiptNo
	 */
	public int getReceiptNo() {

		return receiptNo;
	}

	/**
	 * Sets the receipt no.
	 *
	 * @param receiptNo the receiptNo to set
	 */
	public void setReceiptNo(int receiptNo) {

		this.receiptNo = receiptNo;
	}

	/**
	 * Gets the cu account ID.
	 *
	 * @return the cuAccountID
	 */
	public int getCuAccountID() {

		return cuAccountID;
	}

	/**
	 * Sets the cu account ID.
	 *
	 * @param cuAccountID the cuAccountID to set
	 */
	public void setCuAccountID(int cuAccountID) {

		this.cuAccountID = cuAccountID;
	}

	/**
	 * Gets the override type.
	 *
	 * @return the overrideType
	 */
	public int getOverrideType() {

		return overrideType;
	}

	/**
	 * Sets the override type.
	 *
	 * @param overrideType the overrideType to set
	 */
	public void setOverrideType(int overrideType) {

		this.overrideType = overrideType;
	}

	/**
	 * Gets the user id.
	 *
	 * @return the userId
	 */
	public int getUserId() {

		return userId;
	}

	/**
	 * Sets the user id.
	 *
	 * @param userId the userId to set
	 */
	public void setUserId(int userId) {

		this.userId = userId;
	}

	/**
	 * Gets the terminal ID.
	 *
	 * @return the terminalID
	 */
	public int getTerminalID() {

		return terminalID;
	}

	/**
	 * Sets the terminal ID.
	 *
	 * @param terminalID the terminalID to set
	 */
	public void setTerminalID(int terminalID) {

		this.terminalID = terminalID;
	}

	/**
	 * Gets the account info.
	 *
	 * @return the accountInfo
	 */
	public String getAccountInfo() {

		return accountInfo;
	}

	/**
	 * Sets the account info.
	 *
	 * @param accountInfo the accountInfo to set
	 */
	public void setAccountInfo(String accountInfo) {

		this.accountInfo = accountInfo;
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
	 * Gets the amount.
	 *
	 * @return the amount
	 */
	public double getAmount() {

		return amount;
	}

	/**
	 * Sets the amount.
	 *
	 * @param amount the amount to set
	 */
	public void setAmount(double amount) {

		this.amount = amount;
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
	 * Gets the split header ID.
	 *
	 * @return the splitHeaderID
	 */
	public int getSplitHeaderID() {

		return splitHeaderID;
	}

	/**
	 * Sets the split header ID.
	 *
	 * @param splitHeaderID the splitHeaderID to set
	 */
	public void setSplitHeaderID(int splitHeaderID) {

		this.splitHeaderID = splitHeaderID;
	}

	/**
	 * Gets the message.
	 *
	 * @return the message
	 */
	public String getMessage() {

		return message;
	}

	/**
	 * Sets the message.
	 *
	 * @param message the message to set
	 */
	public void setMessage(String message) {

		this.message = message;
	}

	/**
	 * Gets the override type text.
	 *
	 * @return the overrideTypeText
	 */
	public String getOverrideTypeText() {

		return overrideTypeText;
	}

	/**
	 * Sets the override type text.
	 *
	 * @param overrideTypeText the overrideTypeText to set
	 */
	public void setOverrideTypeText(String overrideTypeText) {

		this.overrideTypeText = overrideTypeText;
	}

	/**
	 * Checks if is checked.
	 *
	 * @return the checked
	 */
	public boolean isChecked() {

		return checked;
	}

	/**
	 * Sets the checked.
	 *
	 * @param checked the checked to set
	 */
	public void setChecked(boolean checked) {

		this.checked = checked;
	}

	/**
	 * Checks if is authorised.
	 *
	 * @return the authorised
	 */
	public boolean isAuthorised() {

		return authorised;
	}

	/**
	 * Sets the authorised.
	 *
	 * @param authorised the authorised to set
	 */
	public void setAuthorised(boolean authorised) {

		this.authorised = authorised;
	}

}
