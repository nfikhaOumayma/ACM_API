/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.api_charge_off.dtos;

import com.acm.utils.dtos.GenericDTO;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Class AccountDTO.
 */
public class AccountDTO extends GenericDTO {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1609132821653987055L;

	/** The name. */
	@JsonProperty("name")
	public String name;

	/** The open balance. */
	@JsonProperty("openBalance")
	public double openBalance;

	/** The debit amount. */
	@JsonProperty("debitAmount")
	public String debitAmount;

	/** The credit amount. */
	@JsonProperty("creditAmount")
	public String creditAmount;

	/** The amount paid. */
	@JsonProperty("amountPaid")
	public double amountPaid;

	/** The closing balance. */
	@JsonProperty("closingBalance")
	public double closingBalance;

	/** The can edit amount. */
	@JsonProperty("canEditAmount")
	public String canEditAmount;

	/** The id. */
	@JsonProperty("id")
	public String id;

	/**
	 * Gets the name.
	 *
	 * @return the name
	 */
	public String getName() {

		return name;
	}

	/**
	 * Sets the name.
	 *
	 * @param name the new name
	 */
	public void setName(String name) {

		this.name = name;
	}

	/**
	 * Gets the open balance.
	 *
	 * @return the open balance
	 */
	public double getOpenBalance() {

		return openBalance;
	}

	/**
	 * Sets the open balance.
	 *
	 * @param openBalance the new open balance
	 */
	public void setOpenBalance(double openBalance) {

		this.openBalance = openBalance;
	}

	/**
	 * Gets the debit amount.
	 *
	 * @return the debit amount
	 */
	public String getDebitAmount() {

		return debitAmount;
	}

	/**
	 * Sets the debit amount.
	 *
	 * @param debitAmount the new debit amount
	 */
	public void setDebitAmount(String debitAmount) {

		this.debitAmount = debitAmount;
	}

	/**
	 * Gets the credit amount.
	 *
	 * @return the credit amount
	 */
	public String getCreditAmount() {

		return creditAmount;
	}

	/**
	 * Sets the credit amount.
	 *
	 * @param creditAmount the new credit amount
	 */
	public void setCreditAmount(String creditAmount) {

		this.creditAmount = creditAmount;
	}

	/**
	 * Gets the amount paid.
	 *
	 * @return the amount paid
	 */
	public double getAmountPaid() {

		return amountPaid;
	}

	/**
	 * Sets the amount paid.
	 *
	 * @param amountPaid the new amount paid
	 */
	public void setAmountPaid(double amountPaid) {

		this.amountPaid = amountPaid;
	}

	/**
	 * Gets the closing balance.
	 *
	 * @return the closing balance
	 */
	public double getClosingBalance() {

		return closingBalance;
	}

	/**
	 * Sets the closing balance.
	 *
	 * @param closingBalance the new closing balance
	 */
	public void setClosingBalance(double closingBalance) {

		this.closingBalance = closingBalance;
	}

	/**
	 * Gets the can edit amount.
	 *
	 * @return the can edit amount
	 */
	public String getCanEditAmount() {

		return canEditAmount;
	}

	/**
	 * Sets the can edit amount.
	 *
	 * @param canEditAmount the new can edit amount
	 */
	public void setCanEditAmount(String canEditAmount) {

		this.canEditAmount = canEditAmount;
	}

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public String getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the new id
	 */
	public void setId(String id) {

		this.id = id;
	}

}
