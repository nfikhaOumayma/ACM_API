/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonBackReference;

/**
 * The Class SettingJournalEnteriesDTO.
 */
public class SettingJournalEnteriesDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */

	private Long id;

	/** The code. */

	private String code;

	/** The libelle. */
	private String libelle;

	/** The description. */
	private String description;

	/** The amount. */
	private String amount;

	/** The percentage. */
	private int percentage;

	/** The debit account. */
	private String debitAccount;

	/** The id debit acount. */
	private Long idDebitAcount;

	/** The credit account. */
	private String creditAccount;

	/** The id credit account. */
	private Long idCreditAccount;

	/** The setting journal entry type. */
	@JsonBackReference
	private SettingJournalEntryTypeDTO settingJournalEntryType;

	/** The id type journal entry. */
	private long idTypeJournalEntry;

	/**
	 * Gets the id.
	 *
	 * @return the id
	 */
	public Long getId() {

		return id;
	}

	/**
	 * Sets the id.
	 *
	 * @param id the id to set
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the code.
	 *
	 * @return the code
	 */
	public String getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(String code) {

		this.code = code;
	}

	/**
	 * Gets the libelle.
	 *
	 * @return the libelle
	 */
	public String getLibelle() {

		return libelle;
	}

	/**
	 * Sets the libelle.
	 *
	 * @param libelle the libelle to set
	 */
	public void setLibelle(String libelle) {

		this.libelle = libelle;
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
	 * Gets the percentage.
	 *
	 * @return the percentage
	 */
	public int getPercentage() {

		return percentage;
	}

	/**
	 * Sets the percentage.
	 *
	 * @param percentage the percentage to set
	 */
	public void setPercentage(int percentage) {

		this.percentage = percentage;
	}

	/**
	 * Gets the debit account.
	 *
	 * @return the debitAccount
	 */
	public String getDebitAccount() {

		return debitAccount;
	}

	/**
	 * Sets the debit account.
	 *
	 * @param debitAccount the debitAccount to set
	 */
	public void setDebitAccount(String debitAccount) {

		this.debitAccount = debitAccount;
	}

	/**
	 * Gets the credit account.
	 *
	 * @return the creditAccount
	 */
	public String getCreditAccount() {

		return creditAccount;
	}

	/**
	 * Sets the credit account.
	 *
	 * @param creditAccount the creditAccount to set
	 */
	public void setCreditAccount(String creditAccount) {

		this.creditAccount = creditAccount;
	}

	/**
	 * Gets the setting journal entry type.
	 *
	 * @return the settingJournalEntryType
	 */
	public SettingJournalEntryTypeDTO getSettingJournalEntryType() {

		return settingJournalEntryType;
	}

	/**
	 * Sets the setting journal entry type.
	 *
	 * @param settingJournalEntryType the settingJournalEntryType to set
	 */
	public void setSettingJournalEntryType(SettingJournalEntryTypeDTO settingJournalEntryType) {

		this.settingJournalEntryType = settingJournalEntryType;
	}

	/**
	 * Gets the id type journal entry.
	 *
	 * @return the idTypeJournalEntry
	 */
	public long getIdTypeJournalEntry() {

		return idTypeJournalEntry;
	}

	/**
	 * Sets the id type journal entry.
	 *
	 * @param idTypeJournalEntry the idTypeJournalEntry to set
	 */
	public void setIdTypeJournalEntry(long idTypeJournalEntry) {

		this.idTypeJournalEntry = idTypeJournalEntry;
	}

	/**
	 * Gets the id debit acount.
	 *
	 * @return the idDebitAcount
	 */
	public Long getIdDebitAcount() {

		return idDebitAcount;
	}

	/**
	 * Sets the id debit acount.
	 *
	 * @param idDebitAcount the idDebitAcount to set
	 */
	public void setIdDebitAcount(Long idDebitAcount) {

		this.idDebitAcount = idDebitAcount;
	}

	/**
	 * Gets the id credit account.
	 *
	 * @return the idCreditAccount
	 */
	public Long getIdCreditAccount() {

		return idCreditAccount;
	}

	/**
	 * Sets the id credit account.
	 *
	 * @param idCreditAccount the idCreditAccount to set
	 */
	public void setIdCreditAccount(Long idCreditAccount) {

		this.idCreditAccount = idCreditAccount;
	}

}
