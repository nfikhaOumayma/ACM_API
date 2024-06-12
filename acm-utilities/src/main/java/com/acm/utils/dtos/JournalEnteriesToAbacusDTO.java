/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.util.ArrayList;
import java.util.List;

/**
 * {@link JournalEnteriesToAbacusDTO} class.
 *
 * @author kouali
 * @since 0.1.0
 */
public class JournalEnteriesToAbacusDTO {

	/** The journal ID. */
	private long journalID;

	/** The journal entry. */
	private List<JournalEnteriesInformationDTO> journalEntry = new ArrayList<>();

	/** The page description. */
	private String pageDescription;

	/** The branch ID. */
	private long branchID;

	/** The currency ID. */
	private int currencyID;

	/**
	 * Gets the journal ID.
	 *
	 * @return the journalID
	 */
	public long getJournalID() {

		return journalID;
	}

	/**
	 * Sets the journal ID.
	 *
	 * @param journalID the journalID to set
	 */
	public void setJournalID(long journalID) {

		this.journalID = journalID;
	}

	/**
	 * Gets the journal entry.
	 *
	 * @return the journalEntry
	 */
	public List<JournalEnteriesInformationDTO> getJournalEntry() {

		return journalEntry;
	}

	/**
	 * Adds the journal entry.
	 *
	 * @param journalEnteriesInformationDTO the journal enteries information DTO
	 */
	public void addJournalEntry(JournalEnteriesInformationDTO journalEnteriesInformationDTO) {

		journalEntry.add(journalEnteriesInformationDTO);
	}

	/**
	 * Sets the journal entry.
	 *
	 * @param journalEntry the journalEntry to set
	 */
	public void setJournalEntry(List<JournalEnteriesInformationDTO> journalEntry) {

		this.journalEntry = journalEntry;
	}

	/**
	 * Gets the page description.
	 *
	 * @return the pageDescription
	 */
	public String getPageDescription() {

		return pageDescription;
	}

	/**
	 * Sets the page description.
	 *
	 * @param pageDescription the pageDescription to set
	 */
	public void setPageDescription(String pageDescription) {

		this.pageDescription = pageDescription;
	}

	/**
	 * Gets the branch ID.
	 *
	 * @return the branch ID
	 */
	public long getBranchID() {

		return branchID;
	}

	/**
	 * Sets the branch ID.
	 *
	 * @param branchID the new branch ID
	 */
	public void setBranchID(long branchID) {

		this.branchID = branchID;
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

}
