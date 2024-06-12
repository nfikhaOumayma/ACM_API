/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonManagedReference;

/**
 * {@link SettingJournalEntryTypeDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */

public class SettingJournalEntryTypeDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -5586197244024675290L;

	/** The id. */
	private Long id;

	/** The code. */
	private String code;

	/** The description. */
	private String description;

	/** The enabled. */
	private Boolean enabled;

	/** The journal id. */
	private Long journalId;

	/** The journal description. */
	private String journalDescription;

	/** The setting journal enteries. */
	@JsonManagedReference
	private List<SettingJournalEnteriesDTO> settingJournalEnteries =
			new ArrayList<SettingJournalEnteriesDTO>();

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
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public Boolean getEnabled() {

		return enabled;
	}

	/**
	 * Sets the enabled.
	 *
	 * @param enabled the enabled to set
	 */
	public void setEnabled(Boolean enabled) {

		this.enabled = enabled;
	}

	/**
	 * Gets the setting journal enteries.
	 *
	 * @return the settingJournalEnteries
	 */
	public List<SettingJournalEnteriesDTO> getSettingJournalEnteries() {

		return settingJournalEnteries;
	}

	/**
	 * Sets the setting journal enteries.
	 *
	 * @param settingJournalEnteries the settingJournalEnteries to set
	 */
	public void setSettingJournalEnteries(List<SettingJournalEnteriesDTO> settingJournalEnteries) {

		this.settingJournalEnteries = settingJournalEnteries;
	}

	/**
	 * Gets the journal id.
	 *
	 * @return the journal id
	 */
	public Long getJournalId() {

		return journalId;
	}

	/**
	 * Sets the journal id.
	 *
	 * @param journalId the new journal id
	 */
	public void setJournalId(Long journalId) {

		this.journalId = journalId;
	}

	/**
	 * Gets the journal description.
	 *
	 * @return the journal description
	 */
	public String getJournalDescription() {

		return journalDescription;
	}

	/**
	 * Sets the journal description.
	 *
	 * @param journalDescription the new journal description
	 */
	public void setJournalDescription(String journalDescription) {

		this.journalDescription = journalDescription;
	}

}
