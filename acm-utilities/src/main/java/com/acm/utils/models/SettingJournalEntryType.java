/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link SettingJournalEntryType} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@Entity
@Table(name = "ACM_SETTING_JOURNAL_ENTRY_TYPE")
public class SettingJournalEntryType extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -2891933251983342207L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_JOURNAL_ENTRY", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE", nullable = false)
	private String code;

	/** The libelle. */
	@Column(name = "LIBELLE")
	private String libelle;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The journal id. */
	@Column(name = "JOURNAL_ID")
	private String journalId;

	/** The journal description. */
	@Column(name = "JOURNAL_DESCRIPTION")
	private String journalDescription;

	/** The setting journal enteries. */
	@OneToMany(mappedBy = "settingJournalEntryType", fetch = FetchType.LAZY)
	private List<SettingJournalEnteries> settingJournalEnteries =
			new ArrayList<SettingJournalEnteries>();

	/** The Work flow steps. */
	@ManyToMany(mappedBy = "journalEntryTypes", fetch = FetchType.EAGER)
	private List<WorkFlowStep> WorkFlowSteps;

	/**
	 * Instantiates a new setting motif rejets.
	 */
	public SettingJournalEntryType() {

		/*
		 * EMPTY
		 */
	}

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
	 * Gets the setting journal enteries.
	 *
	 * @return the settingJournalEnteries
	 */
	public List<SettingJournalEnteries> getSettingJournalEnteries() {

		return settingJournalEnteries;
	}

	/**
	 * Sets the setting journal enteries.
	 *
	 * @param settingJournalEnteries the settingJournalEnteries to set
	 */
	public void setSettingJournalEnteries(List<SettingJournalEnteries> settingJournalEnteries) {

		this.settingJournalEnteries = settingJournalEnteries;
	}

	/**
	 * Gets the work flow steps.
	 *
	 * @return the workFlowSteps
	 */
	public List<WorkFlowStep> getWorkFlowSteps() {

		return WorkFlowSteps;
	}

	/**
	 * Sets the work flow steps.
	 *
	 * @param workFlowSteps the workFlowSteps to set
	 */
	public void setWorkFlowSteps(List<WorkFlowStep> workFlowSteps) {

		WorkFlowSteps = workFlowSteps;
	}

	/**
	 * Gets the journal id.
	 *
	 * @return the journal id
	 */
	public String getJournalId() {

		return journalId;
	}

	/**
	 * Sets the journal id.
	 *
	 * @param journalId the new journal id
	 */
	public void setJournalId(String journalId) {

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
