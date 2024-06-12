/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

/**
 * {@link HabilitationIHMRoute} class.
 *
 * @author HaythemBenizid
 * @since 0.9.0
 */
@Entity
@Table(name = "ACM_HABILITATION_IHM_ROUTE")
public class HabilitationIHMRoute extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1806307629920172849L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_HABILITATION_IHM_ROUTE", unique = true, nullable = false)
	private Long id;

	/** The racine id. */

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "RACINE_ID")
	private AcmModule racineId;

	/** The client. */
	@Column(name = "CLIENT", nullable = false, length = 256)
	private String client;

	/** The code IHM route. */
	@Column(name = "CODE_IHM_ROUTE", nullable = false, length = 512)
	private String codeIHMRoute;

	/** The ihm route. */
	@Column(name = "IHM_ROUTE", nullable = false, length = 512)
	private String ihmRoute;

	/** The description. */
	@Column(name = "DESCRIPTION", nullable = false, length = 512)
	private String description;

	/** Settings Workflow. */
	@Column(name = "SETTINGS_WORKFLOW", nullable = false)
	private boolean settingsWorkflow;

	/** The notifications. */
	@OneToMany(mappedBy = "habilitationIHMRoute")
	private Set<HabilitationIHMButton> habilitationIHMButtons = new HashSet<>();

	/** The habilitation. */
	@OneToMany(fetch = FetchType.EAGER, mappedBy = "habilitationIHMRoute")
	private List<Habilitation> habilitation = new ArrayList<>();

	/** The notifications. */
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "habilitationIHMRoute")
	private Set<AcmIhmForm> forms = new HashSet<>();

	/**
	 * Gets SettingsWorkflowt.
	 *
	 * @return the SettingsWorkflow
	 */

	public boolean getSettingsWorkflow() {

		return settingsWorkflow;
	}

	/**
	 * Sets the SettingsWorkflow.
	 *
	 * @param settingsWorkflow the settingsWorkflow to set
	 */
	public void setSettingsWorkflow(boolean settingsWorkflow) {

		this.settingsWorkflow = settingsWorkflow;
	}

	/**
	 * Instantiates a new habilitation IHM route.
	 */
	public HabilitationIHMRoute() {

		/*
		 * Empty
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
	 * Gets the client.
	 *
	 * @return the client
	 */
	public String getClient() {

		return client;
	}

	/**
	 * Sets the client.
	 *
	 * @param client the client to set
	 */
	public void setClient(String client) {

		this.client = client;
	}

	/**
	 * Gets the code IHM route.
	 *
	 * @return the codeIHMRoute
	 */
	public String getCodeIHMRoute() {

		return codeIHMRoute;
	}

	/**
	 * Sets the code IHM route.
	 *
	 * @param codeIHMRoute the codeIHMRoute to set
	 */
	public void setCodeIHMRoute(String codeIHMRoute) {

		this.codeIHMRoute = codeIHMRoute;
	}

	/**
	 * Gets the ihm route.
	 *
	 * @return the ihmRoute
	 */
	public String getIhmRoute() {

		return ihmRoute;
	}

	/**
	 * Sets the ihm route.
	 *
	 * @param ihmRoute the ihmRoute to set
	 */
	public void setIhmRoute(String ihmRoute) {

		this.ihmRoute = ihmRoute;
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
	 * Gets the racine id.
	 *
	 * @return the racineId
	 */
	public AcmModule getRacineId() {

		return racineId;
	}

	/**
	 * Sets the racine id.
	 *
	 * @param racineId the racineId to set
	 */
	public void setRacineId(AcmModule racineId) {

		this.racineId = racineId;
	}

	/**
	 * Gets the HabilitationIHMButtons.
	 * 
	 * @return the habilitationIHMButtons
	 */
	public Set<HabilitationIHMButton> getHabilitationIHMButtons() {

		return habilitationIHMButtons;
	}

	/**
	 * Sets the HabilitationIHMButtons.
	 * 
	 * @param habilitationIHMButtons the habilitationIHMButtons to set
	 */
	public void setHabilitationIHMButtons(Set<HabilitationIHMButton> habilitationIHMButtons) {

		this.habilitationIHMButtons = habilitationIHMButtons;
	}

	/**
	 * Gets the forms.
	 *
	 * @return the forms
	 */
	public Set<AcmIhmForm> getForms() {

		return forms;
	}

	/**
	 * Sets the forms.
	 *
	 * @param forms the new forms
	 */
	public void setForms(Set<AcmIhmForm> forms) {

		this.forms = forms;
	}

	/**
	 * Gets the habilitation.
	 *
	 * @return the habilitation
	 */
	public List<Habilitation> getHabilitation() {

		return habilitation;
	}

	/**
	 * Sets the habilitation.
	 *
	 * @param habilitation the habilitation to set
	 */
	public void setHabilitation(List<Habilitation> habilitation) {

		this.habilitation = habilitation;
	}

}
