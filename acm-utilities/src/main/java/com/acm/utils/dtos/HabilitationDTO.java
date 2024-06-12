/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link HabilitationDTO} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
public class HabilitationDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4138315972194744215L;

	/** The id. */
	private Long id;

	/** The actions. */
	private String actions;

	/** The client. */
	private String client;

	/** The habilitation. */
	private String acmHabilitation;

	/** The web route. */
	private String acmWebRoute;

	/** The id groupe. */
	private Long idGroupe;

	/** The value. */
	private String value;

	/** The enabled. */
	private Boolean enabled;

	/** The description. */
	private String description;

	/** The habilitation IHM route DTO. */
	private HabilitationIHMRouteDTO habilitationIHMRouteDTO;

	/**
	 * Instantiates a new habilitation DTO.
	 */
	public HabilitationDTO() {

	}

	/**
	 * Instantiates a new habilitation DTO.
	 * 
	 * @param actions the actions
	 * @param client the client
	 * @param acmHabilitation the acmHabilitation
	 * @param acmWebRoute the acmWebRoute
	 * @param idGroupe the idGroupe
	 * @param value the value
	 * @param enabled the enabled
	 */
	public HabilitationDTO(String actions, String client, String acmHabilitation,
			String acmWebRoute, Long idGroupe, String value, Boolean enabled) {

		this.actions = actions;
		this.client = client;
		this.acmHabilitation = acmHabilitation;
		this.acmWebRoute = acmWebRoute;
		this.idGroupe = idGroupe;
		this.value = value;
		this.enabled = enabled;
	}

	/**
	 * Instantiates a new habilitation DTO.
	 *
	 * @param actions the actions
	 * @param client the client
	 * @param acmHabilitation the acm habilitation
	 * @param acmWebRoute the acm web route
	 * @param idGroupe the id groupe
	 * @param value the value
	 * @param enabled the enabled
	 * @param description the description
	 */
	public HabilitationDTO(String actions, String client, String acmHabilitation,
			String acmWebRoute, Long idGroupe, String value, Boolean enabled, String description) {

		this.actions = actions;
		this.client = client;
		this.acmHabilitation = acmHabilitation;
		this.acmWebRoute = acmWebRoute;
		this.idGroupe = idGroupe;
		this.value = value;
		this.enabled = enabled;
		this.description = description;
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
	 * Gets the actions.
	 *
	 * @return the actions
	 */
	public String getActions() {

		return actions;
	}

	/**
	 * Sets the actions.
	 *
	 * @param actions the actions to set
	 */
	public void setActions(String actions) {

		this.actions = actions;
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
	 * Gets the acm habilitation.
	 *
	 * @return the acmHabilitation
	 */
	public String getAcmHabilitation() {

		return acmHabilitation;
	}

	/**
	 * Sets the acm habilitation.
	 *
	 * @param acmHabilitation the acmHabilitation to set
	 */
	public void setAcmHabilitation(String acmHabilitation) {

		this.acmHabilitation = acmHabilitation;
	}

	/**
	 * Gets the acm web route.
	 *
	 * @return the acmWebRoute
	 */
	public String getAcmWebRoute() {

		return acmWebRoute;
	}

	/**
	 * Sets the acm web route.
	 *
	 * @param acmWebRoute the acmWebRoute to set
	 */
	public void setAcmWebRoute(String acmWebRoute) {

		this.acmWebRoute = acmWebRoute;
	}

	/**
	 * Gets the id groupe.
	 *
	 * @return the idGroupe
	 */
	public Long getIdGroupe() {

		return idGroupe;
	}

	/**
	 * Sets the id groupe.
	 *
	 * @param idGroupe the idGroupe to set
	 */
	public void setIdGroupe(Long idGroupe) {

		this.idGroupe = idGroupe;
	}

	/**
	 * Gets the value.
	 *
	 * @return the value
	 */
	public String getValue() {

		return value;
	}

	/**
	 * Sets the value.
	 *
	 * @param value the value to set
	 */
	public void setValue(String value) {

		this.value = value;
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
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the habilitation IHM route DTO.
	 *
	 * @return the habilitationIHMRouteDTO
	 */
	public HabilitationIHMRouteDTO getHabilitationIHMRouteDTO() {

		return habilitationIHMRouteDTO;
	}

	/**
	 * Sets the habilitation IHM route DTO.
	 *
	 * @param habilitationIHMRouteDTO the habilitationIHMRouteDTO to set
	 */
	public void setHabilitationIHMRouteDTO(HabilitationIHMRouteDTO habilitationIHMRouteDTO) {

		this.habilitationIHMRouteDTO = habilitationIHMRouteDTO;
	}

}
