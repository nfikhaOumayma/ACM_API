/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 * {@link Habilitation} class.
 *
 * @author HaythemBenizid
 * @since 0.3.0
 */
@Entity
@Table(name = "ACM_HABILITATION")
@NamedQuery(name = "Habilitation.findAll", query = "SELECT h FROM Habilitation h")
public class Habilitation extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 4623971226312815316L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_HABILITATION", unique = true, nullable = false)
	private Long id;

	/** The actions. */
	@Column(name = "ACTIONS", nullable = false, length = 512)
	private String actions;

	/** The client. */
	@Column(name = "CLIENT", nullable = false, length = 256)
	private String client;

	/** The habilitation. */
	@Column(name = "ACM_HABILITATION", nullable = false, length = 512)
	private String acmHabilitation;

	/** The web route. */
	@Column(name = "ACM_WEB_ROUTE", nullable = false, length = 512)
	private String acmWebRoute;

	/** The id groupe. */
	@Column(name = "GROUPE_ID", nullable = false)
	private Long idGroupe;

	/** The value. */
	@Column(name = "VALUE", length = 256)
	private String value;

	/** The actions. */
	@Column(name = "DESCRIPTION", nullable = false, length = 512)
	private String description;

	/** The habilitation IHM route. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_HABILITATION_IHM_ROUTE")
	private HabilitationIHMRoute habilitationIHMRoute;

	/**
	 * Instantiates a new habilitation.
	 */
	public Habilitation() {

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
	 * Gets the habilitation IHM route.
	 *
	 * @return the habilitationIHMRoute
	 */
	public HabilitationIHMRoute getHabilitationIHMRoute() {

		return habilitationIHMRoute;
	}

	/**
	 * Sets the habilitation IHM route.
	 *
	 * @param habilitationIHMRoute the habilitationIHMRoute to set
	 */
	public void setHabilitationIHMRoute(HabilitationIHMRoute habilitationIHMRoute) {

		this.habilitationIHMRoute = habilitationIHMRoute;
	}

}
