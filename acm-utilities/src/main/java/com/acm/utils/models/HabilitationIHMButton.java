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
import javax.persistence.Table;

/**
 * {@link HabilitationIHMButtonDTO} class.
 *
 * @author MoezMhiri
 * @since 0.13.0
 */
@Entity
@Table(name = "ACM_HABILITATION_IHM_BUTTON")
public class HabilitationIHMButton extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -6138144337448995012L;

	/** The id users notification. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_HABILITATION_IHM_BUTTON", unique = true, nullable = false)
	private Long idHabilitationIhmButton;

	/** The client. */
	@Column(name = "CLIENT", nullable = false, length = 256)
	private String client;

	/** The code ihm button. */
	@Column(name = "CODE_IHM_BUTTON", nullable = false, length = 512)
	private String codeIhmButton;

	/** The ihm button. */
	@Column(name = "IHM_BUTTON", nullable = false, length = 512)
	private String ihmButton;

	/** The description. */
	@Column(name = "DESCRIPTION", nullable = false, length = 512)
	private String description;

	/** The habilitation ihm route. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_HABILITATION_IHM_ROUTE")
	private HabilitationIHMRoute habilitationIHMRoute;

	/**
	 * Instantiates a new notifications.
	 */
	public HabilitationIHMButton() {

		/*
		 * EMPTY
		 */
	}

	/**
	 * Gets the id habilitation ihm button.
	 * 
	 * @return the idHabilitationIhmButton
	 */
	public Long getIdHabilitationIhmButton() {

		return idHabilitationIhmButton;
	}

	/**
	 * Sets the id habilitation ihm button.
	 * 
	 * @param idHabilitationIhmButton the idHabilitationIhmButton to set
	 */
	public void setIdHabilitationIhmButton(Long idHabilitationIhmButton) {

		this.idHabilitationIhmButton = idHabilitationIhmButton;
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
	 * Gets the code ihm button.
	 * 
	 * @return the codeIhmButton
	 */
	public String getCodeIhmButton() {

		return codeIhmButton;
	}

	/**
	 * Sets the code ihm button.
	 * 
	 * @param codeIhmButton the codeIhmButton to set
	 */
	public void setCodeIhmButton(String codeIhmButton) {

		this.codeIhmButton = codeIhmButton;
	}

	/**
	 * Gets the ihm button.
	 * 
	 * @return the ihmButton
	 */
	public String getIhmButton() {

		return ihmButton;
	}

	/**
	 * Sets the ihm button.
	 * 
	 * @param ihmButton the ihmButton to set
	 */
	public void setIhmButton(String ihmButton) {

		this.ihmButton = ihmButton;
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
	 * Gets the habilitation ihm route.
	 * 
	 * @return the habilitationIHMRoute
	 */
	public HabilitationIHMRoute getHabilitationIHMRoute() {

		return habilitationIHMRoute;
	}

	/**
	 * Sets the habilitation ihm route.
	 * 
	 * @param habilitationIHMRoute the habilitationIHMRoute to set
	 */
	public void setHabilitationIHMRoute(HabilitationIHMRoute habilitationIHMRoute) {

		this.habilitationIHMRoute = habilitationIHMRoute;
	}

}
