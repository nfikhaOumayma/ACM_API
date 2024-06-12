/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

/**
 * {@link HabilitationIHMButtonDTO} class.
 *
 * @author MoezMhiri
 * @since 0.13.0
 */
public class HabilitationIHMButtonDTO extends GenericDTO implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 2701114422705901285L;

	/** The id users notification. */
	private Long idHabilitationIhmButton;

	/** The client. */
	private String client;

	/** The code ihm button. */
	private String codeIhmButton;

	/** The ihm button. */
	private String ihmButton;

	/** The description. */
	private String description;

	/**
	 * Instantiates a new notifications.
	 */
	public HabilitationIHMButtonDTO() {

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

}
