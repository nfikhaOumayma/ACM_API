/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.dtos;

import java.io.Serializable;

import org.dozer.Mapping;

/**
 * {@link ItemInstanceDTO} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */

public class ItemInstanceDTO implements Serializable {

	/** The id. */
	private Long id;

	/** The item DTO. */
	@Mapping("item.id")
	private Long idItem;

	/** The code. */

	private Long idWorkFlowStep;

	/** The libelle. */
	private String libelle;

	/** The description. */
	private String description;

	/** The code statut loan. */
	private Long codeStatutItem;

	/** The statut loan. */
	private String statutItem;

	/** The statut loan. */
	private String ihmRoot;

	/** The orderEtapeProcess. */
	private Long orderEtapeProcess;

	/** The action user. */
	private String actionUser;

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
	 * @param id the new id
	 */
	public void setId(Long id) {

		this.id = id;
	}

	/**
	 * Gets the id work flow step.
	 *
	 * @return the id work flow step
	 */
	public Long getIdWorkFlowStep() {

		return idWorkFlowStep;
	}

	/**
	 * Sets the id work flow step.
	 *
	 * @param idWorkFlowStep the new id work flow step
	 */
	public void setIdWorkFlowStep(Long idWorkFlowStep) {

		this.idWorkFlowStep = idWorkFlowStep;
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
	 * @param libelle the new libelle
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
	 * @param description the new description
	 */
	public void setDescription(String description) {

		this.description = description;
	}

	/**
	 * Gets the code statut item.
	 *
	 * @return the code statut item
	 */
	public Long getCodeStatutItem() {

		return codeStatutItem;
	}

	/**
	 * Sets the code statut item.
	 *
	 * @param codeStatutItem the new code statut item
	 */
	public void setCodeStatutItem(Long codeStatutItem) {

		this.codeStatutItem = codeStatutItem;
	}

	/**
	 * Gets the statut item.
	 *
	 * @return the statut item
	 */
	public String getStatutItem() {

		return statutItem;
	}

	/**
	 * Sets the statut item.
	 *
	 * @param statutItem the new statut item
	 */
	public void setStatutItem(String statutItem) {

		this.statutItem = statutItem;
	}

	/**
	 * Gets the ihm root.
	 *
	 * @return the ihm root
	 */
	public String getIhmRoot() {

		return ihmRoot;
	}

	/**
	 * Sets the ihm root.
	 *
	 * @param ihmRoot the new ihm root
	 */
	public void setIhmRoot(String ihmRoot) {

		this.ihmRoot = ihmRoot;
	}

	/**
	 * Gets the order etape process.
	 *
	 * @return the order etape process
	 */
	public Long getOrderEtapeProcess() {

		return orderEtapeProcess;
	}

	/**
	 * Sets the order etape process.
	 *
	 * @param orderEtapeProcess the new order etape process
	 */
	public void setOrderEtapeProcess(Long orderEtapeProcess) {

		this.orderEtapeProcess = orderEtapeProcess;
	}

	/**
	 * Gets the id item.
	 *
	 * @return the id item
	 */
	public Long getIdItem() {

		return idItem;
	}

	/**
	 * Sets the id item.
	 *
	 * @param idItem the new id item
	 */
	public void setIdItem(Long idItem) {

		this.idItem = idItem;
	}

	/**
	 * Gets the action user.
	 *
	 * @return the action user
	 */
	public String getActionUser() {

		return actionUser;
	}

	/**
	 * Sets the action user.
	 *
	 * @param actionUser the new action user
	 */
	public void setActionUser(String actionUser) {

		this.actionUser = actionUser;
	}

}
