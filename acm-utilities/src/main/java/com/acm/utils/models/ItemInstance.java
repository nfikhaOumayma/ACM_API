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
 * {@link ItemInstance} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
@Entity
@Table(name = "ACM_ITEM_INSTANCE")
public class ItemInstance extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -4100570609999190768L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_ITEM_INSTANCE", unique = true, nullable = false)
	private Long id;

	/** The item. */
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "ID_ACM_ITEM")
	private Item item;

	/** The code. */
	@Column(name = "ID_ACM_WORKFLOW_STEP", nullable = false)
	private Long idWorkFlowStep;

	/** The libelle. */
	@Column(name = "LIBELLE")
	private String libelle;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The code statut loan. */
	@Column(name = "CODE_STATUT_ITEM", nullable = false)
	private Long codeStatutItem;

	/** The statut loan. */
	@Column(name = "STATUT_ITEM")
	private String statutItem;

	/** The statut loan. */
	@Column(name = "IHM_WEB_ROOT")
	private String ihmRoot;

	/** The orderEtapeProcess. */
	@Column(name = "ORDER_ETAPE_PROCESS")
	private Long orderEtapeProcess;

	/** The action user. */
	@Column(name = "ACTION_USER")
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
	 * Gets the item.
	 *
	 * @return the item
	 */
	public Item getItem() {

		return item;
	}

	/**
	 * Sets the item.
	 *
	 * @param item the new item
	 */
	public void setItem(Item item) {

		this.item = item;
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
