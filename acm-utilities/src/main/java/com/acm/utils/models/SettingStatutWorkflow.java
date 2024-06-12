/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * {@link SettingStatutWorkflow} class.
 *
 * @author HaythemBenizid
 * @since 0.4.0
 */
@Entity
@Table(name = "ACM_SETTING_STATUT_WORKFLOW")
public class SettingStatutWorkflow extends GenericModel implements Serializable {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 3760187546371536078L;

	/** The id. */
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "ID_ACM_SETTING_STATUT_WORKFLOW", unique = true, nullable = false)
	private Long id;

	/** The code. */
	@Column(name = "CODE", nullable = false)
	private Integer code;

	/** The libelle. */
	@Column(name = "LIBELLE")
	private String libelle;

	/** The description. */
	@Column(name = "DESCRIPTION")
	private String description;

	/** The code statut loan. */
	@Column(name = "CODE_STATUT_LOAN", nullable = false)
	private Long codeStatutLoan;

	/** The statut loan. */
	@Column(name = "STATUT_LOAN")
	private String statutLoan;

	/** The statut loan. */
	@Column(name = "IHM_WEB_ROOT")
	private String ihmRoot;

	/** The client. */
	@Column(name = "CLIENT")
	private String client;

	/** The process name. */
	@Column(name = "BPMN_PROCESS_NAME")
	private String processName;

	/** The orderEtapeProcess. */
	@Column(name = "ORDER_ETAPE_PROCESS")
	private Integer orderEtapeProcess;

	/** The showIb. */
	@Column(name = "SHOW_IB")
	private Boolean showIb;

	/** The codeStautIb. */
	@Column(name = "CODE_STATUT_IB")
	private Integer codeStautIb;

	/**
	 * Instantiates a new setting statut workflow.
	 */
	public SettingStatutWorkflow() {

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
	 * Gets the code statut loan.
	 *
	 * @return the codeStatutLoan
	 */
	public Long getCodeStatutLoan() {

		return codeStatutLoan;
	}

	/**
	 * Sets the code statut loan.
	 *
	 * @param codeStatutLoan the codeStatutLoan to set
	 */
	public void setCodeStatutLoan(Long codeStatutLoan) {

		this.codeStatutLoan = codeStatutLoan;
	}

	/**
	 * Gets the statut loan.
	 *
	 * @return the statutLoan
	 */
	public String getStatutLoan() {

		return statutLoan;
	}

	/**
	 * Sets the statut loan.
	 *
	 * @param statutLoan the statutLoan to set
	 */
	public void setStatutLoan(String statutLoan) {

		this.statutLoan = statutLoan;
	}

	/**
	 * Gets the code.
	 *
	 * @return the code
	 */
	public Integer getCode() {

		return code;
	}

	/**
	 * Sets the code.
	 *
	 * @param code the code to set
	 */
	public void setCode(Integer code) {

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
	 * Gets the ihm root.
	 *
	 * @return the ihmRoot
	 */
	public String getIhmRoot() {

		return ihmRoot;
	}

	/**
	 * Sets the ihm root.
	 *
	 * @param ihmRoot the ihmRoot to set
	 */
	public void setIhmRoot(String ihmRoot) {

		this.ihmRoot = ihmRoot;
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
	 * Gets the process name.
	 *
	 * @return the processName
	 */
	public String getProcessName() {

		return processName;
	}

	/**
	 * Sets the process name.
	 *
	 * @param processName the processName to set
	 */
	public void setProcessName(String processName) {

		this.processName = processName;
	}

	/**
	 * Gets the order etape process.
	 *
	 * @return the orderEtapeProcess
	 */
	public Integer getOrderEtapeProcess() {

		return orderEtapeProcess;
	}

	/**
	 * Sets the order etape process.
	 *
	 * @param orderEtapeProcess the orderEtapeProcess to set
	 */
	public void setOrderEtapeProcess(Integer orderEtapeProcess) {

		this.orderEtapeProcess = orderEtapeProcess;
	}

	/**
	 * Gets the show ib.
	 *
	 * @return the showIb
	 */
	public Boolean getShowIb() {

		return showIb;
	}

	/**
	 * Sets the show ib.
	 *
	 * @param showIb the showIb to set
	 */
	public void setShowIb(Boolean showIb) {

		this.showIb = showIb;
	}

	/**
	 * Gets the code staut ib.
	 *
	 * @return the codeStautIb
	 */
	public Integer getCodeStautIb() {

		return codeStautIb;
	}

	/**
	 * Sets the code staut ib.
	 *
	 * @param codeStautIb the codeStautIb to set
	 */
	public void setCodeStautIb(Integer codeStautIb) {

		this.codeStautIb = codeStautIb;
	}

}
